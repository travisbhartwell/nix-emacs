;;; nix-sandbox.el --- Utility functions to work with nix-shell sandboxes

;; Copyright (C) 2015 Sven Keidel

;; Author: Sven Keidel <svenkeidel@gmail.com>
;; Package-Version: 0.1
;; Package-Requires: ((dash "2.12.1") (s "1.10.0"))
;; Homepage: https://github.com/travisbhartwell/nix-emacs

;; This file is not part of GNU Emacs.

;;; License: GPLv3

;;; Commentary:

;; Useful functions for working with nix-shell sandboxes

;;; Code:

(require 'dash)
(require 's)

(defgroup nix nil
  "customizations for nix"
  :prefix "nix-"
  :group nil)

(defcustom nix-nixpkgs-path nil
  "Absolute path to a nixpkgs directory.

Can be customized to select a nix-channel
e.g. /home/user/.nix-defexpr/channels/unstable/nixpkgs"
  :group 'nix
  :type '(choice (const :tag "No channel" nil)
                 (directory "Custom path to a nixpkgs distribution")))

(defun nix-create-sandbox-rc (sandbox)
  "Creates a new rc file that contains the environment for the sandbox."
  (let* ((env-str (shell-command-to-string
                   (concat "nix-shell --run 'printenv -0' "
                           sandbox
                           " 2> /dev/null")))
         (env (->> env-str
                   (s-split "\0")
                   (-remove (lambda (var) (s-starts-with? "shellHook" var)))
                   (-map (lambda (evar)
                           (pcase (s-split-up-to "=" evar 1)
                             (`(,var ,val)
                              (concat "export " var "=" (shell-quote-argument val))))))))
         (tmp-file (make-temp-file "nix-sandbox-rc-")))
    (write-region (s-join "\n" env) nil tmp-file 'append)
    tmp-file))

(defvar nix-sandbox-rc-map (make-hash-table :test 'equal
                                            :size 10))

(defun nix-sandbox-rc (sandbox)
  "Returns the rc file for the given sandbox or creates one."
  (if (gethash sandbox nix-sandbox-rc-map)
      (gethash sandbox nix-sandbox-rc-map)
    (puthash sandbox (nix-create-sandbox-rc sandbox) nix-sandbox-rc-map)))

;;;###autoload
(defun nix-shell-command (sandbox &rest args)
  "Assembles a nix-shell command that gets executed in the specified sandbox."
  (list "sh" "-c" (format "source %s; %s" (nix-sandbox-rc sandbox) (s-join " " args))))

(defun nix-shell-string (sandbox &rest args)
   (combine-and-quote-strings
    (apply 'nix-shell-command sandbox args)))

;;;###autoload
(defun nix-compile (sandbox &rest args)
  (interactive "Dsandbox: \nMcommand: ")
  (compile (apply 'nix-shell-string sandbox args)))

;;;###autoload
(defun nix-shell (sandbox &rest args)
  "Runs a nix-shell command in the given sandbox and returns its output."
  (shell-command-to-string (apply 'nix-shell-string sandbox args)))

;;;###autoload
(defvar nix-exec-path-map (make-hash-table :test 'equal
                                           :size 10))

;;;###autoload
(defun nix-exec-path (sandbox)
  "Returns the `exec-path' of the given sandbox."
  (if (gethash sandbox nix-exec-path-map)
      (gethash sandbox nix-exec-path-map)
    (puthash sandbox
             (split-string (nix-shell sandbox "printenv" "PATH") ":")
             nix-exec-path-map)))

;;;###autoload
(defun nix-executable-find (sandbox exe)
  "Searches for an executable in the given sandbox"
  (let ((exec-path (nix-exec-path sandbox)))
    (and exec-path (executable-find exe))))

;;;###autoload
(defun nix-find-sandbox (path)
  "Searches for a Nix sandbox files or directories starting at the
given path,looking upwards. If the directory contains a `shell.nix'
file, the path to this file is returned. Otherwise if the directory
contains a `default.nix' file, the parent directory is returned."
  (and (file-exists-p path)
   (let* ((sandbox-directory
           (map-nil 'expand-file-name
                    (locate-dominating-file path
                                            '(lambda (dir) (directory-files dir t ".*\.nix$")))))
          (shell-nix (and sandbox-directory (concat sandbox-directory "shell.nix"))))
     (if (and sandbox-directory (file-exists-p shell-nix))
         shell-nix
       sandbox-directory))))

(defun map-nil (f x)
  (if x
      (funcall f x)
    nil))

;;;###autoload
(defun nix-current-sandbox ()
  "Returns the path of the Nix sandbox that is closest
to the current working directory."
  (nix-find-sandbox default-directory))

(provide 'nix-sandbox)

;;; nix-sandbox.el ends here
