;;; nix-sandbox.el --- Utility functions to work with nix-shell sandboxes

;; Copyright (C) 2015 Sven Keidel

;; Author: Sven Keidel <svenkeidel@gmail.com>
;; Package-Version: 0.1
;; Package-Requires: ((dash "2.11.0"))
;; Homepage: https://github.com/travisbhartwell/nix-emacs

;; This file is not part of GNU Emacs.

;;; License: GPLv3

;;; Commentary:

;; Useful functions for working with nix-shell sandboxes

;;; Code:

(require 'dash)

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

;;;###autoload
(defun nix-shell-command (sandbox &rest args)
  "Assembles a nix-shell command that gets executed in the specified sandbox."
  (append
   (list "nix-shell")
   (if nix-nixpkgs-path
       (list "-I"
        (concat "nixpkgs=" nix-nixpkgs-path)))
   (list "--run"
         (mapconcat 'identity args " ")
         sandbox
         "2>/dev/null")))

(defun nix-shell-string (sandbox &rest args)
  (let* ((cmd (apply 'nix-shell-command sandbox args))
        (run-index (-find-index (lambda (x) (equal x "--run")) cmd))
        (cmd-quoted (-update-at (+ run-index 1) (lambda (x) (concat "'" x "'")) cmd)))
    (mapconcat 'identity cmd-quoted " ")))

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
             (split-string (nix-shell sandbox "echo" "$PATH") ":")
             nix-exec-path-map)))

;;;###autoload
(defun nix-executable-find (sandbox exe)
  "Searches for an executable in the given sandbox"
  (let ((exec-path (nix-exec-path sandbox)))
    (and exec-path (executable-find exe))))

;;;###autoload
(defun nix-find-sandbox (path)
  "Searches for a Nix sandbox starting at the given path,
looking upwards."
  (map-nil 'expand-file-name
   (locate-dominating-file path
                           '(lambda (dir) (directory-files dir t ".*\.nix$")))))

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
