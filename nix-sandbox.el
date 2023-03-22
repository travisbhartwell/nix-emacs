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
(require 'nix-mode)

(defgroup nix nil
  "customizations for nix"
  :prefix "nix-"
  :group 'external)

(defcustom nix-nixpkgs-path nil
  "Absolute path to a nixpkgs directory.

Can be customized to select a nix-channel
e.g. /home/user/.nix-defexpr/channels/unstable/nixpkgs"
  :group 'nix
  :type '(choice (const :tag "No channel" nil)
                 (directory "Custom path to a nixpkgs distribution")))

(defun nix-create-sandbox-rc (sandbox)
  "Create a new rc file containing the environment for the given SANDBOX."
  (let ((flake-path-p (string-match-p "#" (or sandbox "")))
        (current-directory default-directory))
    (and (not flake-path-p) sandbox (cd (file-name-directory sandbox)))
    (let* ((internal-nix-command (if (or flake-path-p (file-exists-p "flake.nix"))
                                     "nix develop"
                                   "nix-shell"))
           (command (if sandbox
                        (concat internal-nix-command
                                (if flake-path-p (concat " " (shell-quote-argument sandbox)) "")
                                (or (and nix-nixpkgs-path (concat "-I nixpkgs=" nix-nixpkgs-path))
                                    "")
                                (if (string= internal-nix-command "nix-shell") " --run" " -c bash -c")
                                " 'declare +x shellHook; declare -x; declare -xf' "
                                (if (string= internal-nix-command "nix-shell") (shell-quote-argument sandbox))
                                " 2> /dev/null")
                      "bash -c 'declare +x shellHook; declare -x; declare -xf'"))
           (env-str (shell-command-to-string command))
           (tmp-file (make-temp-file "nix-sandbox-rc-")))
      (cd current-directory)
      (message "Ran command %s" command)
      (write-region env-str nil tmp-file 'append)
      tmp-file)))

(defvar nix-sandbox-rc-map (make-hash-table :test 'equal
                                            :size 4))

(defun nix-sandbox-rc (sandbox)
  "Return the rc file for the given SANDBOX or create one."
  (or (gethash sandbox nix-sandbox-rc-map)
      (puthash sandbox (nix-create-sandbox-rc sandbox) nix-sandbox-rc-map)))

;;;###autoload
(defun nix-shell-command (sandbox &rest args)
  "Assemble a command from ARGS that can be executed in the specified SANDBOX."
  (list "bash" "-c" (format "source %s; %s" (nix-sandbox-rc sandbox)
                            (mapconcat 'shell-quote-argument args " "))))

(defun nix-shell-string (sandbox &rest args)
  "Assemble a command string from ARGS that can be executed in the specifed SANDBOX."
   (combine-and-quote-strings
    (apply 'nix-shell-command sandbox args)))

;;;###autoload
(defun nix-compile (sandbox &rest command)
  "Compile a program using the given COMMAND in SANDBOX."
  (interactive "Dsandbox: \nMcommand: ")
  (compile (apply 'nix-shell-string sandbox command)))

;;;###autoload
(defun nix-sandbox/nix-shell (sandbox &rest command)
  "Run a COMMAND in the given SANDBOX and return the output."
  (shell-command-to-string (apply 'nix-shell-string sandbox command)))

(defvar nix-exec-path-map (make-hash-table :test 'equal
                                           :size 4))

;;;###autoload
(defun nix-exec-path (sandbox)
  "Return the `exec-path' of the given SANDBOX."

  (or (gethash sandbox nix-exec-path-map)
      (puthash sandbox
               (split-string (s-trim (nix-sandbox/nix-shell sandbox "printenv" "PATH")) ":")
               nix-exec-path-map)))

;;;###autoload
(defun nix-executable-find (sandbox executable)
  "Search for an EXECUTABLE in the given SANDBOX."
  (let ((exec-path (nix-exec-path sandbox)))
    (and exec-path (executable-find executable))))

;;;###autoload
(defun nix-find-sandbox (path)
  "Uses nix-mode to find a sandbox file. First tries to use the
`nix-flake' variable, then the `nix-file' variable, then tries to
locate upwards a flake.nix file, then a default.nix file, then a
shell.nix file, then any nix file, and finally uses the user
environment."
  (and (file-exists-p path)
       (cl-flet ((map-nil (f x)
                          (if x (funcall f x) nil))
                 (file-contents (filename)
                                (with-temp-buffer
                                  (insert-file-contents filename)
                                  (buffer-string))))
         (let* ((sandbox-directory
                 (map-nil 'expand-file-name
                          (locate-dominating-file path
                                                  '(lambda (dir)
                                                     (seq-filter
                                                      (lambda (candidate)
                                                        (not (file-directory-p candidate)))
                                                      (directory-files dir t ".*\\.nix$"))))))
                (flake-nix (and sandbox-directory (concat sandbox-directory "flake.nix")))
                (default-nix (and sandbox-directory (concat sandbox-directory "default.nix")))
                (shell-nix (and sandbox-directory (concat sandbox-directory "shell.nix"))))
           (cond (nix-flake nix-flake)
                 (nix-file nix-file)
                 ((and sandbox-directory (file-exists-p flake-nix)) flake-nix)
                 ((and sandbox-directory (file-exists-p default-nix)) default-nix)
                 ((and sandbox-directory (file-exists-p shell-nix)) shell-nix)
                 (sandbox-directory sandbox-directory)
                 (t nil))))))

;;;###autoload
(defun nix-current-sandbox ()
  "Return the path of the sandbox that is closest to the current working directory."
  (nix-find-sandbox default-directory))

(defun nix-clear-caches ()
  "Clear cached information for all sandboxes."
  (interactive)
  (clrhash nix-sandbox-rc-map)
  (clrhash nix-exec-path-map))

(provide 'nix-sandbox)

;;; nix-sandbox.el ends here
