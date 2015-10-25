;;; nixos-sandbox.el --- Utility functions to work with nix-shell sandboxes

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

(defgroup nixos nil
  "customizations for nixos"
  :prefix "nixos-"
  :group nil)

(defcustom nixos-nixpkgs-path nil
  "Absolute path to a nixpkgs directory.

Can be customized to select a nix-channel
e.g. /home/user/.nix-defexpr/channels/unstable/nixpkgs"
  :group 'nixos
  :type '(choice (const :tag "No channel" nil)
                 (directory "Custom path to a nixpkgs distribution")))

(defun nix-shell-command (sandbox &rest args)
  "Assembles a nix-shell command that gets executed in the specified sandbox."
  (append
   (list "nix-shell")
   (if nixos-nixpkgs-path
       (list "-I"
        (concat "nixpkgs=" nixos-nixpkgs-path)))
   (list "--run"
         (mapconcat 'identity args " ")
         sandbox)))


(defun nix-shell-string (sandbox &rest args)
  (let ((cmd (apply 'nix-shell-command sandbox args)))
    (mapconcat (lambda (x) (concat "'" x "'")) cmd " ")))


;;;###autoload
(defun nix-compile (sandbox &rest args)
  (interactive "Dsandbox: \nMcommand: ")
  (compile (apply 'nix-shell-string sandbox args)))

;;;###autoload
(defun nix-shell (sandbox &rest args)
  "Runs a nix-shell command in the given sandbox and returns its output."
  (shell-command-to-string (apply 'nix-shell-string sandbox args)))

;;;###autoload
(defvar nixos-exec-path-map (make-hash-table :test 'equal
                                             :size 10))

(defun nixos-exec-path (sandbox)
  "Returns the `exec-path' of the given sandbox."
  (if (gethash sandbox nixos-exec-path-map)
      (gethash sandbox nixos-exec-path-map)
    (puthash sandbox
             (split-string (nix-shell sandbox "echo" "$PATH") ":")
             nixos-exec-path-map)))

;;;###autoload
(defun nixos-executable-find (sandbox exe)
  "Searches for an executable in the given sandbox"
  (let ((exec-path (nixos-exec-path sandbox)))
    (and exec-path (executable-find exe))))

(defun nixos-find-sandbox (path)
  "Searches for a NixOS sandbox starting at the given path,
looking upwards."
  (map-nil 'expand-file-name
   (locate-dominating-file path
                           '(lambda (dir) (directory-files dir t ".*\.nix$")))))

(defun map-nil (f x)
  (if x
      (funcall f x)
    nil))

(defun nixos-current-sandbox ()
  "Returns the path of the NixOS sandbox that is closest
to the current working directory."
  (nixos-find-sandbox default-directory))

(provide 'nixos-sandbox)

;;; nixos-sandbox.el ends here
