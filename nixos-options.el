;;; nixos-options.el --- Interface for browsing and completing NixOS options.

;; Copyright (C) 2015 Diego Berrocal and Travis B. Hartwell

;; Author: Diego Berrocal <cestdiego@gmail.com>
;;      Travis B. Hartwell <nafai@travishartwell.net>
;; Created: 18 July 2015

;; Keywords: unix
;; Homepage: http://www.github.com/travisbhartwell/nix-emacs/

;; This file is not part of GNU Emacs.

;;; License:

;; TODO: Likely GPL.

;;; Commentary:

;; Useful functions for exploring the NixOS options.  Inspired by
;; https://nixos.org/nixos/options.html.

;;; Code:

(require 'json)

(defvar nixos-options-json-file
  (let* ((cmd
           "nix-build --no-out-link '<nixpkgs/nixos/release.nix>' -A options")
          (dir (replace-regexp-in-string "\n\\'" ""
                                         (shell-command-to-string cmd))))
    (expand-file-name "share/doc/nixos/options.json" dir))
  "Location of the options file.")

(defun add-name-to-cdr (option)
  (let ((name (car option))
        (data (cdr option)))
    (progn
      (add-to-list 'data `("name" . ,name))
      `(,name . ,data))))

(defvar nixos-options
  (let* ((json-key-type 'string)
         (options (json-read-file nixos-options-json-file)))
    (mapcar 'add-name-to-cdr options)))

(defconst nixos-options-name "name")
(defconst nixos-options-type "type")
(defconst nixos-options-description "description")
(defconst nixos-options-default "default")
(defconst nixos-options-example "example")
(defconst nixos-options-declarations "declarations")

(defconst nixos-options-long-names
  '((nixos-options-name . "Name")
    (nixos-options-type . "Type")
    (nixos-options-description . "Description")
    (nixos-options-default . "Default value")
    (nixos-options-example . "Example value")
    (nixos-options-declarations . "Declared in")))

(defvar nixos-options-name-indent-amount
  (+ 3 (apply 'max (mapcar (lambda (long-name) (length (cdr long-name))) nixos-options-long-names)))
  "Indent by the maximum length, plus a colon, plus two spaces.")

(provide 'nixos-options)
