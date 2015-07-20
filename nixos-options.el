;;; nixos-options.el --- Interface for browsing and completing NixOS options.

;; Copyright (C) 2015 Diego Berrocal and Travis B. Hartwell

;; Author: Diego Berrocal <cestdiego@gmail.com>
;;      Travis B. Hartwell <nafai@travishartwell.net>
;; Created: 18 July 2015

;; Keywords: unix
;; Homepage: http://www.github.com/travisbhartwell/nix-emacs/
;; Version: 0.0.1
;; Package-Requires: ((emacs "24") (json "1.4"))

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

(defvar nixos-options-name-indent-amount 0
   "Indent by the maximum length, plus a colon, plus two spaces.")

(defmacro define-nixos-options-item (item long-name)
  (let* ((name-const (intern (concat "nixos-options-" item)))
         (long-name-const (intern (concat "nixos-options-" item "-long-name")))
         (long-name-length-plus-padding (+ 3 (length long-name)))
         (long-name-docstring (format "The long description for %s." item))A
         (item-getter (intern (concat "nixos-options-get-" item)))
         (item-getter-docstring (format "Get the value of %s from OPTION." item))
         (item-display (intern (concat "nixos-options-display-" item)))
         (item-display-docstring (format "Display the value for %s from OPTION." item)))
    `(progn
      (defconst ,name-const ,item)
      (defconst ,long-name-const ,long-name ,long-name-docstring)
      (if (> ,long-name-length-plus-padding nixos-options-name-indent-amount)
          (setq nixos-options-name-indent-amount ,long-name-length-plus-padding))
      (defun ,item-getter (option)
        ,item-getter-docstring
        (cdr (assoc ,name-const option)))
      (defun ,item-display (option)
        ,item-display-docstring
        (message "%s:  %s" ,long-name-const (,item-getter))))))

(define-nixos-options-item "name" "Name")
(define-nixos-options-item "type" "Type")
(define-nixos-options-item "description" "Description")
(define-nixos-options-item "default" "Default value")
(define-nixos-options-item "example" "Example value")
(define-nixos-options-item "declarations" "Declared in")

(provide 'nixos-options)
