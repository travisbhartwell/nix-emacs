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

;;; License: GPLv3

;;; Commentary:

;; Useful functions for exploring the NixOS options.  Inspired by
;; https://nixos.org/nixos/options.html.

;;; Code:

(require 'json)

(defvar nixos-options-name-indent-amount 0
   "Indent by the maximum length, plus a colon, plus two spaces.")

;; Macros for defining constants and functions for working with options
(defmacro define-nixos-options-item (item long-name)
  (let* ((name-const (intern (concat "nixos-options-" item)))
         (long-name-const (intern (concat "nixos-options-" item "-long-name")))
         (long-name-length-plus-padding (+ 3 (length long-name)))
         (long-name-docstring (format "The long description for %s." item))
         (item-getter (intern (concat "nixos-options-get-" item)))
         (item-getter-docstring
          (format "Get the value of %s from OPTION." item))
         (item-display (intern (concat "nixos-options-display-" item)))
         (item-display-docstring
          (format "Display the value for %s from OPTION." item)))
    `(progn
       (defconst ,name-const ,item)
       (defconst ,long-name-const ,long-name ,long-name-docstring)
       (if (> ,long-name-length-plus-padding nixos-options-name-indent-amount)
           (setq nixos-options-name-indent-amount
                 ,long-name-length-plus-padding))
       (defun ,item-getter (option)
         ,item-getter-docstring
         (cdr (assoc ,name-const option)))
       (defun ,item-display (option)
         ,item-display-docstring
         (let ((item (,item-getter option))
               (format-string
                (format "%%-%ds %%s\n" nixos-options-name-indent-amount)))
           (if (not (null item))
               (format format-string (concat ,long-name-const ":") item)
             ""))))))

(define-nixos-options-item "name" "Name")
(define-nixos-options-item "type" "Type")
(define-nixos-options-item "description" "Description")
(define-nixos-options-item "default" "Default value")
(define-nixos-options-item "example" "Example value")
(define-nixos-options-item "declarations" "Declared in")

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
      (add-to-list 'data `(,nixos-options-name . ,name))
      `(,name . ,data))))

(defvar nixos-options
  (let* ((json-key-type 'string)
         (options (json-read-file nixos-options-json-file)))
    (mapcar 'add-name-to-cdr options)))

(defun nixos-options-get-documentation-for-option (option)
  (concat (nixos-options-display-name option)
          (nixos-options-display-type option)
          (nixos-options-display-description option)
          (nixos-options-display-default option)
          (nixos-options-display-example option)
          (nixos-options-display-declarations option)))

;; Borrowed from anaconda-mode
(defun nixos-options-doc-buffer (doc)
  "Display documentation buffer with contents DOC."
  (let ((buf (get-buffer-create "*nixos-options-doc*")))
    (with-current-buffer buf
      (view-mode -1)
      (erase-buffer)
      (insert doc)
      (goto-char (point-min))
      (view-mode 1)
      buf)))

(defun nixos-options-get-option-by-name (name)
  (assoc name nixos-options))

(provide 'nixos-options)
