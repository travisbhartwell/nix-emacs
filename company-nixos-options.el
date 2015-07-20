;;; company-nixos-options.el --- Company Backend for nixos-options

;; Copyright (C) 2015 Diego Berrocal and Travis B. Hartwell

;; Author: Diego Berrocal <cestdiego@gmail.com>
;;      Travis B. Hartwell <nafai@travishartwell.net>
;; Created: 18 July 2015

;; Keywords: unix
;; Homepage: http://www.github.com/travisbhartwell/nix-emacs/
;; Version: "0.1.0"
;; Package-Requires: ((company "0.8.0") (nixos-options "0.0.1") (cl-lib "0.5.0"))

;; This file is not part of GNU Emacs.

;;; License:

;; TODO: Likely GPL.

;;; Commentary:

;; Useful functions for exploring the NixOS options.  Inspired by
;; https://nixos.org/nixos/options.html.

;;; Code:
(require 'nixos-options)
(require 'cl-lib)

(defvar company-nixos-options-keywords
  (mapcar (lambda (nixos-option)
            (list (car nixos-option)
                  (cdr (assoc nixos-options-description nixos-option))))
          nixos-options))

(defun company-nixos-options--make-candidate (candidate)
  (let ((text (car candidate))
        (meta (cadr candidate)))
    (propertize text 'meta meta)))

(defun company-nixos-options--candidates (prefix)
  (let (res)
    (dolist (item company-nixos-options-keywords)
      (when (string-prefix-p prefix (car item))
        (push (company-nixos-options--make-candidate item) res)))
    res))

(defun company-nixos-options--meta (candidate)
  (format "This will use %s of %s"
          (get-text-property 0 'meta candidate)
          (substring-no-properties candidate)))

(defun company-nixos-options--annotation (candidate)
  (format "  ->  %s" (get-text-property 0 'meta candidate)))

(defun company-nixos-options--prefix ()
  "Grab prefix at point."
  (or (company-grab-symbol-cons "\\." 2)
      'stop))

;;;###autoload
(defun company-nixos-options (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-nixos-options))
    (prefix (company-nixos-options--prefix))
    (candidates (company-nixos-options--candidates arg))
    (annotation (company-nixos-options--annotation arg))
    (meta (company-nixos-options--meta arg))))

(provide 'company-nixos-options)
