;;; company-nixos-options.el --- Company Backend for nixos-options

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
(require 'nixos-options)
(require 'cl-lib)

(defvar company-nix-options-keywords
  (mapcar (lambda (nixos-option)
            (list (car nixos-option)
                  (cdr (assoc nixos-options-description nixos-option))))
          nixos-options))

(defun company-nix-options--make-candidate (candidate)
  (let ((text (car candidate))
        (meta (cadr candidate)))
    (propertize text 'meta meta)))

(defun company-nix-options--candidates (prefix)
  (let (res)
    (dolist (item company-nix-options-keywords)
      (when (string-prefix-p prefix (car item))
        (push (company-nix-options--make-candidate item) res)))
    res))

(defun company-nix-options--meta (candidate)
  (format "This will use %s of %s"
          (get-text-property 0 'meta candidate)
          (substring-no-properties candidate)))

(defun company-nix-options--annotation (candidate)
  (format "  ->  %s" (get-text-property 0 'meta candidate)))

(defun company-nix-options--prefix ()
  "Grab prefix at point."
  (or (company-grab-symbol-cons "\\." 2)
      'stop))

;;;###autoload
(defun company-nix-options (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-nix-options))
    (prefix (company-nix-options--prefix))
    (candidates (company-nix-options--candidates arg))
    (annotation (company-nix-options--annotation arg))
    (meta (company-nix-options--meta arg))))

(provide 'company-nixos-options)
