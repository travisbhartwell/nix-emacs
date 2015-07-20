;;; helm-nixos-options.el --- Helm Interface for nixos-options

;; Copyright (C) 2015 Diego Berrocal and Travis B. Hartwell

;; Author: Diego Berrocal <cestdiego@gmail.com>
;;      Travis B. Hartwell <nafai@travishartwell.net>
;; Created: 18 July 2015

;; Keywords: unix
;; Homepage: http://www.github.com/travisbhartwell/nix-emacs/
;; Version: "0.1.0"
;; Package-Requires: ((nixos-options "0.0.1") (helm "1.5.6"))

;; This file is not part of GNU Emacs.

;;; License:

;; TODO: Likely GPL.

;;; Commentary:

;; Useful functions for exploring the NixOS options.  Inspired by
;; https://nixos.org/nixos/options.html.

;;; Code:
(require 'nixos-options)
(require 'helm)

(defun helm-source-nixos-options-search ()
  `((name . "NixOS Options")
    (requires-pattern . 2)
    (candidates . nixos-options)
    (follow . 1)
    (persistent-action . (lambda (f) (message (format "%s" (cdr (assoc nixos-options-description f))))))
    (action . (("Insert into buffer" . (lambda (f) (insert (cdr (assoc nixos-options-name f)))))
               ("Pretty print" . (lambda (f) (message "Pretty Printed: %s" (pp f))))
               ("Display name" . (lambda (f) (message "Name: %s" (cdr (assoc nixos-options-name f)))))))))

(defun helm-search-nixos-options ()
  (interactive)
  (helm :sources `(,(helm-source-nixos-options-search))
        :buffer "*helm-nixos-options*"))

(provide 'helm-nixos-options)
