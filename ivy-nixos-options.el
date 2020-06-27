;;; ivy-nixos-options.el --- An Ivy Interface for nixos-options.

;; Copyright (C) 2020 Samuel Ruprecht

;; Author: Samuel Ruprecht <samuel@ton-kunst.ch>
;; Created: 20 June 2020

;; Keywords: unix
;; Homepage: http://www.github.com/travisbhartwell/nix-emacs/
;; Version: 0.1.0
;; Package-Requires: ((nixos-options "0.0.1") (ivy "0.13.0"))

;; This file is not part of GNU Emacs.

;;; License: GPLv3

;;; Commentary:

;; Useful functions for exploring the NixOS options.  Inspired by
;; https://nixos.org/nixos/options.html.

;;; Code:
(require 'nixos-options)
(require 'ivy)

(defun ivy-nixos-buffer-display (text)
  "Insert TEXT into a custom ivy-doc buffer and show it if it's not visible.
This function should only be called by ivy, as ivy is automatically resumed with `ivy-resume`"
  (let ((buf (get-buffer-create "*nixos-options-ivy-doc*")))
    (with-current-buffer buf
      (view-mode -1)
      (erase-buffer)
      (insert text)
      (goto-char (point-min))
      (view-mode 1))
    (cond ((get-buffer-window buf))
          ((switch-to-buffer-other-window buf)
           (select-window (previous-window nil nil))))))

(defcustom ivy-nixos-options-default 1
  "Defines the default action when pressing enter for `ivy-nixos-options'.
1 - show the help buffer and resume ivy
2 - insert the string into the buffer
3 - show the description and resume ivy"
  :type 'integer
  :group 'nixos-options
  )

  ;;;###autoload
(defun ivy-nixos-options ()
  "Opens an ivy buffer with all nixos options."
  (interactive)
  (ivy-read
   "NixOS Options: " nixos-options
   :caller 'ivy-nixos-options
   :history 'ivy-nixos-options-history
   :preselect (ivy-thing-at-point)
   :action (list ivy-nixos-options-default
                 '("h" (lambda (f) (progn
                                     (ivy-nixos-buffer-display (nixos-options-get-documentation-for-option f))
                                     (ivy-resume)))
                   "View documentation")
                 '("i" (lambda (f) (insert (nixos-options-get-name f)))
                   "Insert into buffer")
                 '("d" (lambda (f) (progn
                                     (ivy-nixos-buffer-display
                                      (message (format
                                                "%s: %s"
                                                (car f)
                                                (nixos-options-get-description f))))
                                     (ivy-resume)))
                   "Show the description"))))

(add-to-list 'ivy-sort-matches-functions-alist '(ivy-nixos-options . ivy--prefix-sort))
(add-to-list 'ivy-re-builders-alist '(ivy-nixos-options . ivy--regex-plus))

(provide 'ivy-nixos-options)

;;; ivy-nixos-options.el ends here
