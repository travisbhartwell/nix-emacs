(require 'json)
(require 'helm)

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
