;; TODO: Create an action that prints the data in a nicely formated way in a temporary buffer
;; TODO: Cache the json file in a specified directory ~/.emacs.d/cache? by running the nix-build command below
;; cp $(nix-build --no-out-link '<nixpkgs/nixos/release.nix>' -A options)/share/doc/nixos/options.json .
;; TODO: Make this into a proper Emacs module
;; TODO: Add function to refresh the options json cache
;; TODO: Add an action to insert the selected name into the buffer
;; TODO: ? Create a company-mode backend for this for nix-mode?
;; TODO: "Declared In" has link to the file where this option is.  Query the system to find out where in the store this is.  Make a hyperlink to this file.

(require 'json)
(require 'helm)

(defvar nixos-options-json-file
  (expand-file-name "~/options.json")
  "Location of the cached options file.")

(defvar nixos-nixpkgs-base-dir
  "/nix/var/nix/profiles/per-user/root/channels/nixos/nixpkgs"
  "Base directory for the nixpkgs for the current active channel")

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

(defun helm-source-nixos-options-search ()
  `((name . "NixOS Options")
    (requires-pattern . 2)
    (candidates . nixos-options)
    (action . (("Insert into buffer" . (lambda (f) (insert (cdr (assoc "name" f)))))
               ("Pretty print" . (lambda (f) (message "Pretty Printed: %s" (pp f))))
               ("Display name" . (lambda (f) (message "Name: %s" (cdr (assoc "name" f)))))))))

(defun helm-search-nixos-options ()
  (interactive)
  (helm :sources `(,(helm-source-nixos-options-search))
        :buffer "*helm-nixos-options*"))
