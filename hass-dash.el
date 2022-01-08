;;; hass-dash.el --- Dash for Home Assistant -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "25.1") (hass "2.0.0")
;; Version: 1.0.0
;; Author: Ben Whitley
;; Homepage: https://github.com/purplg/hass
;; SPDX-License-Identifier: MIT
;;; Commentary:

;;; Code:
(require 'hass)

(defvar hass-dash-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'hass-dash-refresh)
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map [tab] 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    map)
  "Keymap for hass-dash-mode.")

(defgroup hass-dash '()
  "Customization group for hass-dash."
  :group 'hass-dash
  :prefix "hass-dash-")

(defcustom hass-dash--default-services '(("switch" . "switch.toggle")
                                         ("input_boolean" . "input_boolean.toggle")
                                         ("automation" . "automation.trigger"))
  "An alist of entity domains to their default services."
  :group 'hass-dash
  :type '(repeat (cons string function)))

(defcustom hass-dash-buffer-name "*hass-dash*"
  "The name of the hass-dash buffer."
  :group 'hass-dash
  :type 'string)

(defcustom hass-dash-layout nil
 "A list of cons of entity ID's to their function in the order, top to bottom, to show on the dashboard."
 :group 'hass-dash)

(cl-defun hass-dash--create-widget (entity-id &key name service type icon)
  (unless name ; If no name is set, try to resolve its 'friendly_name' or otherwise just set it to its id.
    (setq name (or (plist-get (cdr (assoc entity-id hass--available-entities))
                              ':friendly_name)
                   entity-id)))
  (unless type ; If no type is set, resolve to the domain portion of its id.
    (setq type (hass--domain-of-entity entity-id)))
  (unless service ; If no service is set, resolve to is default service based on its type.
    (setq service (hass-dash--default-service-of type)))
  (unless icon ; If no icon is set, resolve to is default icon based on the entities domain.
    (setq icon (hass--icon-of-entity entity-id)))
  (widget-create 'toggle
    :tag (concat "hass-dash--entity-" entity-id)
    :format (format "%s %s %s %s" "%[" icon name "- %v%]")
    :value (hass-switch-p entity-id)
    :action (lambda (&rest _) (hass-call-service entity-id service))))

(defun hass-dash--default-service-of (domain)
  (or (cdr (assoc domain hass-dash--default-services))
      (lambda (entity-id)
        (message "hass: No service assigned for domain: %s" domain))))

(defun hass-dash-refresh ()
  (interactive)
  (let ((dash-buffer (get-buffer-create hass-dash-buffer-name)))
    (with-current-buffer dash-buffer
      (let ((inhibit-read-only t)
            (prev-point (progn (beginning-of-line) (point))))
         (erase-buffer)
         (mapc (lambda (layout-item)
                 (let ((entity-id (car layout-item)))
                   (hass-dash--create-widget entity-id
                     :name (plist-get (cdr layout-item) ':name)
                     :service (plist-get (cdr layout-item) ':service)
                     :type (plist-get (cdr layout-item) ':type)
                     :icon (plist-get (cdr layout-item) ':icon)))
                 (insert "\n\n"))
               hass-dash-layout)
         (goto-char prev-point))
      (hass-dash-mode))))

(defun hass-dash-open ()
  (interactive)
  (hass-dash-refresh)
  (let ((dash-buffer (get-buffer-create hass-dash-buffer-name)))
    (switch-to-buffer-other-window dash-buffer)))

(define-derived-mode hass-dash-mode special-mode "Home Assistant Dash"
  "Dashboard for Home Assistant."
  :group 'hass-dash
  :syntax-table nil
  :abbrev-table nil
  :interactive t)

(add-hook 'hass-entity-state-updated-functions (lambda (_) (hass-dash-refresh)))

(hass-dash-refresh)

(provide 'hass-dash)

;;; hass-dash.el ends here
