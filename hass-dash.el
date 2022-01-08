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

(defcustom hass-dash-buffer-name "*hass-dash*"
  "The name of the hass-dash buffer."
  :group 'hass-dash
  :type 'string)

(defcustom hass-dash-layout nil
 "A list of cons of entity ID's to their function in the order, top to bottom, to show on the dashboard."
 :group 'hass-dash)

(defgroup hass-dash '()
  "Minor mode for hass."
  :group 'hass-dash
  :prefix "hass-dash-")

(cl-defun hass-dash--create-widget (entity-id &key name action type icon)
  (unless name
    (setq name (or (plist-get (cdr (assoc entity-id hass--available-entities))
                              ':friendly_name)
                   entity-id)))
  (unless type
    (setq type (intern (hass--domain-of-entity entity-id))))
  (unless action
    (setq action (hass-dash--default-action-of type)))
  (unless icon
    (setq icon (hass--icon-of-entity entity-id)))
  (widget-create 'toggle
    :tag (concat "hass-dash--entity-" entity-id)
    :format (format "%s %s %s %s" "%[" icon name "- %v%]")
    :value (hass-switch-p entity-id)
    :action (lambda (&rest _) (funcall action entity-id))))

(defun hass-dash--default-action-of (domain)
  (cond ((string= "switch" domain)
         #'hass-dash--switch-toggle)
        ((string= "input_boolean" domain)
         #'hass-dash--boolean-toggle)
        ((string= "automation" domain)
         #'hass-dash--automation-trigger)))

(defun hass-dash--switch-toggle (entity-id)
  (hass-call-service entity-id "switch.toggle"))

(defun hass-dash--boolean-toggle (entity-id)
  (hass-call-service entity-id "input_boolean.toggle"))

(defun hass-dash--automation-trigger (entity-id)
  (hass-call-service entity-id "automation.toggle"))

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
                     :action (plist-get (cdr layout-item) ':action)
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
