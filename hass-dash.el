;;; hass-dash.el --- Dashboard for Home Assistant -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "25.1") (hass "2.0.0")
;; Version: 1.0.0
;; Author: Ben Whitley
;; Homepage: https://github.com/purplg/hass
;; SPDX-License-Identifier: MIT
;;; Commentary:

;;; Code:
(require 'hass)


;; Customizable
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
                                         ("automation" . "automation.trigger")
                                         ("vacuum" . "vacuum.start"))
  "An alist of entity domains to their default services."
  :group 'hass-dash
  :type '(repeat (cons string function)))

(defcustom hass-dash-buffer-name "*hass-dash*"
  "The name of the hass-dash buffer."
  :group 'hass-dash
  :type 'string)

(defcustom hass-dash-layout nil
 "A list of widgets to show on the dashboard.
Each element in the `list' is an `alist' of a Group name to a `plist' of entity IDs with their properties.

The `car' of a list is the group name while the `cdr' is a list of widget definitions for that group.

'((\"Group Name\" . ((\"entity.id_example\" :name \"Human Readable Name\"))))

Widget properties:
NAME sets the displayed name of the widget on the dashboard.

SERVICE is the service to be called on Home Assistant when the widget is pressed.

ICON is the icon displayed on the widget. Requires `all-the-icons' package.

Full example:

(setq hass-dash-layout
 '((\"Group One\" . ((\"input_boolean.test_boolean_entity\" :name \"Toggle test boolean entity\")
                   (\"switch.bedroom_light\" :name \"Bedroom Light\")
                   (\"input_boolean.test_boolean_entity\" :name \"Turn off test boolean entity\"
                                                        :service \"input_boolean.turn_off\")
                   (\"automation.some_automation\")))
   (\"Vacuum Group\" . ((\"vacuum.valetudo_vacuum\" :name \"Vacuum\")
                      (\"vacuum.valetudo_vacuum\" :name \"Vacuum return home\"
                                                :service \"vacuum.return_to_base\")))))"
 :group 'hass-dash
 :type 'list)


;; Helper functions
(defun hass-dash--default-service-of (entity-id)
  (let ((domain (hass--domain-of-entity entity-id)))
    (or (cdr (assoc domain hass-dash--default-services))
        (lambda (entity-id)
          (message "hass: No service assigned for entity: %s" entity-id)))))


;; Dashboard rendering
(cl-defun hass-dash--create-widget (entity-id &key
                                    (name (or (plist-get (cdr (assoc entity-id hass--available-entities))
                                                         ':friendly_name)
                                              entity-id))
                                    (service (hass-dash--default-service-of entity-id))
                                    (icon (hass--icon-of-entity entity-id))
                                    (state entity-id))
  "Insert a widget into the dashboard."
  (let ((format (concat "%["
                        (when icon (concat icon " "))
                        name
                        (when state " - %t")
                        "%]")))
    (widget-create 'push-button
      :format format
      :value (when state (hass-state-of state))
      :action (lambda (&rest _) (hass-call-service entity-id service)))))

(defun hass-dash--insert-groups ()
  (dolist (group hass-dash-layout)
    (insert (propertize (car group) 'face 'shr-h1))
    (insert "\n")
    (hass-dash--insert-group (cdr group))
    (insert "\n")))
 
(defun hass-dash--insert-group (group)
  (dolist (item group)
    (apply 'hass-dash--create-widget item)
    (insert "\n")))


;; User functions
(defun hass-dash-refresh ()
  (interactive)
  (let ((dash-buffer (get-buffer-create hass-dash-buffer-name)))
    (with-current-buffer dash-buffer
      (let ((inhibit-read-only t)
            (prev-point (progn (beginning-of-line) (point))))
         (erase-buffer)
         (hass-dash--insert-groups)
         (goto-char prev-point)
         (hass-dash-mode)))))

;;;###autoload
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

;; Refresh dashboard when entity state is updated
(add-hook 'hass-entity-state-updated-functions (lambda (_) (hass-dash-refresh)))


;; To be removed. Just for easy iteration during development (`eval-buffer`)
(hass-dash-refresh)

(provide 'hass-dash)

;;; hass-dash.el ends here
