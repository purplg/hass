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
    (define-key map (kbd "g r") 'hass-dash-refresh)
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map [tab] 'widget-forward)
    (define-key map [backtab] 'widget-backward)
   map)
  "Keymap for hass-dash-mode.")

(defgroup hass-dash '()
  "Customization group for hass-dash."
  :group 'hass-dash
  :prefix "hass-dash-")

(defcustom hass-dash--default-services '(("automation" . "automation.trigger")
                                         ("counter" . "counter.increment")
                                         ("cover" . "cover.toggle")
                                         ("fan" . "fan.toggle")
                                         ("input_boolean" . "input_boolean.toggle")
                                         ("light" . "light.toggle")
                                         ("media_player" . "media_player.media_play_pause")
                                         ("scene" . "scene.turn_on")
                                         ("switch" . "switch.toggle")
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

See `hass-dash--create-widget' for widget properties.

Full example:

(setq hass-dash-layout
 '((\"Group One\" . ((\"input_boolean.test_boolean_entity\" :name \"Toggle test boolean entity\")
                   (\"switch.bedroom_light\" :name \"Bedroom Light\")
                   (\"input_boolean.test_boolean_entity\" :name \"Turn off test boolean entity\"
                                                        :service \"input_boolean.turn_off\")
                   (\"automation.some_automation\")))
   (\"Vacuum Group\" . ((\"vacuum.valetudo_vacuum\" :name \"Vacuum\")
                      (\"vacuum.valetudo_vacuum\" :name \"Vacuum return home\"
                                                :service \"vacuum.return_to_base\"
                                                :state nil
                                                :icon nil)))))"
 :group 'hass-dash
 :type 'list)


;; Helper functions
(defun hass-dash--default-service-of (entity-id)
  "Returns the default service to be called for ENTITY-ID"
  (let ((domain (hass--domain-of-entity entity-id)))
    (or (cdr (assoc domain hass-dash--default-services))
        (lambda (entity-id)
          (message "hass: No service assigned for entity: %s" entity-id)))))

(defun hass-dash--track-layout-entities ()
  "Tracks referenced entities in `hass-dash-layout' and updates their state."
  (dolist (group hass-dash-layout)
    (dolist (item (cdr group))
      (add-to-list 'hass-tracked-entities (car item))))
  (hass--update-all-entities))


;; Dashboard rendering
(cl-defun hass-dash--create-widget (entity-id &key
                                    (name (or (plist-get (cdr (assoc entity-id hass--available-entities))
                                                         ':friendly_name)
                                              entity-id))
                                    (service (hass-dash--default-service-of entity-id))
                                    (icon (hass--icon-of-entity entity-id))
                                    (state entity-id))
  "Insert a widget into the dashboard.
ENTITY-ID is the id of the entity in Home Assistant.

NAME sets the displayed name of the widget on the dashboard.

SERVICE is the service to be called on Home Assistant when the widget is pressed.

ICON is the icon displayed on the widget. Set to `nil' to not show an icon. Requires `all-the-icons' package.

STATE is an entity id of the state to show on the widget. If set to `nil', no state is shown."
  (let ((format (concat "%["
                        (when icon (concat icon " "))
                        (replace-regexp-in-string "%" "%%" name)
                        (when state " - %t")
                        "%]")))
    (widget-create 'push-button
      :format format
      :value (when state (hass-state-of state))
      :action (lambda (&rest _) (hass-call-service entity-id service)))))

(defun hass-dash--insert-groups ()
  "Insert all widgets in `hass-dash-layout'"
  (dolist (group hass-dash-layout)
    (insert (propertize (car group) 'face 'shr-h1))
    (insert "\n")
    (hass-dash--insert-group (cdr group))
    (insert "\n")))
 
(defun hass-dash--insert-group (group)
  "Insert all widgets in a group

GROUP is a list of widget definitions to be inserted into the buffer."
  (dolist (item group)
    (apply 'hass-dash--create-widget item)
    (insert "\n")))


;; User functions
(defun hass-dash-refresh ()
  "Rerender the hass-dash buffer"
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
  "Open the hass-dash buffer"
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

;; After successful connection update the `hass-tracked-entities' list to include the entities in `hass-dash-layout'.
(add-hook 'hass-api-connected-hook #'hass-dash--track-layout-entities)
(when hass--api-running (hass-dash--track-layout-entities))

(provide 'hass-dash)

;;; hass-dash.el ends here
