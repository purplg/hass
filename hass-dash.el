;;; hass-dash.el --- Dashboard for Home Assistant -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "25.1") (hass "2.0.0"))
;; Version: 1.0.0
;; Author: Ben Whitley
;; Homepage: https://github.com/purplg/hass
;; SPDX-License-Identifier: MIT
;;; Commentary:

;; This package extends the `hass' package to include a dashboard to configure
;; quick access to buttons and displays.

;; --------------------
;; Configuration

;; The primary `hass' package must be configured properly first before using
;; this dashboard feature.

;; The main configuration for the dashboard takes place with the
;; `hass-dash-layout' variable.  `hass-dash-layout' declares how the widgets are
;; laid out, what they display, and what they do.  See the docstring for
;; `hass-dash-layout' for details.

;; --------------------
;; Usage

;; To show the dashboard, call the `hass-dash-open' function.  Nothing fancy is
;; done to show this buffer so standard buffer management configuration
;; applies.  It can be handy to use packages like `popper' and/or `shackle' to
;; configure how the dashboard is displayed.

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
  "Keymap for `hass-dash-mode'.")

(defface hass-dash-group-face
  '((t (:inherit info-title-2)))
  "Face for widget group labels in HASS's dashboard."
  :group 'hass-dash)

(defface hass-dash-widget-label-face
  '((t (:inherit widget-button)))
  "Face for widgets in HASS's dashboard."
  :group 'hass-dash)

(defface hass-dash-widget-state-face
  '((t (:inherit hass-dash-widget-label-face)))
  "Face for widgets in HASS's dashboard."
  :group 'hass-dash)

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
  :type '(repeat (cons string string)))

(defcustom hass-dash-buffer-name "*hass-dash*"
  "The name of the hass-dash buffer."
  :group 'hass-dash
  :type 'string)

(defcustom hass-dash-layout nil
 "A list of widgets to show on the dashboard.
Each element in the `list' is an `alist' of a Group name to a `plist' of entity
IDs with their properties.

The `car' of a list is the group label while the `cdr' is a list of widget
definitions for that group.

'((\"Group Label\" . ((\"entity.id_example\" :label \"Human Readable Name\"))))

See `hass-dash--create-widget' for widget properties.

Full example:

\(setq `hass-dash-layout'
 '((\"Group One\" . ((\"input_boolean.test_boolean\"
                    :label \"Toggle entity\")
                   (\"switch.bedroom_light\"
                    :label \"Bedroom Light\")
                   (\"input_boolean.test_boolean\"
                    :label \"Turn off test boolean\"
                    :service \"input_boolean.turn_off\")
                   (\"automation.some_automation\")))
   (\"Vacuum Group\" . ((\"vacuum.valetudo_vacuum\"
                       :label \"Vacuum\")
                      (\"vacuum.valetudo_vacuum\"
                       :label \"Vacuum return home\"
                       :service \"vacuum.return_to_base\"
                       :state nil
                       :icon nil)))))"
 :group 'hass-dash
 :type 'list)

;; Default formatters
(defcustom hass-dash-default-widget-formatter #'hass-dash-widget-formatter
  "The function called to format the widgets on the dashboard."
  :group 'hass-dash
  :type 'function)

(defcustom hass-dash-default-label-formatter #'hass-dash-label-formatter
  "The function called to format the label of widgets on the dashboard."
  :group 'hass-dash
  :type 'function)

(defcustom hass-dash-default-state-formatter #'hass-dash-state-formatter
  "The function called to format the state of widgets on the dashboard."
  :group 'hass-dash
  :type 'function)

(defcustom hass-dash-default-icon-formatter #'hass-dash-icon-formatter
  "The function called to format the icon of widgets on the dashboard."
  :group 'hass-dash
  :type 'function)


;; Helper functions
(defun hass-dash--default-service-of (entity-id)
  "Return the default service to be called for ENTITY-ID."
  (let ((domain (hass--domain-of-entity entity-id)))
    (or (cdr (assoc domain hass-dash--default-services))
        (lambda (entity-id)
          (message "hass: No service assigned for entity: %s" entity-id)))))

(defun hass-dash--track-layout-entities ()
  "Tracks referenced entities in `hass-dash-layout' and update their state."
  (dolist (layout-entry hass-dash-layout)
    (when-let ((group (cond ((listp layout-entry) layout-entry)
                            ((boundp layout-entry) (symbol-value layout-entry)))))
      (dolist (item (cdr group))
        (add-to-list 'hass-tracked-entities
                     (or (plist-get (cdr item) ':state) (car item))))))
  (hass--update-all-entities))


;; Dashboard rendering
(defun hass-dash-widget-formatter (label state icon
                                   label-formatter
                                   state-formatter
                                   icon-formatter)
  "Default constructor for a widget.
This function composes a widget in the way it should be shown on the dashboard
buffer.
LABEL-FORMATTER is a function that manipulates the way the LABEL is rendered to
the dashboard buffer.

STATE-FORMATTER is a function that manipulates the way the STATE is rendered to
the dashboard buffer.

ICON-FORMATTER is a function that manipulates the way the ICON is rendered to
the dashboard buffer."
  (concat (when icon (funcall icon-formatter icon))
          (funcall label-formatter label)
          (when state (funcall state-formatter state))))

(defun hass-dash-label-formatter (label)
  "The default implementation of a widget label formatter.
LABEL is a string of the label of the widget to be rendered."
  (propertize label 'face 'hass-dash-widget-label-face))

(defun hass-dash-state-formatter (state)
  "The default implementation of a widget state formatter.
STATE is a string of the current state of the widget to be rendered."
  (propertize (concat " - "  state)
              'face 'hass-dash-widget-state-face))

(defun hass-dash-icon-formatter (icon)
  "The default implementation of a widget icon formatter.
ICON is the icon of the widget to be rendered."
  (concat icon " "))

(cl-defun hass-dash--create-widget (entity-id &key
                                    (service (hass-dash--default-service-of entity-id))
                                    ;; `:name' keyword is deprecated. Use `:label' instead.
                                    (name (or (plist-get (cdr (assoc entity-id hass--available-entities))
                                                         ':friendly_name)
                                              entity-id))
                                    (label name)
                                    (state entity-id)
                                    (icon (hass--icon-of-entity entity-id))
                                    (widget-formatter hass-dash-default-widget-formatter)
                                    (label-formatter hass-dash-default-label-formatter)
                                    (state-formatter hass-dash-default-state-formatter)
                                    (icon-formatter hass-dash-default-icon-formatter)
                                    confirm
                                    &allow-other-keys)
  "Insert a widget into the dashboard.
ENTITY-ID is the id of the entity in Home Assistant.

SERVICE is the service to be called on Home Assistant when the widget is
pressed.

LABEL sets the displayed label of the widget on the dashboard.

NAME is deprecated.  Use LABEL instead.

STATE is an entity id of the state to show on the widget.  If set
to nil, no state is shown.

ICON is the icon displayed on the widget.  Set to nil to not show an icon.
Requires `all-the-icons' package.

WIDGET-FORMATTER is the function used to format the entire widget.  Can be used
to re-arrange the elements of the widget.  For example, displaying the STATE
before the LABEL.  See `hass-dash-widget-formatter' for an example
implementation.

LABEL-FORMATTER is the function used to format the label of the widget.  See
`hass-dash-label-formatter' for an example implementation.

STATE-FORMATTER is the function used to format the state of the widget.  See
`hass-dash-state-formatter' for an example implementation.

ICON-FORMATTER is the function used to format the icon of the widget.  See
`hass-dash-icon-formatter' for an example implementation.

When CONFIRM is non-nil a prompt will ask for confirmation before the SERVICE
is called.  A string of will be used for a custom prompt.  If a function is
passed then the service will only be called when the function returns t."
  (widget-create 'push-button
    :tag (funcall widget-formatter label (hass-state-of state) icon
                                   label-formatter state-formatter icon-formatter)
    :format (if service "%[%t%]" "%t")
    :action (cond ((stringp confirm)
                   (lambda (&rest _)
                     (when (yes-or-no-p confirm)
                       (hass-call-service entity-id service))))
                  ((functionp confirm)
                   (lambda (&rest _)
                     (when (funcall confirm entity-id)
                       (hass-call-service entity-id service))))
                  (confirm
                   (lambda (&rest _)
                     (when (yes-or-no-p (concat "Toggle " name "? "))
                       (hass-call-service entity-id service))))
                  ((lambda (&rest _) (hass-call-service entity-id service))))))

(defun hass-dash--insert-groups ()
  "Insert all widgets in `hass-dash-layout'."
  (dolist (layout-entry hass-dash-layout)
    (when-let ((group (cond ((listp layout-entry) layout-entry)
                            ((boundp layout-entry) (symbol-value layout-entry)))))
      (insert (propertize (car group) 'face 'hass-dash-group-face))
      (insert "\n")
      (dolist (widget (cdr group))
        (unless (if-let ((hide-fn (plist-get (cdr widget) ':hide-fn)))
                   (funcall hide-fn widget))
          (apply 'hass-dash--create-widget widget)
          (insert "\n")))
      (insert "\n"))))
 

;; User functions
;;;###autoload
(defun hass-dash-refresh ()
  "Rerender the hass-dash buffer."
  (interactive)
  (when (get-buffer-window hass-dash-buffer-name)
    (with-current-buffer (get-buffer-create hass-dash-buffer-name)
        (let ((inhibit-read-only t)
              (prev-line (line-number-at-pos)))
           (erase-buffer)
           (hass-dash--insert-groups)
           (goto-char (point-min))
           (forward-line (1- prev-line))
           (hass-dash-mode)))))

;;;###autoload
(defun hass-dash-open ()
  "Open the hass-dash buffer."
  (interactive)
  (let ((dash-buffer (get-buffer-create hass-dash-buffer-name)))
    (switch-to-buffer-other-window dash-buffer)
    (hass-dash-refresh)))


(define-derived-mode hass-dash-mode special-mode "Home Assistant Dash"
  "Dashboard for Home Assistant."
  :group 'hass-dash
  :syntax-table nil
  :abbrev-table nil
  :interactive t)

;; Refresh dashboard when entity state is updated
(add-hook 'hass-entity-updated-hook 'hass-dash-refresh)

;; After successful connection update the `hass-tracked-entities' list to
;; include the entities in `hass-dash-layout'.
(add-hook 'hass-api-connected-hook #'hass-dash--track-layout-entities)
(when hass--api-running (hass-dash--track-layout-entities))

(provide 'hass-dash)

;;; hass-dash.el ends here
