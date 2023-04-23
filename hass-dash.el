;;; hass-dash.el --- Dashboard for Home Assistant -*- lexical-binding: t; -*-

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
;; `hass-dash-layouts' variable.  `hass-dash-layouts' declares how the widgets
;; are laid out, what they display, and what they do.  See the docstring for
;; `hass-dash-layouts' for details.

;; --------------------
;; Full layout example

;;(setq hass-dash-layouts
;;      `((default . ((hass-dash-group :title "Home Assistant"
;;                                     :format "%t\n\n%v"
;;                                     (hass-dash-group :title "Kitchen"
;;                                                      :title-face outline-2
;;                                                      (hass-dash-toggle :entity-id "light.kitchen_lights")
;;                                                      (hass-dash-toggle :entity-id "light.master_bedroom_lights")
;;                                                      (hass-dash-toggle :entity-id "switch.entry_light"
;;                                                                        :label "Hallway"
;;                                                                        :confirm t)))
;;                    (hass-dash-group :title "Group 2"
;;                                     :format "\n\n%t\n\n%v"
;;                                     (hass-dash-toggle :entity-id "light.master_bedroom_fan_light"))))
;;
;;        (simple . ((hass-dash-toggle :entity-id "light.kitchen_lights")
;;                   (hass-dash-toggle :entity-id "switch.entry_lights")))))

;; --------------------
;; Usage

;; To show the dashboard, call the `hass-dash-open' function.  Nothing fancy is
;; done to show this buffer so standard buffer management configuration
;; applies.  It can be handy to use packages like `popper' and/or `shackle' to
;; configure how the dashboard is displayed.

;;; Code:
(require 'subr-x)
(require 'hass)
(require 'wid-edit)

(require 'hass-websocket)

(defvar-local hass-dash--widgets '())


;;; Customizable
(defvar hass-dash-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map [tab] 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    map)
  "Keymap for `hass-dash-mode'.")

(defface hass-dash-group
  '((t (:inherit outline-1)))
  "Face for dashboard titles in HASS's dashboard."
  :group 'hass-dash)

(defface hass-dash-widget-label
  '((t (:inherit outline-8)))
  "Face for widgets in HASS's dashboard."
  :group 'hass-dash)

(defgroup hass-dash '()
  "Customization group for hass-dash."
  :group 'hass-dash
  :prefix "hass-dash-")

(defcustom hass-dash-default-services '(("automation" . "automation.trigger")
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

(defcustom hass-dash-buffer-name-function #'hass-dash--buffer-name
  "Function to generate a dashboard buffer name.
Takes one argument, the key of the dashboard.  See the default
`hass-dash--buffer-name' for an example implementation."
  :group 'hass-dash
  :type 'function)

(defvar hass-dash-layouts nil
  "An alist describing the dashboards.
The key of each entry is a dashboard name which you can open with
`hass-dash-open'.  The value for each entry is a list describing the root
widgets to show on the dashboard.  You can then build a tree of arbitrary
widgets to display on the dashboard.  You'll probably want to make use of hass
widgets such as `hass-dash-toggle' or `hass-dash-group'.

Full example:

\(setq `hass-dash-layouts'
  \\=`((default .
     ((hass-dash-group
       :title \"Home Assistant\"
       :format \"%t\\n\\n%v\"
       (hass-dash-group
        :title \"Kitchen\"
        :title-face outline-2
        (hass-dash-toggle :entity-id \"light.kitchen_lights\")
        (hass-dash-toggle :entity-id \"light.master_bedroom_lights\")
        (hass-dash-toggle :entity-id \"switch.entry_light\"
                          :label \"Hallway\"
                          :confirm t)))
      (hass-dash-group
       :title \"Group 2\"
       :format \"\\n\\n%t\\n\\n%v\"
       (hass-dash-toggle :entity-id \"light.master_bedroom_fan_light\"))))

    (simple .
     ((hass-dash-toggle :entity-id \"light.kitchen_lights\")
      (hass-dash-toggle :entity-id \"switch.entry_lights\")))))")


;;; Dashboard rendering
(defun hass-dash--buffer-name (dashboard)
  "Return the name of the hass-dash buffer for dashboard key DASHBOARD."
  (concat "*hass-dash-" (symbol-name dashboard) "*"))

(defun hass-dash--update ()
  "Update all currently active dashboards with entity state."
  (let ((dashboard-buffers (mapcar (lambda (dashboard)
                                     (get-buffer (funcall hass-dash-buffer-name-function (car dashboard))))
                                   hass-dash-layouts)))
    (dolist (buffer dashboard-buffers)
      (when buffer
        (with-current-buffer (get-buffer buffer)
          (dolist (widget hass-dash--widgets)
            (let ((icon (widget-get widget :icon))
                  (label (hass-dash--widget-label widget)))
              (widget-put widget :tag (hass-dash--widget-format-tag icon label)))
            (widget-value-set widget (hass-state-of (widget-get widget :entity-id)))))))))

(defun hass-dash--render (layout)
  "Render a hass-dash layout in the current buffer.
LAYOUT is the layout in `hass-dash-layouts' to be rendered."
  (let ((prev-line (line-number-at-pos)))
    (erase-buffer)
    (widget-create
      (append '(group :format "%v")
              layout))
    (goto-char (point-min))
    (forward-line (1- prev-line))))


;;; Widget definitions
(defun hass-dash--widget-label (widget)
  "Return the label for WIDGET.
Uses the `:label' property if one is set on the WIDGET, otherwise tries to use
the `:friendly_name' property out of the list of available entities.  If neither
is set, falls back to using the `:entity_id' property on the WIDGET."
  (propertize (or (widget-get widget :label)
                  (let ((entity-id (widget-get widget :entity-id)))
                    (or (hass-friendly-name entity-id)
                        entity-id)))
              'face 'hass-dash-widget-label))

(defun hass-dash--widget-format-tag (icon label)
  "Formats a tag with ICON and LABEL to be rendered on a dashboard."
  (if icon
      (concat icon " " label)
    label))

(defun hass-dash--widget-create (widget)
  "Create the widget WIDGET.
This just uses `widget-default-create', but sets the `:tag' property if it isn't
already set by using the widget icon and label."
  (unless (widget-get widget :tag)
    (let* ((entity-id (widget-get widget :entity-id))
           (icon (or (widget-get widget :icon)
                     (hass--icon-of-entity entity-id)))
           (label (hass-dash--widget-label widget)))
      (add-to-list 'hass-tracked-entities entity-id)
      (widget-put widget :icon icon)
      (widget-put widget :tag (hass-dash--widget-format-tag icon label))
      (widget-put widget :value (hass-state-of entity-id))
      (add-to-list 'hass-dash--widgets widget)))
  (widget-default-create widget))

(defun hass-dash--widget-action (widget &optional _)
  "Action handler for WIDGET.
If the `:service' property is set, this will call that service.  Otherwise, it
will call the relevant service in `hass-dash-default-services'.

The `:confirm' property can be used to trigger a confirmation before calling the
service.  It can take on the following values:

• if it's a string, that string will be used for the prompt
• if it's a function, then the service will only be called if that function
  returns t
• if it's t, a prompt will be created using the value returned by
  `hass-dash--widget-label'"
  (let* ((confirm (widget-get widget :confirm))
         (entity-id (widget-get widget :entity-id))
         (service (or (widget-get widget :service)
                      (cdr (assoc (hass--domain-of-entity entity-id)
                                  hass-dash-default-services)))))
    (cond ((stringp confirm) (when (y-or-n-p confirm)
                               (hass-call-service entity-id service nil)))
          ((functionp confirm) (when (funcall confirm entity-id)
                                 (hass-call-service entity-id service nil)))
          (confirm (when (y-or-n-p (concat "Toggle "
                                           (hass-dash--widget-label widget)
                                           "? "))
                     (hass-call-service entity-id service nil)))
          (t (hass-call-service entity-id service nil)))))

(define-widget 'hass-dash-state 'item
  "A read-only widget for home-assistant dashboards.
You must pass an `:entity-id' property to indicate the id of the entity in Home
Assistant.  The following optional properties can also be used:

• `:label': The friendly name to show for the widget.  If not passed, a sane
  default will be found in the list of available entities.  If nothing is found
  there, then the `:entity-id' property value will be used.
• `:icon': The icon to show for the widget.  If not passed one will be found
  based on the entity id."
  :create #'hass-dash--widget-create
  :format "%t: %v\n")

(define-widget 'hass-dash-button 'push-button
  "A button widget for home-assistant dashboards.
You must pass an `:entity-id' property to indicate the id of the entity in Home
Assistant.  The following optional properties can also be used:

• `:service': The service to call when triggering the action on the widget.  If
  not passed, then the default will be found in `hass-dash-default-services'
  instead.
• `:label': The friendly name to show for the widget.  If not passed, a sane
  default will be found in the list of available entities.  If nothing is found
  there, then the `:entity-id' property value will be used.
• `:icon': The icon to show for the widget.  If not passed one will be found
  based on the entity id.
• `:confirm': If passed, this will control how the action is confirmed before
  being confirmed.  See `hass-dash--widget-action' for details."
  :create #'hass-dash--widget-create
  :format "%[%t: %v%]\n"
  :value-create #'widget-item-value-create
  :action #'hass-dash--widget-action)

(defun hass-dash--button-widget-value-get (widget)
  "Get the state for a toggle WIDGET."
  (hass-state-of (widget-get widget :entity-id)))

(define-widget 'hass-dash-toggle 'toggle
  "A toggle widget for home-assistant dashboards.
You must pass an `:entity-id' property to indicate the id of the entity in Home
Assistant.  The following optional properties can also be used:

• `:service': The service to call when triggering the action on the widget.  If
  not passed, then the default will be found in `hass-dash-default-services'
  instead.
• `:label': The friendly name to show for the widget.  If not passed, a sane
  default will be found in the list of available entities.  If nothing is found
  there, then the `:entity-id' property value will be used.
• `:icon': The icon to show for the widget.  If not passed one will be found
  based on the entity id.
• `:confirm': If passed, this will control how the action is confirmed before
  being confirmed.  See `hass-dash--widget-action' for details."
  :create #'hass-dash--widget-create
  :format "%[%t: %v%]\n"
  :value-get #'hass-dash--toggle-widget-value-get
  :action #'hass-dash--widget-action)

(defun hass-dash--toggle-widget-value-get (widget)
  "Set the state for a toggle WIDGET."
  (hass-switch-p (widget-get widget :entity-id)))

(define-widget 'hass-dash-group 'group
  "A grouping widget for home-assistant dashboards.
You can pass `:title' to give the group a title, and pass `:title-face' to set
the font face for the title."
  :format "%t\n%v"
  :create #'hass-dash--group-create
  :title-face 'hass-dash-group)

(defun hass-dash--group-create (widget)
  "Create the hass dashboard group WIDGET.
This just uses `widget-default-create', but sets the `:tag' property if it isn't
already set using the `:title' and `:title-face' properties."
  (unless (widget-get widget :tag)
    (widget-put widget :tag (propertize (widget-get widget :title)
                                        'face (widget-get widget :title-face))))
  (widget-default-create widget))


;;; User functions
;;;###autoload
(defun hass-dash-open (dashboard)
  "Open the hass-dash buffer for DASHBOARD."
  (interactive (list (pcase (length hass-dash-layouts)
                       (0 (hass--warning "You must configure some dashboards in `hass-dash-layouts'.") nil)
                       (1 (caar hass-dash-layouts))
                       (_ (intern (completing-read "Dashboard: " hass-dash-layouts))))))
  (hass-ensure)
  (when (and dashboard (hass-websocket-ensure))
    (let* ((buffer (get-buffer-create (funcall hass-dash-buffer-name-function dashboard)))
           (window (get-buffer-window buffer))
           (layout (cdr (assoc dashboard hass-dash-layouts))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (hass-dash-mode)
          (hass-dash--render layout)))
      (hass--update-tracked-entities)
      (pop-to-buffer buffer)
      (set-window-dedicated-p window t))))

(define-derived-mode hass-dash-mode special-mode "Home Assistant Dash"
  "Dashboard for Home Assistant."
  :group 'hass-dash
  :syntax-table nil
  :abbrev-table nil
  :interactive t
  ;; Refresh dashboard when entity state is updated
  (add-hook 'hass-entity-updated-hook #'hass-dash--update))

(provide 'hass-dash)

;;; hass-dash.el ends here
