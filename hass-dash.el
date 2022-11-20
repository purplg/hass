;;; hass-dash.el --- Dashboard for Home Assistant -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "25.1") (hass "2.0.0"))
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
;; Full layout example

;; (setq hass-dash-layout
;;       `(group :format ,(propertize "Home Assistant\n%v" 'face 'outline-1)
;;               (item :format "\n")
;;               (group :format ,(propertize " Kitchen\n %v" 'face 'outline-2)
;;                      (hass-dash-toggle :entity-id "light.kitchen_lights")
;;                      (item :format "\n")
;;                      (hass-dash-toggle :entity-id "switch.entry_light"))))

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


;; Customizable
(defvar hass-dash-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g r") 'hass-dash-refresh)
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map [tab] 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    map)
  "Keymap for `hass-dash-mode'.")

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

(defvar hass-dash-layout nil
  "A list describing the root widget to show on the dashboard.
In most cases, this will be a `group' widget.  You can then build a tree of
arbitrary widgets to display on the dashboard.  You'll probably want to make use
of hass widgets such as `hass-dash-toggle'.

Full example:

\(setq hass-dash-layout
      \\=`(group :format ,(propertize \"Home Assistant\\n%v\" \\='face \\='outline-1)
              (item :format \"\\n\")
              (group :format ,(propertize \" Kitchen\\n %v\" \\='face \\='outline-2)
                     (hass-dash-toggle :entity-id \"light.kitchen_lights\")
                     (item :format \"\\n\")
                     (hass-dash-toggle :entity-id \"switch.entry_light\"))))")


;; Helper functions
(defun hass-dash--track-layout-entities ()
  "Tracks referenced entities in `hass-dash-layout' and update their state."
  (dolist (entity-id (delq nil
                           (delete-dups
                            (flatten-tree
                             (cl-labels ((get-entity-ids (widgets)
                                           (mapcar (lambda (widget)
                                                     (unless (stringp widget)
                                                       (if (string= (car widget) "group")
                                                           (get-entity-ids (cl-remove-if-not 'listp widget))
                                                         (plist-get (cdr widget) :entity-id)))) widgets)))
                               (get-entity-ids (list hass-dash-layout)))))))
    (add-to-list 'hass-tracked-entities entity-id))
  (hass--update-all-entities))

(defun hass-dash--widget-label (widget)
  "Return the label for WIDGET.
Uses the `:label' property if one is set on the WIDGET, otherwise tries to use
the `:friendly_name' property out of the list of available entities.  If neither
is set, falls back to using the `:entity_id' property on the WIDGET."
  (let ((entity-id (widget-get widget :entity-id)))
    (or (widget-get widget :label)
        (plist-get (cdr (assoc entity-id hass--available-entities))
                   ':friendly_name)
        entity-id)))

(defun hass-dash--widget-icon (widget)
  "Return the icon for WIDGET.
Uses the `:icon' property if one is set on the WIDGET, otherwise returns the
default icon based on the entity ID."
  (or (widget-get widget :icon)
      (hass--icon-of-entity (widget-get widget :entity-id))))

(defun hass-dash--widget-format-handler (widget escape)
  "Handle ESCAPE in the format for WIDGET.
Specifically, this format handler handles the following:

• '%i' will insert the icon returned from `hass-dash--widget-icon'
• '%l' will insert the label returned from `hass-dash--widget-label'
• '%s' will insert the current state of the entity"
  (cond ((eq escape ?i) (insert (hass-dash--widget-icon widget)))
        ((eq escape ?l) (insert (propertize (hass-dash--widget-label widget)
                                            'face 'hass-dash-widget-label-face)))
        ((eq escape ?s) (insert (propertize (hass-state-of (widget-get widget :entity-id))
                                            'face 'hass-dash-widget-state-face)))
        (t (widget-default-format-handler widget escape))))

(defun hass-dash--widget-action (widget &optional _)
  "Action handler for WIDGET.
If the `:service' property is set, this will call that service.  Otherwise, it
will call the relevant service in `hass-dash--default-services'.

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
                                  hass-dash--default-services)))))
    (cond ((stringp confirm) (when (y-or-n-p confirm)
                               (hass-call-service entity-id service nil)))
          ((functionp confirm) (when (funcall confirm entity-id)
                                 (hass-call-service entity-id service nil)))
          (confirm (when (y-or-n-p (concat "Toggle "
                                           (hass-dash--widget-label widget)
                                           "? "))
                     (hass-call-service entity-id service nil)))
          (t (hass-call-service entity-id service nil)))))

(define-widget 'hass-dash-toggle 'push-button
  "A toggle widget for home-assistant dashboards.
You must pass an `:entity-id' property to indicate the id of the entity in Home
Assistant.  The following optional properties can also be used:

• `:service': The service to call when triggering the action on the widget.  If
  not passed, then the default will be found in `hass-dash--default-services'
  instead.
• `:label': The friendly name to show for the widget.  If not passed, a sane
  default will be found in the list of available entities.  If nothing is found
  there, then the `:entity-id' property value will be used.
• `:icon': The icon to show for the widget.  If not passed one will be found
  based on the entity id.
• `:confirm': If passed, this will control how the action is confirmed before
  being confirmed.  See `hass-dash--widget-action' for details."
  :format "%[%i %l - %s%]"
  :format-handler 'hass-dash--widget-format-handler
  :action 'hass-dash--widget-action)


;; User functions
;;;###autoload
(defun hass-dash-refresh ()
  "Rerender the hass-dash buffer."
  (interactive)
  (with-current-buffer (get-buffer-create hass-dash-buffer-name)
    (let ((inhibit-read-only t)
          (prev-line (line-number-at-pos)))
      (erase-buffer)
      (widget-create hass-dash-layout)
      (goto-char (point-min))
      (forward-line (1- prev-line))
      (hass-dash-mode))))

;;;###autoload
(defun hass-dash-open ()
  "Open the hass-dash buffer."
  (interactive)
  (hass-dash-refresh)
  (let* ((buffer (get-buffer hass-dash-buffer-name))
         (window (get-buffer-window buffer)))
    (pop-to-buffer buffer)
    (set-window-dedicated-p window t)))


(define-derived-mode hass-dash-mode special-mode "Home Assistant Dash"
  "Dashboard for Home Assistant."
  :group 'hass-dash
  :syntax-table nil
  :abbrev-table nil
  :interactive t)

;; Refresh dashboard when entity state is updated
(add-hook 'hass-entity-updated-hook
          (lambda ()
            (when (get-buffer-window hass-dash-buffer-name)
              (hass-dash-refresh))))

;; After successful connection update the `hass-tracked-entities' list to
;; include the entities in `hass-dash-layout'.
(add-hook 'hass-api-connected-hook #'hass-dash--track-layout-entities)
(when hass--api-running (hass-dash--track-layout-entities))

(provide 'hass-dash)

;;; hass-dash.el ends here
