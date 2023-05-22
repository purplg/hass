;;; hass-dash.el --- Dashboard for Home Assistant -*- lexical-binding: t; -*-

;; Author: Ben Whitley
;; Homepage: https://github.com/purplg/hass
;; SPDX-License-Identifier: MIT
;;; Commentary:

;; This package extends the `hass' package to include a dashboard to configure
;; quick access to buttons and displays.  The primary `hass' package must be
;; configured properly first before using this dashboard feature.

;; The main configuration for the dashboard takes place with the
;; `hass-dash-layouts' variable.  `hass-dash-layouts' declares how the widgets
;; are laid out, what they display, and what they do.  See the docstring for
;; `hass-dash-layouts' for details.

;;;; Widget properties

;; All built-in widget properties apply.
;; See Info node `(widget) The Emacs Widget Library'

;;  :label
;;
;; The human-readable text to display inside the widget. This will, by default,
;; be prefixed to the current value (often state) of the widget. Internally,
;; this property is usedf to derive the `:tag' property. You can override `:tag' if
;; you want, but then `:label' and `:icon' are not used.

;;  :icon
;;
;; This is the icon shown within the widget next to `:label'. By default, hass
;; will try to pick a good icons by selecting the icon associated with the
;; entity's domain in `hass-icons'.

;;  :confirm
;;
;; Prompt the user for confirmation when triggering a widget.
;;
;; - If set to t, a default prompt will be used to ask the user for
;;   confirmation.
;;
;; - If set to a string, that string will be used for the prompt.
;;
;; - If it's a function, then the service will only be called if that function
;;   returns t. You're expected to call `y-or-n-p', or similar, within this
;;   function.

;;  :service
;;
;; The service called when triggering the widget. If omitted, hass will use the
;; default for the associated domain listed in `hass-dash-default-services'. If
;; you want to trigger a more complex service with a payload, you should use the
;; built-in widget property `:action'.

;;  :value-source
;;
;; Where the value of the slider should derive from.  It can be either 'state
;; or '(attribute . name_of_attribute) where `name_of_attribute' is the name of
;; the attribute the value should use. When this value is omitted, hass will
;; select the relevant option based on the entities' domain according to the
;; variable `hass-dash--widget-preferred-attribute'.

;;;; Group properties

;;  :title
;;
;; Similar to `:label' in the widget properties, this is the text displayed at
;; the top of the group.

;;;; Examples

;;
;; Full layout example
;;

;; You can set `hass-dash-layouts' directly like in the following example:

;;   (setq hass-dash-layouts
;;         `((default . ((hass-dash-group :title "Home Assistant"
;;                                        :format "%t\n\n%v"
;;                                        (hass-dash-group :title "Kitchen"
;;                                                         :title-face outline-2
;;                                                         (hass-dash-toggle :entity-id "light.kitchen_lights")
;;                                                         (hass-dash-toggle :entity-id "light.master_bedroom_lights")
;;                                                         (hass-dash-toggle :entity-id "switch.entry_light"
;;                                                                           :label "Hallway"
;;                                                                           :confirm t)))
;;                       (hass-dash-group :title "Group 2"
;;                                        :format "\n\n%t\n\n%v"
;;                                        (hass-dash-toggle :entity-id "light.master_bedroom_fan_light"))))
;;
;;           (simple . ((hass-dash-toggle :entity-id "light.kitchen_lights")
;;                      (hass-dash-toggle :entity-id "switch.entry_lights")))))

;;
;; Layout file example
;;

;; Or for more complex layouts you can create a layout file and load with
;; `hass-dash-load-layout'.  The following defines the same layout as above.
;;

;;   default
;;
;;   (hass-dash-group :title "Home Assistant"
;;                    :format "%t\n\n%v"
;;                    (hass-dash-group :title "Kitchen"
;;                                     :title-face outline-2
;;                                     (hass-dash-toggle :entity-id "light.kitchen_lights")
;;                                     (hass-dash-toggle :entity-id "light.master_bedroom_lights")
;;                                     (hass-dash-toggle :entity-id "switch.entry_light"
;;                                                       :label "Hallway"
;;                                                       :confirm t)))
;;   (hass-dash-group :title "Group 2"
;;                    :format "\n\n%t\n\n%v"
;;                    (hass-dash-toggle :entity-id "light.master_bedroom_fan_light"))
;;
;;   simple
;;
;;   (hass-dash-toggle :entity-id "light.kitchen_lights")
;;   (hass-dash-toggle :entity-id "switch.entry_lights")

;; --------------------
;; Usage

;; To show the dashboard, call the `hass-dash-open' function.  Nothing fancy is
;; done to show this buffer so standard buffer management configuration
;; applies.  It can be handy to use packages like `popper' and/or `shackle' to
;; configure how the dashboard is displayed.

;;; Code:
(require 'subr-x)
(require 'wid-edit)

(require 'hass)
(require 'hass-websocket)

(defvar-local hass-dash--widgets '()
  "An alist of entity-id's and points where associated widgets are
at.")

(defvar-local hass-dash--rendering nil
  "Whether the dashboard is currently rendering.
This is used to populate `hass-dash--widgets'. When we are
actively rendering, then we'll add the widget to the list on
creation.")


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

(defcustom hass-dash-group-indent 1
  "How much to indent elements of a group widget.
How much to offset the of each group and subgroups. This value
gets added to the `:indent' tag for every subgroup.

Set this to `0' to not indent groups at all."
  :group 'hass-dash
  :type 'integer)

(defvar hass-dash-layouts nil
  "An alist describing the dashboards.
The key of each entry is a dashboard name which you can open with
`hass-dash-open'.  The value for each entry is a list describing the root
widgets to show on the dashboard.  You can then build a tree of arbitrary
widgets to display on the dashboard.  You'll probably want to make use of hass
widgets such as `hass-toggle' or `hass-group'.

Full example:

\(setq `hass-dash-layouts'
  \\=`((default .
     ((hass-group
       :title \"Home Assistant\"
       :format \"%t\\n\\n%v\"
       (hass-group :title \"Kitchen\"
        :title-face outline-2
        (hass-toggle \"light.kitchen_lights\")
        (hass-toggle \"light.master_bedroom_lights\")
        (hass-toggle \"switch.entry_light\"
                     :label \"Hallway\"
                     :confirm t)))
      (hass-group :title \"Group 2\"
       :format \"\\n\\n%t\\n\\n%v\"
       (hass--toggle \"light.master_bedroom_fan_light\"))))

    (simple .
     ((hass-toggle \"light.kitchen_lights\")
      (hass-toggle \"switch.entry_lights\")))))")

(defvar hass-dash-slider-value-types '((light . percent)
                                       (counter . raw)
                                       (input_number . raw)))


;;; Dashboard rendering
(defun hass-dash--buffer-name (dashboard)
  "Return the name of the hass-dash buffer for dashboard key DASHBOARD."
  (concat "*hass-dash-" (symbol-name dashboard) "*"))

(defun hass-dash--update-entity (entity-id)
  (let ((dashboard-buffers (mapcar (lambda (dashboard)
                                     (get-buffer (funcall hass-dash-buffer-name-function (car dashboard))))
                                   hass-dash-layouts)))
    (dolist (buffer (seq-filter #'identity dashboard-buffers))
      (with-current-buffer buffer
        (dolist (widget-point (alist-get entity-id hass-dash--widgets nil nil #'string=))
          (when-let* ((widget (widget-at widget-point)))
            (widget-value-set widget (widget-value widget))))))))


(defun hass-dash--render (layout)
  "Render a hass-dash layout in the current buffer.
LAYOUT is the layout in `hass-dash-layouts' to be rendered."
  (erase-buffer)
  (let ((hass-dash--rendering t))
    (dolist (widget layout)
      (widget-create widget))))

(defmacro hass-dash--percent (value min max)
  "Return the completion percent of VALUE between MIN and MAX."
  `(* (/ (- ,value ,min)
         (- ,max ,min))
      100.0))


;;; Widget definitions

;; Every hass-dash widget should use `hass-dash--widget-create' for it's
;; ':create' parameter. This function parses the shared/tweaked dashboard
;; parameters, like ':tag' and ':icon' and adds the widget to the list of all
;; widgets so they can be cleaned up when necessary. You can embed
;; `hass-dash--widget-create' in your own ':create' function, so long as it is
;; eventually called.

(defun hass-dash--widget-convert (widget)
  "Initialize a dashboard widget."
  (when-let* ((type (car widget))
              (args (widget-get widget :args))
              (entity-id (pop args))
              (widget (push type args)))
    (let* ((icon (or (widget-get widget :icon)
                     (hass--icon-of-entity entity-id)))
           (label (or (widget-get widget :label)
                      (hass-friendly-name entity-id)
                      entity-id))
           (tag (or (widget-get widget :tag)
                    (if icon (concat icon " " label) label)))
           (service (or (widget-get widget :service)
                        (cdr (assoc (hass--domain-of-entity entity-id)
                                    hass-dash-default-services)))))
      (widget-put widget :entity-id entity-id)
      (widget-put widget :icon icon)
      (widget-put widget :label label)
      (widget-put widget :tag tag)
      (widget-put widget :value (widget-value widget))
      (widget-put widget :service service))
    widget))

(defun hass-dash--widget-create (widget)
  "Create the widget WIDGET.
This just uses `widget-default-create', but sets the `:tag' property if it isn't
already set by using the widget icon and label."
  (widget-default-create widget)
  (when-let ((hass-dash--rendering)
             (entity-id (widget-get widget :entity-id)))
    (add-to-list 'hass-tracked-entities entity-id)
    (let ((marker (widget-get widget :from)))
      (push (copy-marker marker) (alist-get entity-id hass-dash--widgets nil nil #'string=)))))

(defun hass-dash--action-none (widget &optional _)
  "Action for when service is unsupported for widget type."
  (message "No default action for entity `%s'"
           (widget-get widget :entity-id)))

(defun hass-dash--action (widget &optional _)
  "Action handler for WIDGET.
If the `:service' property is set, this will call that service.  Otherwise, it
will call the relevant service in `hass-dash-default-services'.

The `:confirm' property can be used to trigger a confirmation before calling the
service.  It can take on the following values:

• if it's a string, that string will be used for the prompt
• if it's a function, then the service will only be called if that function
  returns t
• if it's t, a prompt will be created using the ':label' property of the
  widget."
  (let* ((confirm (widget-get widget :confirm))
         (entity-id (widget-get widget :entity-id))
         (service (or (widget-get widget :service))))
    (cond ((stringp confirm) (when (y-or-n-p confirm)
                               (hass-call-service entity-id service nil)))
          ((functionp confirm) (when (funcall confirm entity-id)
                                 (hass-call-service entity-id service nil)))
          (confirm (when (y-or-n-p (concat "Toggle " (widget-get widget :label) "? "))
                     (hass-call-service entity-id service nil)))
          (t (hass-call-service entity-id service nil)))))

(defun hass-dash--value-state (widget)
  (when-let ((entity-id (widget-get widget :entity-id))
             (value-source (widget-get widget :value-source)))
    (cond ((listp value-source) (hass-attribute-of entity-id (cadr value-source)))
          ((eq 'state value-source) (hass-state-of entity-id))
          (t (hass--warning "Invalid :value-source.") "ERR"))))

;;;; State widget
(define-widget 'hass-state 'item
  "A read-only widget for home-assistant dashboards.
The following optional properties can be used:

• `:label': The friendly name to show for the widget.  If not passed, a sane
  default will be found in the list of available entities.  If nothing is found
  there, then the `:entity-id' property value will be used.
• `:icon': The icon to show for the widget.  If not passed one will be found
  based on the entity id."
  :convert-widget #'hass-dash--widget-convert
  :create #'hass-dash--widget-create
  :button-face 'default
  :format "%[%t: %v%]\n"
  :value-get #'hass-dash--value-state)

(define-widget 'hass-dash-state 'hass-state
  "Replaced with `hass-state'.")

;;;; Button widget
(define-widget 'hass-button 'push-button
  "A button widget for home-assistant dashboards.
The following optional properties can also be used:

• `:service': The service to call when triggering the action on the widget.  If
  not passed, then the default will be found in `hass-dash-default-services'
  instead.
• `:label': The friendly name to show for the widget.  If not passed, a sane
  default will be found in the list of available entities.  If nothing is found
  there, then the `:entity-id' property value will be used.
• `:icon': The icon to show for the widget.  If not passed one will be found
  based on the entity id.
• `:confirm': If passed, this will control how the action is confirmed before
  being confirmed.  See `hass-dash--action' for details."
  :convert-widget #'hass-dash--widget-convert
  :create #'hass-dash--widget-create
  :format "%[%t: %v%]\n"
  :value-get #'hass-dash--value-state
  :value-create #'widget-item-value-create
  :action #'hass-dash--action)

(define-widget 'hass-dash-button 'hass-button
  "Replaced with `hass-button'.")

;;;; Slider widget

(define-widget 'hass-slider 'item
  "A slider widget for home-assistant dashboards.
Slider widgets are a little complicated because the useful value
can either be an entities' state or an attribute. For example,
light entities use the `brightness' attribute but a counter
entity just uses its' state.

Additionally, the user has the option to present the value in its
raw form or in a percentage form. Similar to before, it's usually
more useful to display a light as a percentage, but a counter is
probably more useful with its raw value displayed.

To account for these two abilities, when a slider widget's value
is retrieve, it firsts looks up what kind of value it wants,
value or percent. See function `hass-dash--slider-value-get'. The
respective value-type functions will then look up the domain of
the entity and fetch and format the result properly.

A percent value needs a minimum and maximum value. The way these
are derived per domain are defined with the
`hass-dash--slider-value-min' and `hass-dash--slider-value-max'
functions.

All slider properties:

• `:step': The amount to step by when adjusting the slider.

• `:value-type': When `raw', display raw number next to slider.
  Good for widgets like counters. When `percent', display the
  percentage between it's minimum and maximum value.  Good for
  lights. `percent' also changes `:step' to a percentage value "
  :convert-widget #'hass-dash--slider-convert
  :create #'hass-dash--widget-create
  :format "%[%t: %v%]\n"
  :value-get #'hass-dash--slider-value-get
  :action #'hass-dash--action)

(defun hass-dash--slider-convert (widget)
  "Initialize a slider widget."
  (let ((widget (hass-dash--widget-convert widget)))
    (pcase (hass--domain-of-entity (widget-get widget :entity-id))
      ("light"
       (unless (widget-get widget :value-type) (widget-put widget :value-type 'percent))
       (unless (widget-get widget :value-source) (widget-put widget :value-source '(attribute brightness))))
      ("counter"
       (unless (widget-get widget :value-type) (widget-put widget :value-type 'raw))
       (unless (widget-get widget :value-source) (widget-put widget :value-source 'state)))
      ("input_number"
       (unless (widget-get widget :value-type) (widget-put widget :value-type 'raw))
       (unless (widget-get widget :value-source) (widget-put widget :value-source 'state))))
    widget))

(defun hass-dash--slider-value-get (widget)
  "The main entry point for retrieving a sliders value."
  (pcase (or (widget-get widget :value-type)
             (alist-get (intern (hass--domain-of-entity
                                 (widget-get widget :entity-id)))
                        hass-dash-slider-value-types))
    ('percent (hass-dash--slider-value-percent widget))
    ('raw (hass-dash--slider-value-raw widget))))

(defun hass-dash--slider-value-percent (widget)
  "Return a percent value."
  (if-let* ((domain (hass--domain-of-entity
                     (widget-get widget :entity-id)))
            (min (hass-dash--slider-value-min widget))
            (max (hass-dash--slider-value-max widget))
            (value (hass-dash--slider-value-raw widget)))
      (format "%3d%%" (hass-dash--percent value min max))
    "N/A"))

(defun hass-dash--slider-value-min (widget)
  "Return the minimum possible value for WIDGET."
  (let ((entity-id (widget-get widget :entity-id)))
    (pcase (hass--domain-of-entity entity-id)
      ("light" 0.0)
      ("counter" (when-let ((min (hass-attribute-of entity-id 'minimum)))
                   (float min)))
      ("input_number" (hass-attribute-of entity-id 'min)))))

(defun hass-dash--slider-value-max (widget)
  "Return the maximum possible value for WIDGET."
  (let ((entity-id (widget-get widget :entity-id)))
    (pcase (hass--domain-of-entity entity-id)
      ("light" 255.0)
      ("counter" (when-let ((max (hass-attribute-of entity-id 'maximum)))
                   (float max)))
      ("input_number" (hass-attribute-of entity-id 'max)))))

(defun hass-dash--slider-value-raw (widget)
  "Return the raw value."
  (let ((value (hass-dash--value-state widget)))
    (or (if (stringp value)
            (string-to-number value)
          value)
        0)))

(defun hass-dash--slider-action-default (widget)
  "Return the default slider action for the domain of ENTITY-ID."
  (let ((domain (hass--domain-of-entity (widget-get widget :entity-id)))
        (value-type (widget-get widget :value-type)))
    (if (eq 'percent value-type)
        (pcase domain
          ;; percent value actions
          ("light" #'hass-dash--slider-action:light:percent)
          ("counter" #'hass-dash--slider-action:counter:percent)
          ("input_number" #'hass-dash--slider-action:input-number:percent)
          (_ (hass--message "Sliding by percent not supported for this widget type.") nil))
      (pcase domain
        ;; raw value actions
        ("light" #'hass-dash--slider-action:light:raw)
        ("counter" #'hass-dash--slider-action:counter:raw)
        ("input_number" #'hass-dash--slider-action:input-number:raw)
        (_ (hass--message "Sliding not supported for this widget type.") nil)))))

;;;;; Light
(defun hass-dash--slider-action:light:raw (entity-id step)
  "Adjust the brightness of a light entity."
  (hass-call-service-with-payload
   "light.turn_on"
   `((entity_id . ,entity-id)
     (brightness_step . ,step))))

(defun hass-dash--slider-action:light:percent (entity-id step_pct)
  "Adjust the brightness of a light entity."
  (hass-call-service-with-payload
   "light.turn_on"
   `((entity_id . ,entity-id)
     (brightness_step_pct . ,step_pct))))

;;;;; Counter
(defun hass-dash--slider-action:counter:raw (entity-id step)
  "Step a counter helper."
  (let ((amount (abs step)))
    (if (= amount (hass-attribute-of entity-id 'step))
        ; If the counter already has the correct step value, just move it.
        (hass-dash--slider-action:counter:adjust entity-id step)
      ; Otherwise, configure it first then move it.
      (hass-call-service-with-payload "counter.configure"
                                      `((entity_id . ,entity-id)
                                        (step . ,amount))
                                      (lambda (&rest _)
                                        (hass-dash--slider-action:counter:adjust entity-id step))))))

(defun hass-dash--slider-action:counter:percent (entity-id step-pct)
  "Step a counter helper a certain percentage."
  (when-let* ((minimum (hass-attribute-of entity-id 'minimum))
              (maximum (hass-attribute-of entity-id 'maximum))
              (step (+ (* (- maximum minimum) (/ (abs step-pct) 100.0)) minimum))
              (step (max 1 step)))
    (if (> step-pct 0)
        (hass-dash--slider-action:counter:raw entity-id step)
      (hass-dash--slider-action:counter:raw entity-id (* -1 step)))))

(defun hass-dash--slider-action:counter:adjust (entity-id step)
  (cond ((< step 0) (hass-call-service entity-id "counter.decrement"))
        ((> step 0) (hass-call-service entity-id "counter.increment"))))

;;;;; Input number
(defun hass-dash--slider-action:input-number:raw (entity-id step)
  "Step a input_number helper."
  (let ((amount (abs step)))
    (if (= amount (hass-attribute-of entity-id 'step))
        ; If the counter already has the correct step value, just move it.
        (hass-dash--slider-action:input-number:adjust entity-id step)
      ; Otherwise, we're going to use 'set_value' because Home Assistant doesn't
      ; offer away to configure the 'step' attribute for input_numbers.
      (hass-dash--slider-action:input-number:set-by-step entity-id step))))

(defun hass-dash--slider-action:input-number:percent (entity-id step-pct)
  "Step a input_number helper a certain percentage."
  (when-let* ((minimum (hass-attribute-of entity-id 'min))
              (maximum (hass-attribute-of entity-id 'max))
              (step (+ (* (- maximum minimum) (/ (abs step-pct) 100.0)) minimum))
              (step (max 1 step)))
    (if (> step-pct 0)
        (hass-dash--slider-action:input-number:set-by-step entity-id step)
      (hass-dash--slider-action:input-number:set-by-step entity-id (* -1 step)))))

(defun hass-dash--slider-action:input-number:set-by-step (entity-id step)
  (let* ((value (string-to-number (hass-state-of entity-id)))
         (value (+ value step))
         ;; clamp between min and max values
         (min (or (hass-attribute-of entity-id 'min) value))
         (max (or (hass-attribute-of entity-id 'max) value))
         (value (max min value))
         (value (min max value)))
    (hass-call-service-with-payload
     "input_number.set_value"
     `((entity_id . ,entity-id)
       (value . ,(round value))))))

(defun hass-dash--slider-action:input-number:adjust (entity-id step)
  (cond ((< step 0) (hass-call-service entity-id "input_number.decrement"))
        ((> step 0) (hass-call-service entity-id "input_number.increment"))))

;;;; Toggle widget
(define-widget 'hass-toggle 'toggle
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
  being confirmed.  See `hass-dash--action' for details."
  :convert-widget #'hass-dash--widget-convert
  :create #'hass-dash--widget-create
  :format "%[%t: %v%]\n"
  :value-get #'hass-dash--toggle-widget-value-get
  :action #'hass-dash--action)

(define-widget 'hass-dash-toggle 'hass-toggle
  "Replaced with `hass-toggle'.")

(defun hass-dash--toggle-widget-value-get (widget)
  "Set the state for a toggle WIDGET."
  (hass-switch-p (widget-get widget :entity-id)))

;;;; Group widget
(define-widget 'hass-group 'group
  "A grouping widget for home-assistant dashboards.
You can pass `:title' to give the group a title, and pass `:title-face' to set
the font face for the title."
  :convert-widget #'hass-dash--group-convert
  :format "%t\n%v"
  :value-create #'hass-dash--group-value-create
  :title-face 'hass-dash-group)

(define-widget 'hass-dash-group 'hass-group
  "Replaced with `hass-group'.")

(defun hass-dash--group-convert (widget)
  "Create the hass dashboard group WIDGET.
This just uses `widget-default-create', but sets the `:tag' property if it isn't
already set using the `:title' and `:title-face' properties."
  (unless (widget-get widget :tag)
    (widget-put widget :tag (propertize (widget-get widget :title)
                                        'face (widget-get widget :title-face))))
  widget)

(defun hass-dash--group-value-create (widget)
  "Insert the child widgets into the buffer."
  (widget-put widget :indent (+ hass-dash-group-indent
                                (or (widget-get widget :indent)
                                    0)))
  (widget-group-value-create widget))


;;; User Interface
(defun hass-dash--slider-adjust (scale)
  "Adjust the value of a slider widget at point.
SCALE is multiplied against the step value and is usually either
just -1 or 1 to affect slider move direction."
  (when-let* ((widget (widget-at))
              (_ (eq 'hass-slider (widget-type (widget-at))))
              (entity-id (widget-get widget :entity-id))
              (action (or (widget-get widget :slider)
                          (hass-dash--slider-action-default widget)))
              (step (or (widget-get widget :step)
                        1)))
    (funcall action entity-id (* step scale))))

(defun hass-dash-slider-increase (&optional step)
  "Increase the value of a slider widget at point."
  (interactive)
  (hass-dash--slider-adjust (or step 1)))

(defun hass-dash-slider-decrease (&optional step)
  "Decrease the value of a slider widget at point."
  (interactive)
  (hass-dash--slider-adjust (* -1 (or step 1))))

;;;###autoload
(defun hass-dash-load-layout (path)
  "Load dashboards from file at PATH.
The contents of this file will automatically be put into a list
and stored in `hass-dash-layouts'.

A new layout is defined by placing an unquoted name on the top
level. Following the name, the contents of the dashboard is read
and collected into a list.


The example below creates two dashboards named `my-lights' and
`my-fans' which each have a state and button widget:

  my-lights

  (hass-state :entity-id \"light.office\"
                   :format \"The light is %v\")
  (hass-button :entity-id \"light.office\"
                    :label \"Press me\")

  my-fans

  (hass-state :entity-id \"fan.bedroom\"
                   :format \"The fan is %v\")
  (hass-button :entity-id \"bedroom.bedroom\"
                    :label \"Press me\")"
  (interactive "f")
  (setq hass-dash-layouts
        (with-temp-buffer
          (insert "(\n")
          (insert-file-contents
           path)
          (goto-char (point-max))
          (insert "\n)")
          (goto-char (point-min))
          (hass--debug-clear-buffer)
          (let ((data (read (current-buffer)))
                result
                layout)
            (while data
              (while (symbolp (car data))
                (setq layout (list (pop data))))
              (while (consp (car data))
                (push (pop data) layout))
              (push (nreverse layout) result))
            result))))

;;;###autoload
(defun hass-dash-open (dashboard)
  "Open the hass-dash buffer for DASHBOARD."
  (interactive (list (pcase (length hass-dash-layouts)
                       (0 (hass--warning "You must configure some dashboards in `hass-dash-layouts'.") nil)
                       (1 (caar hass-dash-layouts))
                       (_ (intern (completing-read "Dashboard: " hass-dash-layouts))))))

  (let* ((buffer (get-buffer-create (funcall hass-dash-buffer-name-function dashboard)))
         (window (get-buffer-window buffer))
         (layout (cdr (assoc dashboard hass-dash-layouts))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (hass-dash-mode)
        (hass-dash--render layout)))
    (hass--update-tracked-entities)
    (pop-to-buffer buffer)
    (set-window-dedicated-p window t)))

(define-derived-mode hass-dash-mode special-mode "Home Assistant Dash"
  "Dashboard for Home Assistant."
  :group 'hass-dash
  :syntax-table nil
  :abbrev-table nil
  :interactive t
  ;; Refresh dashboard when entity state is updated
  (unless hass-mode (hass-mode 1))
  (setq-local hass-dash--widgets nil)
  (setq-local hass-dash--rendering nil)
  (add-hook 'hass-entity-updated-functions #'hass-dash--update-entity))

(provide 'hass-dash)

;;; hass-dash.el ends here
