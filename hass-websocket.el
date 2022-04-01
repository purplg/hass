;;; hass-websocket.el --- Communicate with Home Assistant over websockets -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "25.1") (hass "2.0.0") (websocket "1.12"))
;; Version: 1.0.0
;; Author: Ben Whitley
;; SPDX-License-Identifier: MIT
;; Homepage: https://github.com/purplg/hass

;;; Commentary:

;; This mode is an extension to the `hass' package to provide realtime updates via websockets to a
;; Home Assistant instance.

;; Alternatively, the `hass' package has a 'polling mode' that will periodically query the Home
;; Assistant instance to get state updates of the configured tracked entities.  This package enables
;; the use of websockets to get updates instantly.

;; --------------------
;; Configuration

;; First, `hass' must be configured properly.

;; Since we don't want Emacs keeping track of /every/ entity in Home Assistant, you must tell hass
;; which entities you want to track using `hass-polling-mode'.  `hass-polling-mode' takes a list of
;; strings of Home Assistant entity IDs.

;;  (setq hass-tracked-entities '("switch.bedroom_light" "switch.bedroom_fan"))

;; Then, you can use the function hook `hass-entity-state-updated-functions' react to changes in
;; Home Assistant.  The lambda added to `hass-entity-state-updated-functions' takes one argument.  A
;; string that contains the entity id that changed state.  You can get the current state of the
;; entity using `hass-state-of'.  For a switch entity or other entity that have an on/off state, it
;; can be conventient to use `hass-switch-p' which returns 't' if the entity's state is 'on',

;; This example changes Emacs' theme based on whether or not a light is on:

;;  (add-hook 'hass-entity-state-updated-functions
;;    (lambda (entity-id)
;;      (cond ((string= entity-id "switch.bedroom_light")
;;             (if (hass-switch-p entity-id)
;;               (set-theme 'doom-one-light)
;;               (set-theme 'doom-one))))))

;; NOTE: This function hook is only called when an entity ID listed in `hass-tracked-entities' is updated.

;; Lastly, enable websocket mode.  This intializes the websocket connect from Emacs to the Home
;; Assistant instance.

;;  (hass-websocket-mode t)

;;; Code:
(require 'hass)
(require 'json)
(require 'websocket)

;; User customizable
(defvar hass-websocket-mode-map (make-sparse-keymap)
  "Keymap for `hass-websocket-mode'.")

(defvar hass-websocket-connected-hook #'hass-websocket--subscribe-to-state-changes
 "Hook called after successful authentication to websocket.")

;; Internal state
(defvar hass-websocket--connection '()
  "Websocket connection info.")

(defvar hass-websocket--interactions '()
  "Number Websocket interactions to use for message IDs.")

;; Updates - Received from Home Assistant over websocket
(defun hass-websocket--handle-message (_websocket frame)
  "Route FRAME received from websocket."
  (let* ((content (hass--deserialize (websocket-frame-text frame)))
         (type (cdr (assoc 'type content))))
    (cond ((string= "auth_required" type)
           (hass-websocket--send
            `((type . "auth")
              (access_token . ,(hass--apikey)))))
          ((string= type "auth_ok")
           (message "hass: Connected to websocket")
           (run-hooks 'hass-websocket-connected-hook))
          ((string= type "auth_invalid")
           (user-error "hass: Failed to connect to websocket: %s" (cdr (assoc 'message content))))
          ((string= type "result")
           (message (unless (cdr (assoc 'success content)) "hass: Error")))
          ((string= type "event")
           (hass-websocket--handle-event (cdr (assoc 'event content)))))))

(defun hass-websocket--handle-event (event)
  "Handle a websocket message.
EVENT is the name of the event in Home Assistant that triggered."
  (let ((event-type (cdr (assoc 'event_type event)))
        (data (cdr (assoc 'data event))))
    (cond ((string= event-type "state_changed")
           (hass-websocket--handle-state-change data)))))

(defun hass-websocket--handle-state-change (data)
  "Handle a websocket message for the 'state_changed' event.
This event is only handled when the `entity-id' of this event is
in the `hass-tracked-entities' list.  Otherwise, this event is
ignored.

DATA is the data retrieved from an event that triggered in Home
Assistant."
  (let ((entity-id (cdr (assoc 'entity_id data))))
    (when (member entity-id hass-tracked-entities)
      (hass--query-entity-result
       entity-id
       (cdr (assoc 'state (cdr (assoc 'new_state data))))))))

;; Requests - Send to Home Assistant over websocket
(defun hass-websocket--subscribe-to-state-changes ()
  "Request 'state_changed' events be sent over the websocket connection."
  (hass-websocket--subscribe "state_changed"))

(defun hass-websocket--subscribe (event-type)
  "Wrapper function to subscribe to an event.
EVENT-TYPE is a string of event name to subscribe to"
  (hass-websocket--send `((id . ,hass-websocket--interactions)
                          (type . "subscribe_events")
                          (event_type . ,event-type))))

(defun hass-websocket--send (message)
  "Send a message to the websocket.
MESSAGE is an alist to be encoded into a JSON object."
  (websocket-send-text hass-websocket--connection (hass--serialize message))
  (setq hass-websocket--interactions (1+ hass-websocket--interactions)))

;; Mode toggle
(defun hass-websocket--connect ()
  "Establish a websocket connection to Home Assistant."
  (setq hass-websocket--connection
    (websocket-open (format "%s://%s:8123/api/websocket"
                            (if hass-insecure "ws" "wss")
                            hass-host)
      :on-message #'hass-websocket--handle-message
      :on-open (lambda (_websocket) (setq hass-websocket--interactions 0))
      :on-close (lambda (_websocket) (setq hass-websocket--connection nil)))))

(defun hass-websocket--disconnect ()
  "Disconnect the websocket connection to Home Assistant."
  (when hass-websocket--connection
    (websocket-close hass-websocket--connection)
    (setq hass-websocket--connection nil)
    (message "hass: Disconnected from websocket")))

;;;###autoload
(define-minor-mode hass-websocket-mode
  "Toggle mode for a websocket connection to Home Assistant.
Similar to `hass-polling-mode' but uses websocket to get realtime
updates from the Home Assistant instance.

Use the variable `hass-tracked-entities' to set which entities
you want to track state updates for."
  :lighter nil
  :group 'hass
  :global t
  (hass-websocket--disconnect)
  (when hass-websocket-mode
    (hass-websocket--connect)))

(provide 'hass-websocket)

;;; hass-websocket.el ends here
