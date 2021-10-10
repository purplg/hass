;;; hass-websocket.el --- Communicate with Home Assistant over websockets -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "25.1") (websocket "1.12"))
;; Author: Ben Whitley
;; SPDX-License-Identifier: MIT
;;; Commentary:

;; Homepage: https://github.com/purplg/hass

;;; Code:
(require 'hass)
(require 'json)
(require 'websocket)


;; Customizable
(defvar hass-realtime-mode-map (make-sparse-keymap)
  "Keymap for hass-realtime-mode.")

(defvar hass-websocket-connected-hook #'hass-websocket--subscribe-to-state-changes
 "Hook called after successful authentication to websocket.")


;; Internal state
(defvar hass-websocket-connection '()
  "Websocket connection info.")

(defvar hass-websocket--interactions '()
  "Number Websocket interactions to use for message IDs.")


;; Updates - Received from Home Assistant over websocket
(defun hass-websocket--handle-message (_websocket frame)
  "Route messages received from websocket."
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
           (message (if (cdr (assoc 'success content)) "hass: Success" "hass: Error")))
          ((string= type "event")
           (hass-websocket--handle-event (cdr (assoc 'event content))))
          ((message "received unhandled frame: %S" (helpful--pretty-print content))))))

(defun hass-websocket--handle-event (event)
  "Handle a websocket message with the type 'event'."
  (let ((event-type (cdr (assoc 'event_type event)))
        (data (cdr (assoc 'data event))))
    (cond ((string= event-type "state_changed")
           (hass-websocket--handle-state-change data))
          ((message "hass: unhandled event-type fired: %s" event-type)))))

(defun hass-websocket--handle-state-change (data)
  "Handle a websocket message for the 'state_changed' event.
This event is only handled when the `entity-id' of this event is
in the `hass-watched-entities' list. Otherwise, this event is
ignored."
  (let ((entity-id (cdr (assoc 'entity_id data))))
    (when (member entity-id hass-watched-entities)
      (hass--query-entity-result
       entity-id
       (cdr (assoc 'state (cdr (assoc 'new_state data))))))))


;; Requests - Send to Home Assistant over websocket
(defun hass-websocket--subscribe-to-state-changes ()
  "Request 'state_changed' events be sent over the websocket
connection."
  (hass-websocket--subscribe "state_changed"))

(defun hass-websocket--subscribe (event-type)
  (message "hass: Subscribing to [%s]: `%s'" hass-websocket--interactions event-type)
  (hass-websocket--send `((id . ,hass-websocket--interactions)
                          (type . "subscribe_events")
                          (event_type . ,event-type))))

(defun hass-websocket--send (message)
  "Send a message to the websocket.
MESSAGE is an alist to be encoded into a JSON object."
  (message "hass: Sending message to websocket: `%S'" message)
  (websocket-send-text hass-websocket-connection (hass--serialize message))
  (setq hass-websocket--interactions (1+ hass-websocket--interactions)))


;; Manage connection
;;;###autoload
(defun hass-websocket--connect ()
  "Establish a websocket connection to Home Assistant."
  (setq hass-websocket-connection
    (websocket-open (format "%s://%s:8123/api/websocket"
                            (if hass-insecure "ws" "wss")
                            hass-host)
      :on-message #'hass-websocket--handle-message
      :on-open (lambda (_websocket) (setq hass-websocket--interactions 0))
      :on-close (lambda (_websocket) (setq hass-websocket-connection nil)))))

(defun hass-websocket--disconnect ()
  "Disconnect the websocket connection to Home Assistant."
  (when hass-websocket-connection
    (websocket-close hass-websocket-connection)
    (setq hass-websocket-connection nil)
    (message "hass: Disconnected from websocket")))

(defun hass-websocket--reconnect ()
  "Disconnect and reconnect the websocket connection to Home Assistant."
  (hass-websocket--disconnect)
  (hass-websocket--connect))


;; Mode toggle
(define-minor-mode hass-realtime-mode
  "Toggle mode for a websocket connection to Home Assistant.
Similar to `hass-polling-mode' but uses websocket to get realtime
updated from the Home Assistant instance.

Use the variable `hass-watched-entities' to set which entities
you want to track state updates for."
  :lighter nil
  :group 'hass
  :global t
  (if hass-realtime-mode
    (hass-websocket--reconnect)
    (hass-websocket--disconnect)))

(provide 'hass-websocket)
