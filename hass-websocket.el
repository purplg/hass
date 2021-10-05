;;; hass-websocket.el --- Communicate with Home Assistant over websockets -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1") (websocket "1.12"))
;; Author: Ben Whitley
;; SPDX-License-Identifier: MIT
;;; Commentary:

;; Homepage: https://github.com/purplg/hass

;;; Code:
(require 'json)
(require 'websocket)

(defvar hass-websocket-connected-hook #'hass-websocket--subscribe-to-state-changes
 "Hook called after successful authentication to websocket.")

(defvar hass-websocket-connection '()
  "Websocket connection info.")

(defvar hass-websocket--interactions '()
  "Number Websocket interactions to use for message IDs.")

(defvar hass-websocket--subscriptions '()
  "Events to listen for over websocket.")

(defun hass-websocket--handle-message (_websocket frame)
  "Route messages received from websocket."
  (let* ((content (json-read-from-string (websocket-frame-text frame)))
         (type (cdr (assoc 'type content))))
    (cond ((string= "auth_required" type)
           (hass-websocket--send
            `(("type" . "auth")
              ("access_token" . ,(hass--apikey)))))
          ((string= type "auth_ok")
           (message "hass: Connected to websocket")
           (run-hooks 'hass-websocket-connected-hook))
          ((string= type "auth_invalid")
           (user-error "hass: Failed to connect to websocket: %s" (cdr (assoc 'message content))))
          ((string= type "result")
           (message (if (cdr (assoc 'success content))
                     "hass: Success"
                     "hass: Error")))
          ((string= type "event")
           (hass-websocket--handle-event (cdr (assoc 'event content))))
          ((message "received unhandled frame: %S" (helpful--pretty-print (json-read-from-string content)))))))

(defun hass-websocket--handle-event (event)
  (let* ((event-type (cdr (assoc 'event_type event)))
         (data (cdr (assoc 'data event)))
         (entity-id (cdr (assoc 'entity_id data)))
         (old-state (cdr (assoc 'old_state data)))
         (new-state (cdr (assoc 'new_state data))))
    (message "hass: event-type: %s, entity-id: %s" event-type entity-id)))

(defun hass-websocket--subscribe-to-state-changes ()
  (hass-websocket--subscribe "state_changed"))

(defun hass-websocket--subscribe (event-type)
  (message "hass: Subscribing to [%s]: `%s'" hass-websocket--interactions event-type)
  (hass-websocket--send `(("id" . ,hass-websocket--interactions)
                          ("type" . "subscribe_events")
                          ("event_type" . ,event-type))))

(defun hass-websocket--send (message)
  "Send a message to the websocket.
MESSAGE is an alist to encoded into a JSON object."
  (when hass-websocket-connection
    (message "hass: Sending message to websocket: `%s'" message)
    (websocket-send-text hass-websocket-connection (json-encode message))
    (setq hass-websocket--interactions (1+ hass-websocket--interactions))))

;;;###autoload
(defun hass-websocket--connect ()
  (interactive)
  (setq hass-websocket-connection
    (websocket-open (format "%s://%s:8123/api/websocket" (if hass-insecure "ws" "wss") hass-host)
      :on-message #'hass-websocket--handle-message
      :on-open (lambda (_websocket) (setq hass-websocket--interactions 0))
      :on-close (lambda (_websocket) (setq hass-websocket-connection nil)))))

(defun hass-websocket--disconnect ()
  (interactive)
  (websocket-close hass-websocket-connection)
  (setq hass-websocket-connection nil)
  (message "hass: Disconnected from websocket"))

(defun hass-websocket-toggle ()
  (if hass-websocket-connection
    (hass-websocket--disconnect)
    (hass-websocket--connect)))

(provide 'hass-websocket)
