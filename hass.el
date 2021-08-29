;;; hass.el --- File description  -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "24.4") (request "0.3.3"))
;; Version: 0
;; Author: Ben Whitley
;;; Commentary:

;;; Code:
(require 'request)

(defgroup hass '()
  "Minor mode for hass."
  :group 'hass)

(defcustom hass-url nil
  "The URL of the Home Assistant instance.
For example, 'https://192.168.1.10:8123'"
  :group 'hass
  :type 'string)
(defcustom hass-entities '()
  "A list of tracked Home Assistant entities."
  :group 'hass
  :type '(string))
(defcustom hass-apikey nil
  "API key used for Home Assistant queries.
The key generated from the Home Assistant instance used to
authorize API requests"
  :group 'hass
  :type 'string)

(defvar hass--states '()
  "An alist of entity ids to their last queried states")
(defvar hass--entity-state-change-hook nil
 "Hook called after an entity state has been changed")
(defvar hass--user-agent "Emacs hass.el"
  "The user-agent sent in API requests to Home Assistant")


(defun hass--parse-apikey ()
  "If HASS-APIKEY is a lambda, execute it to get value.
Otherwise return HASS-APIKEY as is."
  (if (and (equal (type-of hass-apikey) 'cons)
           (equal (car hass-apikey) 'lambda))
      (funcall hass-apikey)
      hass-apikey))


(defun hass--entity-url (entity-id) 
  "Generate entity state endpoint URLs"
  (format "%s/%s/%s" hass-url "api/states" entity-id))

(defun hass--service-url (domain service) 
  "Generate service endpoint URL"
  (format "%s/api/services/%s/%s" hass-url domain service))


(defun hass--entity-state-result (entity-id state)
  "Callback when an entity state data is received from API."
  (setf (alist-get entity-id hass--states nil nil 'string-match-p) state)
  (run-hooks 'hass--entity-state-change-hook))

(defun hass--service-result (entity-id state)
  "Callback when a successful service request is received from API"
  (setf (alist-get entity-id hass--states nil nil 'string-match-p) state)
  (run-hooks 'hass--entity-state-change-hook))


(defun hass--query-entity-state (entity-id)
  "Retrieve the current state of ENTITY-ID from the Home Assistant server."
    (request (hass--entity-url entity-id)
       :sync nil
       :type "GET" 
       :headers `(("User-Agent" . hass--user-agent) 
                  ("Authorization" . ,(concat "Bearer " (hass--parse-apikey)))) 
       :parser 'json-read 
       :success (cl-function
                  (lambda (&key response &allow-other-keys)
                    (let ((data (request-response-data response))) 
                      (hass--entity-state-result entity-id (cdr (assoc 'state data)))))) 
       :error (cl-function
                (lambda (&rest args &key error-thrown &allow-other-keys) 
                  (message "Error: %S" error-thrown)))))

(defun hass--call-service (domain service entity-id)
  "Set the state of =entity-id` from the Home Assistant server"
    (request (hass--service-url domain service)
       :sync nil
       :type "POST" 
       :headers `(("User-Agent" . hass--user-agent) 
                  ("Authorization" . ,(concat "Bearer " (hass--parse-apikey))) 
                  ("Content-Type" . "application/json")) 
       :data (format "{\"entity_id\": \"%s\"}" entity-id)
       :parser 'json-read 
       :success (cl-function
                  (lambda (&key &allow-other-keys)
                    (message "Toggled %s" entity-id))) 
       :error (cl-function
                (lambda (&rest args &key error-thrown &allow-other-keys) 
                  (message "Error: %S" error-thrown)))))


(defun hass-switch-turn-on (switch-id)
  (hass--call-service "switch" "turn_on" switch-id))

(defun hass-switch-turn-off (switch-id)
  (hass--call-service "switch" "turn_off" switch-id))

(defun hass-switch-toggle (switch-id)
  (hass--call-service "switch" "toggle" switch-id))

(defun hass-boolean-turn-on (boolean-id)
  (hass--call-service "input_boolean" "turn_on" boolean-id))

(defun hass-boolean-turn-off (boolean-id)
  (hass--call-service "input_boolean" "turn_off" boolean-id))

(defun hass-boolean-toggle (boolean-id)
  (hass--call-service "input_boolean" "toggle" boolean-id))


(defun hass-query-all-entities ()
  (interactive)
  "Update the current state all of the registered entities."
  (dolist (entity hass-entities) 
    (hass--query-entity-state entity)))


(define-minor-mode hass-mode
  "Toggle hass-mode."
  :lighter nil
  :interactive t
  :group 'hass
  :after-hook
  (unless (equal (type-of (hass--parse-apikey)) 'string)
    (user-error "HASS-APIKEY must be set to use hass-mode."))
  :global t)
 
(provide 'hass)

;;; hass.el ends here
