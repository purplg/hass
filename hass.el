;;; hass.el --- Interact with Home Assistant. -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "24.4") (request "0.3.3"))
;; Version: 0.1
;; Author: Ben Whitley
;;; Commentary:

;;; Code:
(require 'request)

(defgroup hass '()
  "Minor mode for hass."
  :group 'hass
  :prefix "hass-")

(defcustom hass-url nil
  "The URL of the Home Assistant instance.

Set this to the URL of the Home Assistant instance you want to
control. (e.g. https://192.168.1.10:8123)"
  :group 'hass
  :type 'string)
(defcustom hass-entities nil
  "A list of tracked Home Assistant entities.

Set this to a list of Home Assistant entity ID strings. An entity ID looks
something like *switch.bedroom_light*."

  :group 'hass
  :type '(repeat string))
(defcustom hass-apikey nil
  "API key used for Home Assistant queries.

The key generated from the Home Assistant instance used to authorize API
requests"
  :group 'hass
  :type 'string)

(defvar hass-entity-state-change-hook nil
 "Hook called after an entity state has been changed.")
(defvar hass-service-called-hook nil
 "Hook called after a service has been called.")
(defvar hass--states '()
  "An alist of entity ids to their last queried states.")
(defvar hass--user-agent "Emacs hass.el"
  "The user-agent sent in API requests to Home Assistant.")
(defvar hass--supported-domains
  '("switch" "input_boolean")
  "List of supported domains.")
(defvar hass--services '((toggle . "toggle")
                         (turn-on . "turn_on") 
                         (turn-off . "turn_off"))
  "Map of services to their corresponding strings.")


(defun hass--parse-apikey ()
  "Returns the effective apikey.

If HASS-APIKEY is a lambda, execute it to get value. Otherwise return
HASS-APIKEY as is."
  (if (and (equal (type-of hass-apikey) 'cons)
           (equal (car hass-apikey) 'lambda))
      (funcall hass-apikey)
      hass-apikey))


(defun hass--entity-url (entity-id) 
  "Generate entity state endpoint URLs."
  (format "%s/%s/%s" hass-url "api/states" entity-id))

(defun hass--service-url (domain service) 
  "Generate service endpoint URL."
  (format "%s/api/services/%s/%s" hass-url domain service))


(defun hass--entity-state-result (entity-id state)
  "Callback when an entity state data is received from API."
  (setf (alist-get entity-id hass--states nil nil 'string-match-p) state)
  (run-hooks 'hass-entity-state-change-hook))

(defun hass--service-result (entity-id state)
  "Callback when a successful service request is received from API."
  (setf (alist-get entity-id hass--states nil nil 'string-match-p) state)
  (run-hooks 'hass-service-called-hook))


(defun hass--query-entity-state (entity-id)
 "Retrieve the current state of ENTITY-ID from the Home Assistant server.

This function is just for sending the actual API request."
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
  "Call service SERVICE for ENTITY-ID on the Home Assistant server.

This function is just for sending the actual API request."
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
                    (run-hooks 'hass-service-called-hook) 
                    (hass--query-entity-state entity-id)))
       :error (cl-function
                (lambda (&rest args &key error-thrown &allow-other-keys) 
                  (message "Error: %S" error-thrown)))))
   

(cl-defun hass-call-service (&key entity-id service) 
  "Call service SERVICE for ENTITY-ID on the Home Assistant server.

This will send an API request to the url configure in HASS-URL. This function
requires both ENTITY-ID and SERVICE keyword arguments to e passed.

ENTITY-ID is a string of the entity id in Home Assistant you want to call the
service on. (e.g. `\"switch.kitchen_light\").

SERVICE is the service you want to call on ENTITY-ID. (e.g. 'turn-off)"
  (when (equal entity-id nil) (user-error "Missing ENTITY-ID"))
  (let ((domain (car (split-string entity-id "\\."))))
    (unless (member domain hass--supported-domains)
      (user-error "%S is not a supported domain" domain))
    (hass--call-service domain (alist-get service hass--services) entity-id)))
    

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
  (unless (equal (type-of hass-url) 'string)
    (user-error "HASS-URL must be set to use hass-mode."))
  :global t)
 
(provide 'hass)

;;; hass.el ends here
