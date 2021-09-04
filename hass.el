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
(defcustom hass-auto-query nil
  "Periodically query the state of the configured in HASS-ENTITIES. "
  :group 'hass
  :type 'boolean)
(defcustom hass-auto-query-frequency 60
  "Amount of seconds between auto-querying HASS-ENTITIES."
  :group 'hass
  :type 'integer)

(defvar hass-mode-map (make-sparse-keymap)
  "Keymap for hass mode.")
(defvar hass-entity-state-updated-functions nil
 "List of functions called when an entity state changes.

Each function is called with one arguments: the ENTITY-ID of the
entity whose state changed.")
(defvar hass-entity-state-updated-hook nil
 "Hook called after an entity state data was received.")
(defvar hass-service-called-hook nil
 "Hook called after a service has been called.")
(defvar hass--states '()
  "An alist of entity ids to their last queried states.")
(defvar hass--user-agent "Emacs hass.el"
  "The user-agent sent in API requests to Home Assistant.")
(defvar hass--supported-domains
  '("switch" "input_boolean")
  "List of supported domains.")
(defvar hass--timer nil
  "Stores a reference to the timer used to periodically update
entity state.")
(defvar hass--available-entities nil
  "The entities retrieved from the Home Assistant instance.")
(defvar hass--available-services nil
  "The servies retrieved from the Home Assistant instance.")

;; Helper functions
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

(defun hass--domain-of-entity (entity-id)
  "Convert an ENTITY-ID to its respective domain."
  (car (split-string entity-id "\\.")))

(defun hass--services-for-entity (entity-id)
  "Returns the services available for an ENTITY-ID."
  (cdr (assoc (hass--domain-of-entity entity-id) hass--available-services)))

(defun hass-state-of (entity-id)
  "Returns the last known state of ENTITY-ID."
  (cdr (assoc entity-id hass--states)))

;; API parsing
(defun hass--parse-all-entities (entities)
  "Convert entity state data into a list of available entities."
  (mapcar (lambda (entity)
            (hass--parse-entity entity))
          entities))

(defun hass--parse-entity (entity-state)
  "Convert an entity's state data into its entity-id."
  (cdr (car entity-state)))

(defun hass--parse-all-domains (domains)
  "Collect all domains into an alist of the domains to their
associated list of services."
  (mapcar #'hass--parse-domain domains))

(defun hass--parse-domain (domain)
  "Collect each domain into cons cell of the domain to its
available list of services."
  (cons (cdr (assoc 'domain domain))
        (hass--parse-services (cdr (assoc 'services domain)))))
  
(defun hass--parse-services (services)
  "Flattens the list of services return from /api/services endpoint
to just the service name."
  (mapcar #'(lambda (service)
              (car service))
          services))

;; Request Callbacks
(defun hass--get-entities-result (entities)
  "Callback when all entity states is received from API."
  (setq hass--available-entities (hass--parse-all-entities entities)))

(defun hass--get-available-services-result (domains)
  "Callback when all service information is received from API."
  (setq hass--available-services (hass--parse-all-domains domains)))

(defun hass--query-entity-result (entity-id state)
  "Callback when an entity state data is received from API."
  (let ((previous-state (hass-state-of entity-id)))
    (setf (alist-get entity-id hass--states nil nil 'string-match-p) state)
    (unless (equal previous-state state)
      (run-hook-with-args 'hass-entity-state-updated-functions entity-id)))
  (run-hooks 'hass-entity-state-updated-hook))

(defun hass--call-service-result (entity-id state)
  "Callback when a successful service request is received from API."
  (setf (alist-get entity-id hass--states nil nil 'string-match-p) state)
  (run-hooks 'hass-service-called-hook))

;; Requests
(cl-defun hass--request-error (&key error-thrown &allow-other-keys)
  "Error handler for invalid requests."
  (error "hass-mode: %S" error-thrown))

(defun hass--get-available-entities ()
  "Retrieve the available entities from the Home Assistant instance.

Makes a request to /api/states but drops everything except an
list of entity-ids."
  (request (concat hass-url "/api/states")
     :sync nil
     :type "GET"
     :headers `(("User-Agent" . hass--user-agent)
                ("Authorization" . ,(concat "Bearer " (hass--parse-apikey))))
     :parser 'json-read
     :error #'hass--request-error
     :success (cl-function
                (lambda (&key response &allow-other-keys)
                  (let ((data (request-response-data response)))
                    (hass--get-entities-result data))))))

(defun hass--get-available-services ()
  "Retrieve the available services from the Home Assistant instance."
  (request (concat hass-url "/api/services")
     :sync nil
     :type "GET"
     :headers `(("User-Agent" . hass--user-agent)
                ("Authorization" . ,(concat "Bearer " (hass--parse-apikey))))
     :parser 'json-read
     :error #'hass--request-error
     :success (cl-function
                (lambda (&key response &allow-other-keys)
                  (let ((data (request-response-data response)))
                    (hass--get-available-services-result data))))))

(defun hass--get-entity-state (entity-id)
  "Retrieve the current state of ENTITY-ID from the Home Assistant server.

This function is just for sending the actual API request."
  (request (hass--entity-url entity-id)
     :sync nil
     :type "GET"
     :headers `(("User-Agent" . hass--user-agent)
                ("Authorization" . ,(concat "Bearer " (hass--parse-apikey))))
     :parser 'json-read
     :error #'hass--request-error
     :success (cl-function
                (lambda (&key response &allow-other-keys)
                  (let ((data (request-response-data response)))
                    (hass--query-entity-result entity-id (cdr (assoc 'state data))))))))

(defun hass--call-service (domain service entity-id)
  "Call service SERVICE for ENTITY-ID on the Home Assistant server.

This function is just for building and sending the actual API request.

DOMAIN is a string for the domain in Home Assistant this service is apart of.

SERVICE is a string of the Home Assistance service in DOMAIN that is being called.

ENTITY-ID is a string of the entity_id in Home Assistant."
  (request (hass--service-url domain service)
     :sync nil
     :type "POST"
     :headers `(("User-Agent" . hass--user-agent)
                ("Authorization" . ,(concat "Bearer " (hass--parse-apikey)))
                ("Content-Type" . "application/json"))
     :data (format "{\"entity_id\": \"%s\"}" entity-id)
     :parser 'json-read
     :error #'hass--request-error
     :success (cl-function
                (lambda (&rest _)
                  (run-hooks 'hass-service-called-hook)
                  (hass--get-entity-state entity-id)))))

(defun hass-call-service (entity-id service)
  "Call service SERVICE for ENTITY-ID on the Home Assistant server.

If called interactively, prompt the user for an ENTITY-ID and
SERVICE to call.

This will send an API request to the url configure in HASS-URL. This function
requires both ENTITY-ID and SERVICE keyword arguments to e passed.

ENTITY-ID is a string of the entity id in Home Assistant you want to call the
service on. (e.g. `\"switch.kitchen_light\").

SERVICE is the service you want to call on ENTITY-ID. (e.g. \"turn_off\")"
  (interactive
    (let ((entity (completing-read "Entity: " hass--available-entities nil t)))
      (list entity
        (completing-read (format "%s: " entity)
                         (hass--services-for-entity entity) nil t))))
            
  (when (equal entity-id nil) (user-error "Missing ENTITY-ID"))
  (let ((domain (hass--domain-of-entity entity-id)))
    (unless (member domain hass--supported-domains)
      (user-error "%S is not a supported domain" domain))
    (hass--call-service domain service entity-id)))

;; Auto query
(defun hass-auto-query-toggle ()
  "Toggle querying Home Assistant periodically."
  (interactive)
  (if hass-auto-query
    (hass-auto-query-disable)
    (hass-auto-query-enable)))

(defun hass-auto-query-enable ()
  "Enable auto-query."
  (unless hass-mode
    (user-error "hass-mode must be enabled to use this feature."))
  (when hass--timer
    (hass--auto-query-cancel))
  (setq hass--timer
    (run-with-timer nil hass-auto-query-frequency 'hass-query-all-entities))
  (setq hass-auto-query t))

(defun hass-auto-query-disable ()
  "Disable auto-query."
  (hass--auto-query-cancel)
  (setq hass-auto-query nil))

(defun hass-query-all-entities ()
  "Update the state of all entities."
  (interactive)
  "Update the current state all of the registered entities."
  (dolist (entity hass-entities)
    (hass--get-entity-state entity)))

(defun hass--auto-query-cancel ()
  "Cancel auto-query without disabling it."
  (when hass--timer
    (cancel-timer hass--timer)
    (setq hass--timer nil)))

;;;###autoload
(define-minor-mode hass-mode
  "Toggle hass-mode.

Key bindings:
\\{hass-mode-map}"
  :lighter nil
  :interactive t
  :group 'hass
  :global t
  (when hass-mode
      (unless (equal (type-of (hass--parse-apikey)) 'string)
          (user-error "HASS-APIKEY must be set to use hass-mode."))
      (unless (equal (type-of hass-url) 'string)
          (user-error "HASS-URL must be set to use hass-mode."))
      (when hass-auto-query
        (hass-auto-query-enable))
      (hass--get-available-entities)
      (hass--get-available-services))
  (unless hass-mode
    (hass--auto-query-cancel)))

(provide 'hass)

;;; hass.el ends here
