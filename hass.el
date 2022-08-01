;;; hass.el --- Interact with Home Assistant -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "25.1") (request "0.3.3"))
;; Version: 2.2.2
;; Author: Ben Whitley
;; Homepage: https://github.com/purplg/hass
;; SPDX-License-Identifier: MIT
;;; Commentary:

;; This mode enables interaction with a Home Assistant instance from within Emacs.

;; --------------------
;; Configuration

;; Both `hass-host' and `hass-apikey' must be set to use this package.

;; (setq hass-host "192.168.1.10") ; Required
;; (setq hass-insecure t) ; If using HTTP and not HTTPS
;; (setq hass-port 8123) ; If using a different port other than the default 8123
;; (setq hass-apikey "APIKEY-GOES-IN-HERE") ; Required.  See below.
;; (hass-setup)

;; Getting an API Key:
;; Ensure that your Home Assistant instance is configured to support API calls by following the
;; instructions here: `https://www.home-assistant.io/integrations/api/'.

;; Retrieve your API key a.k.a. /Long-Lived Access Token/ by logging into your Home Assistant
;; instance and going to your profile by selecting your username in the lower-left corner or going
;; to this URL: `http://HOME-ASSISTANT-URL:8123/profile'.  You can generate an API token at the very
;; bottom of this page.

;; --------------------
;; Usage

;; Use `hass-call-service' to make service calls on the configured Home Assistant instance:

;; (hass-call-service "switch.bedroom_light" "switch.toggle")

;; Or use `hass-call-service-with-payload' to customize the payload:
;; (hass-call-service-with-payload
;;  "mqtt.publish"
;;  (json-encode '(("payload" . "PERFORM")
;;                 ("topic" . "valetudo/vacuum/LocateCapability/locate/set"))))

;; See README.org for more information.

;;; Code:
(require 'json)
(require 'request)

(defgroup hass '()
  "Minor mode for hass."
  :group 'hass
  :prefix "hass-")


;; Customizable
(defcustom hass-host nil
  "The URL of the Home Assistant instance.
Set this to the URL of the Home Assistant instance you want to
control.  (e.g. 192.168.1.10)"
  :group 'hass
  :type 'string)

(defcustom hass-port 8123
  "The port used for Home Assistant requests."
  :group 'hass
  :type 'integer)

(defcustom hass-insecure nil
  "Whether to use HTTP or HTTPS.
When Set to non-nil, use HTTP instead of HTTPS when making requests."
  :group 'hass
  :type 'boolean)

(defcustom hass-apikey nil
  "API key used for Home Assistant queries.
The key generated from the Home Assistant instance used to authorize API
requests"
  :group 'hass
  :type 'string)

(defcustom hass-icons '(("default" . "faicon:cog")
                        ("automation" . "faicon:bolt")
                        ("switch" . "faicon:toggle-on")
                        ("input_boolean" . "faicon:toggle-on")
                        ("vacuum" ."fileicon:robot"))
  "An alist of entity domains to icons to be used."
  :group 'hass
  :type '(repeat (cons symbol string)))

(defcustom hass-tracked-entities nil
  "A list of tracked Home Assistant entities.
Set this to a list of Home Assistant entity ID strings.  An entity ID looks
something like *switch.bedroom_light*.

This is used by `hass-polling-mode' and `hass-websocket-mode' to
detect changes in entity state."
  :group 'hass
  :type '(repeat string))

(defvar hass-polling-mode-map (make-sparse-keymap)
  "Keymap for `hass-polling-mode'.")

(defcustom hass-polling-frequency 60
  "Amount of seconds between querying HASS-ENTITIES."
  :group 'hass
  :type 'integer)

(defface hass-icon-face
  '((t (:inherit all-the-icons-lsilver)))
  "Face for widgets in HASS's dashboard.")


;; Hooks
(defvar hass-entity-state-changed-functions nil
  "List of functions called when an entity state changes.
Each function is called with one argument: the ENTITY-ID of the
entity whose state changed.")

(defvar hass-entity-updated-hook nil
  "Hook called when any entity information is updated.")

(defvar hass-api-connected-hook nil
  "Hook called after a successful Home Assistant API connection check is made.")

(defvar hass-service-called-hook nil
  "Hook called after a service has been called.")


;; Internal state
(defvar hass--states (make-hash-table :test 'equal)
  "A hashtable of entity ids to their last queried states.")

(defvar hass--user-agent "Emacs hass.el"
  "The user-agent sent in API requests to Home Assistant.")

(defvar hass--timer nil
  "Stores a reference to the timer used to periodically update entity state.")

(defvar hass--available-entities nil
  "The entities retrieved from the Home Assistant instance.")

(defvar hass--available-services nil
  "The services retrieved from the Home Assistant instance.")

(defvar hass--api-running nil
  "Whether a successful connection to Home Assistant API has been made.")


;; Helper functions
(defun hass--url (&optional path)
  "Formats a Home Assistant API request path to its' full URL.
PATH is the Home Assistant endpoint path."
  (format "%s://%s:%s/%s"
          (if hass-insecure "http" "https")
          hass-host
          hass-port
          path))

(defun hass--apikey ()
  "Return the effective apikey.
If HASS-APIKEY is a function, execute it to get value.  Otherwise
return HASS-APIKEY as is."
  (if (functionp hass-apikey)
      (funcall hass-apikey)
    hass-apikey))

(defun hass--entity-url (entity-id)
  "Generate entity state endpoint URLs.
ENTITY-ID is the id of the entity in Home Assistant."
  (hass--url (concat "api/states/" entity-id)))

(defun hass--service-url (service)
  "Generate service endpoint URL.
SERVICE is a string of the service to call."
  (let* ((parts (split-string service "\\."))
         (domain (pop parts))
         (service (pop parts)))
    (hass--url (format "api/services/%s/%s" domain service))))

(defun hass--domain-of-entity (entity-id)
  "Convert an ENTITY-ID to its respective domain."
  (car (split-string entity-id "\\.")))

(defun hass--services-for-entity (entity-id)
  "Return the services available for an ENTITY-ID."
  (cdr (assoc (hass--domain-of-entity entity-id) hass--available-services)))

(defun hass--deserialize (str-object)
  "Wrapper function to use native JSON parser when available.
STR-OBJECT is a JSON object in as a string to be deserialzied
into a JSON object."
  (if (version<= "27.1" emacs-version)
    (json-parse-string str-object :object-type 'alist)
    (json-read-from-string str-object)))

(defun hass--serialize (object)
  "Wrapper function to use native JSON serializer when available.
OBJECT is a JSON object to be serialized into string."
  (if (version<= "27.1" emacs-version)
    (json-serialize object)
    (json-encode object)))

(defun hass--icon-of-entity (entity-id)
  "Get the default icon of an entity.
ENTITY-ID is the id of the entity in Home Assistant."
  (when (require 'all-the-icons nil 'noerror)
    (let ((parts (split-string (or (cdr (assoc (hass--domain-of-entity entity-id) hass-icons))
                                   (cdr (assoc "default" hass-icons)))
                               ":")))
      (funcall (intern (concat "all-the-icons-" (pop parts))) (pop parts) :face 'hass-icon-face))))

(defun hass--set-state (entity-id state)
  "Set the state of an entity.
ENTITY-ID is the id of the entity in Home Assistant.

STATE is a string of the state of ENTITY-ID in Home Assistant."
  (puthash entity-id state hass--states))

(defun hass-state-of (entity-id)
  "Return the last known state of ENTITY-ID.
ENTITY-ID is the id of the entity in Home Assistant."
  (gethash entity-id hass--states))

(defun hass-switch-p (entity-id)
  "Return t if switch status is 'on' of ENTITY-ID.
ENTITY-ID is the id of the entity in Home Assistant."
  (string= (hass-state-of entity-id) "on"))

(defun hass-friendly-name (entity-id)
  "Get the friendly name of an entity.
ENTITY-ID is the id of the entity in Home Assistant."
  (plist-get
   (cdr (assoc entity-id hass--available-entities))
   ':friendly_name))


;; API parsing
(defun hass--parse-all-entities (entities)
  "Convert entity state data into a list of available entities.
ENTITIES is the data returned from the `/api/states' endpoint."
  (delq nil (mapcar (lambda (entity) (hass--parse-entity entity))
                    entities)))

(defun hass--parse-entity (entity-state)
  "Convert an entity's state data into its entity-id.
ENTITY-STATE is an individual entity state data return from the
`/api/states' endpoint.

Filters out entities that do not have callable services available."
  (let* ((entity-id (cdr (assoc 'entity_id entity-state)))
         (friendly-name (or (cdr (assoc 'friendly_name (cdr (assoc 'attributes entity-state))))
                            entity-id)))
    (when (hass--services-for-entity entity-id)
      `(,entity-id . (:friendly_name ,friendly-name
                                     :icon ,(hass--icon-of-entity entity-id))))))

(defun hass--parse-all-domains (domains)
  "Collect DOMAINS into an alist of their associated services.
DOMAINS is the data returned from the `/api/services' endpoint."
  (mapcar #'hass--parse-domain domains))

(defun hass--parse-domain (domain)
  "Convert DOMAIN into cons cell of its available list of services.
DOMAIN is a single domain return from the `/api/services'
endpoint."
  (cons (cdr (assoc 'domain domain))
        (hass--parse-services (cdr (assoc 'services domain)))))

(defun hass--parse-services (services)
  "Flattens the SERVICES return from `/api/services'."
  (mapcar (lambda (service) (car service))
          services))


;; Request Callbacks
(defun hass--get-entities-result (entities)
  "Callback when states of all ENTITIES is received from API."
  (setq hass--available-entities (hass--parse-all-entities entities))
  (run-hooks 'hass-entity-updated-hook))

(defun hass--get-available-services-result (domains)
  "Callback when all service information is received from API.
DOMAINS is the response from the `/api/services' endpoint which
returns a list of domains and their available services."
  (setq hass--available-services (hass--parse-all-domains domains)))

(defun hass--query-entity-result (entity-id state)
  "Callback when an entity state data is received from API.
ENTITY-ID is the id of the entity in Home Assistant that has state STATE."
  (let ((previous-state (hass-state-of entity-id)))
    (unless (equal previous-state state)
      (hass--set-state entity-id state)
      (run-hook-with-args 'hass-entity-state-changed-functions entity-id)
      (run-hooks 'hass-entity-updated-hook))))

(defun hass--call-service-result (entity-id state)
  "Callback when a successful service request is received from API.
ENTITY-ID is the id of the entity in Home Assistant that was
affected and now has STATE."
  (hass--set-state entity-id state)
  (run-hooks 'hass-service-called-hook))

(cl-defun hass--request-error (&key error-thrown &allow-other-keys)
  "Error handler for invalid requests.
ERROR-THROWN is the error thrown from the request.el request."
  (let ((error (replace-regexp-in-string "\n$" "" (cdr error-thrown))))
    (cond ((string= error "exited abnormally with code 7\n")
           (user-error "Hass: No Home Assistant instance detected at url: %s" hass-host))
          ((string= error "exited abnormally with code 35\n")
           (user-error "Hass: Error connecting to url `%s'? Try toggling variable `hass-insecure'" (hass--url)))
          ((error "Hass: unknown error: %S" error)))))


;; Requests
(defun hass--request (type url &optional success payload)
  "Function to reduce a lot of boilerplate when making a request.
TYPE is a string of the type of request to make.  For example, `\"GET\"'.

URL is a string of URL of the request.

SUCCESS is a callback function for when the request successful
completed.

PAYLOAD is contents the body of the request."
  (request url
    :sync nil
    :type type
    :headers `(("User-Agent" . hass--user-agent)
               ("Authorization" . ,(concat "Bearer " (hass--apikey)))
               ("Content-Type" . "application/json"))
    :data payload
    :parser (lambda () (hass--deserialize (buffer-string)))
    :error #'hass--request-error
    :success (when success
               (cl-function
                (lambda (&key response &allow-other-keys)
                  (let ((data (request-response-data response)))
                    (funcall success data)))))))

(defun hass--check-api-connection ()
  "Set `hass--api-running' to t when a successful connection is made."
  (setq hass--api-running nil)
  (hass--request "GET" (hass--url "api/")
                 (lambda (data)
                   (when (string= "API running." (cdr (assoc 'message data)))
                     (setq hass--api-running t)
                     (run-hooks 'hass-api-connected-hook)))))

(defun hass--get-available-entities (&optional callback)
  "Retrieve the available entities from the Home Assistant instance.
Makes a request to `/api/states' but drops everything except an
list of entity-ids.
Optional argument CALLBACK ran after entities are received."
  (hass--request "GET" (hass--url "api/states")
                 (lambda (data)
                   (hass--get-entities-result data)
                   (when callback (funcall callback)))))

(defun hass--get-available-services (&optional callback)
  "Retrieve the available services from the Home Assistant instance.
Optional argument CALLBACK ran after services are received."
  (hass--request "GET" (hass--url "api/services")
                 (lambda (data)
                   (hass--get-available-services-result data)
                   (when callback (funcall callback)))))

(defun hass--get-entity-state (entity-id)
  "Retrieve the current state of ENTITY-ID from the Home Assistant server."
  (hass--request "GET" (hass--entity-url entity-id)
                 (lambda (data)
                   (hass--query-entity-result entity-id (cdr (assoc 'state data))))))

(defun hass--update-all-entities ()
  "Update current state of tracked entities."
  (dolist (entity hass-tracked-entities)
    (hass--get-entity-state entity)))

(defun hass--call-service (service payload &optional success-callback)
  "Call service SERVICE for ENTITY-ID on the Home Assistant server.

SERVICE is a string of the Home Assistant service to be called.

PAYLOAD is a JSON-encoded string of the payload to be sent with SERVICE.

SUCCESS-CALLBACK is a function to be called with a successful request response."
  (hass--request "POST"
                 (hass--service-url service)
                 success-callback
                 payload))

(cl-defun hass-call-service (entity-id service &optional (update t))
  "Call service for an entity on Home Assistant.
If called interactively, prompt the user for an ENTITY-ID and
SERVICE to call.

This will send an API request to the address configure in `hass-host'.

ENTITY-ID is the id of the entity in Home Assistant.
to call the service on.  (e.g. `\"switch.kitchen_light\"').

SERVICE is the service you want to call on ENTITY-ID.  (e.g. `\"turn_off\"')"
  (interactive
   (let ((entity (completing-read "Entity: " hass--available-entities nil t)))
     (list entity
           (format "%s.%s"
                   (hass--domain-of-entity entity)
                   (completing-read (format "%s: " entity) (hass--services-for-entity entity) nil t)))))
  (hass-call-service-with-payload
   service
   (format "{\"entity_id\": \"%s\"}" entity-id)
   (when update (lambda (&rest _) (hass--get-entity-state entity-id)))))

(defun hass-call-service-with-payload (service payload &optional success-callback)
  "Call service with a custom payload on Home Assistant.
This will send an API request to the address configure in `hass-host'.

SERVICE is a string of the Home Assistant service to be called.

PAYLOAD is a JSON-encoded string of the payload to be sent with SERVICE.

SUCCESS-CALLBACK is a function to be called with a successful request response."
  (hass--call-service
   service
   payload
   (lambda (&rest _)
     (run-hooks 'hass-service-called-hook)
     (when success-callback (funcall success-callback)))))


;; Polling
(defun hass-polling--cancel-timer ()
  "Cancel polling without disabling it."
  (when hass--timer
    (cancel-timer hass--timer)
    (setq hass--timer nil)))

(defun hass-polling--query-entities ()
  "Update the current state all of the registered entities."
  (dolist (entity hass-tracked-entities)
    (hass--get-entity-state entity)))

;;;###autoload
(define-minor-mode hass-polling-mode
  "Toggle mode for querying Home Assistant periodically.
Similar to `hass-realtime-mode' but is done without requiring
websockets by periodically polling the API.

Use the variable `hass-polling-frequency' to change how
frequently (in seconds) the Home Assistant instance should be
queried.

Use the variable `hass-tracked-entities' to set which entities
you want to query automatically."
  :lighter nil
  :group 'hass
  :global t
  (when hass--timer (hass-polling--cancel-timer))
  (when hass-polling-mode
    (hass--get-available-services 'hass--get-available-entities)
    (when hass--timer (hass-polling--cancel-timer))
    (setq hass--timer (run-with-timer
                       nil
                       hass-polling-frequency
                       'hass-polling--query-entities))))


;;;###autoload
(defun hass-setup ()
  "Run before using any hass features.
Check whether necessary variables are set and then query the Home
Assistant instance for available services and entities."

  ;; Backwards compability. Split `hass-url', into appropriate variables.
  (when (boundp 'hass-url)
    (message "hass: `hass-url' is deprecated as of v2.0. Please use `hass-host'. https://github.com/purplg/hass/blob/master/README.org#deprecated-hass-url")
    (save-match-data
      (string-match "http\\(s?\\)://\\(.*\\):\\([0-9]*\\)$" hass-url)
      (setq hass-insecure (string-empty-p (match-string 1 hass-url)))
      (setq hass-host (match-string 2 hass-url))
      (setq hass-port (match-string 3 hass-url))))

  (cond ((not (equal (type-of (hass--apikey)) 'string))
         (user-error "HASS-APIKEY must be set to use hass"))
        ((not (equal (type-of hass-host) 'string))
         (user-error "HASS-HOST must be set to use hass")))
  
  (add-hook 'hass-api-connected-hook
            (lambda ()
              (hass--get-available-services #'hass--get-available-entities)))

  (hass--check-api-connection))

(provide 'hass)

;;; hass.el ends here
