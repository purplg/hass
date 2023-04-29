;;; hass.el --- Interact with Home Assistant -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "25.1") (request "0.3.3") (websocket "1.13"))
;; Version: 3.0.2
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
;;  '((payload . "PERFORM")
;;    ("topic" . "valetudo/vacuum/LocateCapability/locate/set")))

;; See README.org for more information.

;;; Code:
(require 'json)
(require 'request)


;;; Customizable
(defgroup hass '()
  "Minor mode for hass."
  :group 'applications
  :prefix "hass-")

(defvar hass-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `hass-mode'.")

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
                        ("light" . "faicon:lightbulb-o")
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

This is used by `hass-websocket-mode' to detect changes in entity
state."
  :group 'hass
  :type '(repeat string))

(defface hass-icon-face
  '((t (:inherit all-the-icons-lsilver)))
  "Face for widgets in HASS's dashboard.")


;;; Hooks
(defvar hass-entity-state-changed-functions nil
  "List of functions called when an entity state changes.
Each function is called with one argument: the ENTITY-ID of the
entity whose state changed.")

(defvar hass-entity-updated-hook nil
  "Hook called when any entity information is updated.")

(defvar hass-api-connected-hook (lambda ()
                                  (hass--get-available-services #'hass--get-available-entities))
  "Hook called after a successful Home Assistant API connection check is made.")

(defvar hass-service-called-hook nil
  "Hook called after a service has been called.")


;;; Internal state
(defvar hass--states (make-hash-table :test 'equal)
  "A hashtable of entity ids to their last queried states.")

(defvar hass--user-agent "Emacs hass.el"
  "The user-agent sent in API requests to Home Assistant.")

(defvar hass--available-entities nil
  "The entities retrieved from the Home Assistant instance.")

(defvar hass--available-services nil
  "The services retrieved from the Home Assistant instance.")

(defvar hass--api-running nil
  "Whether a successful connection to Home Assistant API has been made.")


;;; Helper functions
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

(defun hass--entity-endpoint (entity-id)
  "Generate entity state endpoint URLs.
ENTITY-ID is the id of the entity in Home Assistant."
  (concat "api/states/" entity-id))

(defun hass--service-endpoint (service)
  "Generate service endpoint URL.
SERVICE is a string of the service to call."
  (let* ((parts (split-string service "\\."))
         (domain (pop parts))
         (service (pop parts)))
    (format "api/services/%s/%s" domain service)))

(defun hass--domain-of-entity (entity-id)
  "Convert an ENTITY-ID to its respective domain."
  (car (split-string entity-id "\\.")))

(defun hass--services-for-entity (entity-id)
  "Return the services available for an ENTITY-ID."
  (cdr (assoc (hass--domain-of-entity entity-id) hass--available-services)))

(if (version<= "27.1" emacs-version)
    (defun hass--deserialize (str-object)
      "Wrapper function to use native JSON parser when available.
STR-OBJECT is a JSON object in as a string to be deserialzied
into a JSON object."
      (json-parse-string str-object :object-type 'alist))
  (defun hass--deserialize (str-object)
    "Wrapper function to use native JSON parser when available.
STR-OBJECT is a JSON object in as a string to be deserialzied
into a JSON object."
    (json-read-from-string str-object)))

(if (version<= "27.1" emacs-version)
    (defun hass--serialize (object)
      "Wrapper function to use native JSON serializer when available.
OBJECT is a JSON object to be serialized into string."
      (json-serialize object))
  (defun hass--serialize (object)
    "Wrapper function to use native JSON serializer when available.
OBJECT is a JSON object to be serialized into string."
    (json-encode object)))

(defun hass--icon-of-entity (entity-id)
  "Get the default icon of an entity.
ENTITY-ID is the id of the entity in Home Assistant."
  (when (require 'all-the-icons nil 'noerror)
    (let ((parts (split-string (or (cdr (assoc (hass--domain-of-entity entity-id) hass-icons))
                                   (cdr (assoc "default" hass-icons)))
                               ":")))
      (funcall (intern (concat "all-the-icons-" (pop parts))) (pop parts) :face 'hass-icon-face))))

(defun hass--set-state (entity-id state attributes)
  "Set the state of an entity.
ENTITY-ID is the id of the entity in Home Assistant.

STATE is a string of the state of ENTITY-ID in Home Assistant."
  (let ((current-attrs (cdr (gethash entity-id hass--states))))
    (puthash entity-id (cons state (or attributes current-attrs)) hass--states)))

(defun hass-state-of (entity-id)
  "Return the last known state of ENTITY-ID.
ENTITY-ID is the id of the entity in Home Assistant."
  (car (gethash entity-id hass--states)))

(defun hass-attribute-of (entity-id attribute)
  "Return the last known value of ATTRIBUTE for ENTITY-ID.
ENTITY-ID is the id of the entity in Home Assistant.

ATTRIBUTE is the key of the attribute to return the value for."
  (cdr (assoc attribute (cdr (gethash entity-id hass--states)))))

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


;;; Logging
(defvar hass--debug nil
  "Enable debug logging when t.")

(defvar hass--debug-buffer "*Hass Debug*"
  "Name of the buffer used for debug messages.")

(defvar hass--debug-ignore '("EVENT")
  "Debug events to not show in debug buffer.")

(defun hass--debug-buffer ()
  "Return the debug buffer for hass."
  (or (get-buffer hass--debug-buffer)
      (let ((buf (get-buffer-create hass--debug-buffer)))
        (with-current-buffer buf
          (read-only-mode 1)
          (set (make-local-variable 'window-point-insertion-type) t))
        buf)))

(defface hass--debug-heading-face
  `((default :height 1.0
             :background "#4f00aa"
             :weight bold
             :extend t))
  "Face for widget group labels in HASS's dashboard."
  :group 'hass)

(defface hass--debug-timestamp-face
  `((default :height 0.8
             :inherit hass--debug-heading-face
             :background "#270055"
             :weight normal
             :extend t))
  "Face for widget group labels in HASS's dashboard."
  :group 'hass)

(defun hass--debug-clear-buffer ()
  "Erase the hass-debug buffer."
  (interactive)
  (when-let ((buf (get-buffer hass--debug-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun hass--debug (type &rest msg)
  "Display a message in the hass debug buffer.
TYPE is the type of debug message.  Also shown as the header of
the logged message.

MSG is the message to be display in the debug buffer."
  (when (and hass--debug
             (not (member type hass--debug-ignore)))
    (with-current-buffer (hass--debug-buffer)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (propertize (concat " " type " ") 'face 'hass--debug-heading-face))
        (insert (propertize (concat " " (current-time-string)) 'face 'hass--debug-timestamp-face))
        (newline)
        (insert (apply #'format msg))
        (newline)))))

(defun hass--message (&rest msg)
  "Display a message in the `*Messages*' buffer.
MSG is the message to be display in the messages buffer."
  (hass--debug "MESSAGE" "%s" (apply #'format msg))
  (message "(hass) %s" (apply #'format msg)))

(defun hass--warning (&rest msg)
  "Display a warning in the warnings buffer.
MSG is the message to be display in the warnings buffer."
  (hass--debug "WARNING" "%s" (apply #'format msg))
  (display-warning 'hass (apply #'format msg)))


;;; HTTP
;;;; Parsing
(defun hass--parse-all-entities (entities)
  "Convert entity state data into a list of available entities.
ENTITIES is the data returned from the `/api/states' endpoint."
  (delq nil (mapcar (lambda (entity) (hass--parse-entity entity))
                    entities)))

(defun hass--parse-entity (entity-state)
  "Convert an entity's state data into its entity-id.
ENTITY-STATE is an individual entity state data return from the
`/api/states' endpoint."
  (let* ((entity-id (cdr (assoc 'entity_id entity-state)))
         (friendly-name (or (cdr (assoc 'friendly_name (cdr (assoc 'attributes entity-state))))
                            entity-id)))
    `(,entity-id . (:friendly_name ,friendly-name
                                   :icon ,(hass--icon-of-entity entity-id)))))

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


;;;; Callbacks
(defun hass--get-entities-result (entities)
  "Callback when states of all ENTITIES is received from API."
  (setq hass--available-entities (hass--parse-all-entities entities))
  (run-hooks 'hass-entity-updated-hook))

(defun hass--get-available-services-result (domains)
  "Callback when all service information is received from API.
DOMAINS is the response from the `/api/services' endpoint which
returns a list of domains and their available services."
  (setq hass--available-services (hass--parse-all-domains domains)))

(defun hass--query-entity-result (entity-id state attributes)
  "Callback when an entity state data is received from API.
ENTITY-ID is the id of the entity in Home Assistant that has state STATE."
  (unless (equal (hass-state-of entity-id) state)
    (run-hook-with-args 'hass-entity-state-changed-functions entity-id))
  (run-hooks 'hass-entity-updated-hook)
  (hass--set-state entity-id state attributes))

(defun hass--call-service-result (entity-id state)
  "Callback when a successful service request is received from API.
ENTITY-ID is the id of the entity in Home Assistant that was
affected and now has STATE."
  (hass--set-state entity-id state)
  (run-hooks 'hass-service-called-hook))

(defun hass--request-error-handler (endpoint payload response)
  "Entrypoint for an HTTP request error.
ENDPOINT is the endpoint the request went to.

HTTP-STATUS-CODE is the integer http error code from the request.

RESPONSE is the request-response object from requests.el"
  (setq endpoint (split-string endpoint "/"))
  (or (hass--request-error-handle-api endpoint payload response)
      (hass--request-error-handle-states endpoint response)
      (hass--request-error-handle-services endpoint response)
      (if-let ((data (request-response-data response)))
          (hass--warning "Unknown error occurred with URL: `%s', %s" (request-response-url response) data)
        (hass--warning "Unknown error occurred with URL: %s" (request-response-url response)))))

(defun hass--request-error-handle-api (response payload response)
  "Try to handle error dealing with the main `api' endpoint.
Return t if handled.

RESPONSE is the request-response object from requests.el"
  (when (eq (request-response-symbol-status response) 'parse-error)
    (hass--warning "Request failed: HTTP %s
url: %s
payload: %s "
                   (request-response-status-code response)
                   (request-response-url response)
                   payload)
    t))

(defun hass--request-error-handle-states (endpoint response)
  "Try to handle error dealing with the `states' endpoint.
Return t if handled.

ENDPOINT is the endpoint the request went to.

HTTP-STATUS-CODE is the integer http error code from the request.

RESPONSE is the request-response object from requests.el"
  (when-let ((entity-id (and (= 404 (request-response-status-code response))
                             (nth 2 endpoint))))
    (setq hass-tracked-entities (delete entity-id hass-tracked-entities))
    (hass--warning "Entity %S was not found." entity-id)
    t))

(defun hass--request-error-handle-services (endpoint response)
  "Try to handle error dealing with the `services' endpoint.
Return t if handled.

ENDPOINT is the endpoint the request went to.

HTTP-STATUS-CODE is the integer http error code from the request.

RESPONSE is the request-response object from requests.el"
  (when (and (= 404 (request-response-status-code response))
             (>= 3 (length endpoint)))
    (setq hass-tracked-entities (delete (nth 2 endpoint) hass-tracked-entities))
    (hass--warning "Entity %S was not found." (nth 2 endpoint))
    t))


;;;; Requests
(defun hass--request (type endpoint &optional success payload)
  "Make a request to Home Assistant.
TYPE is a string of the type of request to make.  For example, `\"GET\"'.

ENDPOINT is a string of endpoint portion of the url for the request.

SUCCESS is a callback function for when the request successful
completed.

PAYLOAD is contents the body of the request."
  (hass--debug "HTTP" "endpoint: %s\npayload: %s" endpoint payload)
  (request (hass--url endpoint)
    :sync nil
    :type type
    :headers `(("User-Agent" . ,hass--user-agent)
               ("Authorization" . ,(concat "Bearer " (hass--apikey)))
               ("Content-Type" . "application/json"))
    :data payload
    :parser (lambda () (hass--deserialize (buffer-string)))
    :error (cl-function
            (lambda (&key _symbol-status response &allow-other-keys)
              (hass--request-error-handler endpoint
                                           payload
                                           response)))
    :success (when success
               (cl-function
                (lambda (&key response &allow-other-keys)
                  (let ((data (request-response-data response)))
                    (funcall success data)))))))

(defun hass--get-available-entities (&optional callback)
  "Retrieve the available entities from the Home Assistant instance.
Makes a request to `/api/states' but drops everything except an
list of entity-ids.
Optional argument CALLBACK ran after entities are received."
  (hass--request "GET" "api/states"
                 (lambda (data)
                   (hass--get-entities-result data)
                   (when callback (funcall callback)))))

(defun hass--get-available-services (&optional callback)
  "Retrieve the available services from the Home Assistant instance.
Optional argument CALLBACK ran after services are received."
  (hass--request "GET" "api/services"
                 (lambda (data)
                   (hass--get-available-services-result data)
                   (when callback (funcall callback)))))

(defun hass--get-entity-state (entity-id)
  "Retrieve the current state of ENTITY-ID from the Home Assistant server."
  (hass--request "GET" (hass--entity-endpoint entity-id)
                 (lambda (data)
                   (hass--query-entity-result
                    entity-id
                    (cdr (assoc 'state data))
                    (cdr (assoc 'attributes data))))))

(defun hass--update-tracked-entities ()
  "Update current state of tracked entities."
  (dolist (entity hass-tracked-entities)
    (hass--get-entity-state entity)))

(defun hass--call-service (service payload &optional success-callback)
  "Call service SERVICE for ENTITY-ID on the Home Assistant server.

SERVICE is a string of the Home Assistant service to be called.

PAYLOAD is a JSON-encoded string of the payload to be sent with SERVICE.

SUCCESS-CALLBACK is a function to be called with a successful request response."
  (hass--request "POST"
                 (hass--service-endpoint service)
                 success-callback
                 payload))

;;;###autoload
(cl-defun hass-call-service (entity-id service &optional (update t))
  "Call service for an entity on Home Assistant.
If called interactively, prompt the user for an ENTITY-ID and
SERVICE to call.

This will send an API request to the address configure in `hass-host'.

ENTITY-ID is the id of the entity in Home Assistant.
to call the service on.  (e.g. `\"switch.kitchen_light\"').

SERVICE is the service you want to call on ENTITY-ID.  (e.g. `\"turn_off\"')

When UPDATE is t, another API request will be sent to retrieve
the new state of the affected entity."
  (interactive
   (let ((entity (completing-read "Entity: "
                                  hass--available-entities
                                  (lambda (entity) (hass--services-for-entity (car entity)))
                                  t)))
     (list entity
           (format "%s.%s"
                   (hass--domain-of-entity entity)
                   (completing-read (format "%s: " entity) (hass--services-for-entity entity) nil t)))))
  (hass-call-service-with-payload
   service
   `((entity_id . ,entity-id))
   (when update (lambda (&rest _) (hass--get-entity-state entity-id)))))

;;;###autoload
(defun hass-call-service-with-payload (service payload &optional success-callback)
  "Call service with a custom payload on Home Assistant.
This will send an API request to the address configure in `hass-host'.

SERVICE is a string of the Home Assistant service to be called.

PAYLOAD is an alist of service parameters to their values be sent with SERVICE.

SUCCESS-CALLBACK is a function to be called with a successful request response."
  (hass--call-service
   service
   (hass--serialize payload)
   (lambda (&rest _)
     (run-hooks 'hass-service-called-hook)
     (when success-callback (funcall success-callback)))))


;;; Init
(defun hass--config-errors ()
  "Return nil if no configuration errors are found.
If error found, a string of the error message is returned."
  (cond ((not (equal (type-of (hass--apikey)) 'string))
         "HASS-APIKEY must be set to use hass.")
        ((not (equal (type-of hass-host) 'string))
         "HASS-HOST must be set to use hass.")
        (t nil)))

(defun hass--connect ()
  "Populate available Home Assistant entities and services."
  (hass--request "GET" "api/"
                 (lambda (data)
                   (when (string= "API running." (cdr (assoc 'message data)))
                     (run-hooks 'hass-api-connected-hook)))))

;;;###autoload
(define-minor-mode hass-mode
  ""
  :global t
  :lighter nil
  :group 'hass
  (if hass-mode
      (if-let ((err (hass--config-errors)))
          (hass--warning err nil)
        (hass--connect)
        (hass-websocket--connect))
    (hass-websocket--disconnect)))

(provide 'hass)

;;; hass.el ends here
