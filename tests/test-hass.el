;;; test-hass.el --- Tests of hass -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'ert)
(require 'ert-async)

(require 'hass)

(unless hass-host (setq hass-host "localhost"))
(setq hass-insecure t)
(setq hass-apikey "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiIzZmMxMjU1MDEwYTM0MGM2YjEyMjY0ZjgwMjRkN2E3NCIsImlhdCI6MTY1OTIxNDU0NSwiZXhwIjoxOTc0NTc0NTQ1fQ.9Qrtm-XjTdS0_RnADoI_D1YxZ9mF80iXmU6EtXUNY_U")

;; These values are from the testing Home Assistant docker instance.
(setq hass-test-entity-id "input_boolean.hass_test")
(setq hass-test-entity-name "Hass test")

(defun hass-test-with-entities (callback)
  "Setup function where the callback will be called after all
services and entities are retrieved."
  (hass--get-available-services
   (lambda ()
     (hass--get-available-entities (funcall callback)))))

(ert-deftest hass-test-entity-url nil
  (should (string= (hass--entity-url hass-test-entity-id)
                   (format "http://%s:8123/api/states/%s" hass-host hass-test-entity-id))))

(ert-deftest hass-test-service-url nil
  (should (string= (hass--service-url "the_domain.service")
                   (format "http://%s:8123/api/services/the_domain/service" hass-host))))

(ert-deftest hass-test-domain-of-entity nil
  (should (string= (hass--domain-of-entity "the_domain.entity_id") "the_domain")))

(ert-deftest hass-test-deserialize nil
  "Ensure the correct version of json deserialization is being
called. Native json parsing should only be used on Emacs 27.1 or
higher."
  (let ((native-called nil)
        (elisp-called nil))
    (advice-add #'json-read-from-string :after (lambda (&rest _) (setq elisp-called t)))
    (advice-add #'json-parse-string :after (lambda (&rest _) (setq native-called t)))
    (hass--deserialize "{}")

    (when (version< emacs-version "27.1")
      (should elisp-called)
      (should-not native-called))

    (when (version<= "27.1" emacs-version)
      (should native-called)
      (should-not elisp-called))))

(ert-deftest hass-test-serialize nil
  "Ensure the correct version of json serialization is being
called. Native json parsing should only be used on Emacs 27.1 or
higher."
  (let ((native-called nil)
        (elisp-called nil))
    (advice-add #'json-encode :after (lambda (&rest _) (setq elisp-called t)))
    (advice-add #'json-serialize :after (lambda (&rest _) (setq native-called t)))

    (hass--serialize '())

    (when (version< emacs-version "27.1")
      (should elisp-called)
      (should-not native-called))

    (when (version<= "27.1" emacs-version)
      (should native-called)
      (should-not elisp-called))))

;; TODO
;; (ert-deftest hass--icon-of-entity nil (should nil))

(ert-deftest hass-test-entity-parsing nil
  "Entities without domains with services listed in the
`hass--available-services' list should be filtered out of the
`hass--available-entities' list."
  (let ((hass--available-services '(("callable_services" . some_service)))
        (entities '[((entity_id . "callable_services.test_entity_one")
                     (state . "off")
                     (attributes
                      (icon . "mdi:test-tube")
                      (friendly_name . "Test Entity One"))
                     (last_changed . "ignored")
                     (last_updated . "ignored"))
                    ((entity_id . "no_services.test_entity_one")
                     (state . "on")
                     (attributes
                      (icon . "mdi:test-tube")
                      (friendly_name . "Test Entity Two"))
                     (last_changed . "ignored")
                     (last_updated . "ignored"))]))
    (setq hass--available-entities (hass--parse-all-entities entities))

    (should (string= (hass-friendly-name "callable_services.test_entity_one") "Test Entity One"))
    (should-not (hass-friendly-name "callable_services.test_entity_two"))))

(ert-deftest hass-test-domain-parsing nil
  (let ((services '[((domain . "test_domain_one")
                     (services (domain_one_service_one
                                (name . "Domain One Service One")
                                (description . "This is the first test service in the first domain"))
                               (domain_one_service_two
                                (name . "Domain One Service two")
                                (description . "This is the second test service in the first domain"))))
                    ((domain . "test_domain_two")
                     (services (domain_two_service_one
                                (name . "Domain Two Service One")
                                (description . "This is the first test service in the second domain"))
                               (domain_two_service_two
                                (name . "Domain Two Service two")
                                (description . "This is the second test service in the second domain"))))]))
    (setq hass--available-services (hass--parse-all-domains services))

    (should (equal (cdr (assoc "test_domain_one" hass--available-services))
                   '(domain_one_service_one domain_one_service_two)))

    (should (equal (cdr (assoc "test_domain_two" hass--available-services))
                   '(domain_two_service_one domain_two_service_two)))))

(ert-deftest-async hass-test-check-api-connection (done)
  (add-hook 'hass-api-connected-hook done)
  (hass--check-api-connection))

(ert-deftest-async hass-test-get-available (done-services done-entities)
  (hass--get-available-services
   (lambda ()
     (should (member 'toggle (hass--services-for-entity hass-test-entity-id)))
     (funcall done-services)
     (hass--get-available-entities
      (lambda ()
        (should (string= (hass-friendly-name hass-test-entity-id) hass-test-entity-name))
        (funcall done-entities))))))

(ert-deftest-async hass-test-entity-states (done-turn-on)
  (hass--set-state hass-test-entity-id "off")
  (add-hook 'hass-entity-state-changed-functions
            (lambda (entity-id)
              (funcall done-turn-on)
              (should (string= entity-id hass-test-entity-id))
              (should (string= (hass-state-of hass-test-entity-id) "on"))))
  (hass-call-service hass-test-entity-id "input_boolean.turn_on"))
