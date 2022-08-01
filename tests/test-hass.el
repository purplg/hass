;;; test-hass.el --- Tests of hass -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'ert)
(require 'ert-async)

(require 'hass)

(unless hass-host (setq hass-host "localhost"))
(setq hass-insecure t)
(setq hass-apikey "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiIzZmMxMjU1MDEwYTM0MGM2YjEyMjY0ZjgwMjRkN2E3NCIsImlhdCI6MTY1OTIxNDU0NSwiZXhwIjoxOTc0NTc0NTQ1fQ.9Qrtm-XjTdS0_RnADoI_D1YxZ9mF80iXmU6EtXUNY_U")

(setq hass-test-entity "input_boolean.hass_test")

(defun hass-test-with-entities (callback)
  "Setup function where the callback will be called after all
services and entities are retrieved."
  (hass--get-available-services
   (lambda ()
     (hass--get-available-entities (funcall callback)))))

(ert-deftest hass--entity-url nil
  (should (string= (hass--entity-url hass-test-entity)
                   (format "http://%s:8123/api/states/%s" hass-host hass-test-entity))))

(ert-deftest hass--service-url nil
  (should (string= (hass--service-url "the_domain.service")
                   (format "http://%s:8123/api/services/the_domain/service" hass-host))))

(ert-deftest hass--domain-of-entity nil
  (should (string= (hass--domain-of-entity "the_domain.entity_id") "the_domain")))

(ert-deftest hass--services-for-entity nil
  (should (member 'toggle (hass--services-for-entity hass-test-entity))))

;; TODO
;; (ert-deftest hass--icon-of-entity nil (should nil))

(ert-deftest-async hass--check-api-connection (done)
  (add-hook 'hass-api-connected-hook done)
  (hass--check-api-connection))

(ert-deftest-async hass--get-available (done-services done-entities)
  (hass--get-available-services
   (lambda ()
     (should (member 'toggle (hass--services-for-entity hass-test-entity)))
     (funcall done-services)
     (hass--get-available-entities done-entities))))

(ert-deftest hass-friendly-name nil
  (hass-test-with-entities
   (lambda ()
     (should (string= (hass-friendly-name hass-test-entity) "Hass test")))))
