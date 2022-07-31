;;; test-hass.el --- Tests of hass -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(require 'ert)
(require 'hass)

(cl-defun hass-test-wait (process &optional (seconds 0.1) (times 100))
  "Wait for SECONDS seconds TIMES times for PROCESS to finish."
  (when process
    (cl-loop for i upto times ;; 10 seconds
             while (not (request-response-done-p process))
             do (sleep-for seconds))))

(setq hass-host "homeassistant")
(setq hass-insecure t)
(setq hass-apikey "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiIzZmMxMjU1MDEwYTM0MGM2YjEyMjY0ZjgwMjRkN2E3NCIsImlhdCI6MTY1OTIxNDU0NSwiZXhwIjoxOTc0NTc0NTQ1fQ.9Qrtm-XjTdS0_RnADoI_D1YxZ9mF80iXmU6EtXUNY_U")

(setq hass-test-entity "input_boolean.hass_test")

(ert-deftest hass--url nil
  (let ((hass-insecure t) (hass-host "homeassistant") (hass-port 8123))
    (should (string= (hass--url "api/") "http://homeassistant:8123/api/")))

  (let ((hass-insecure nil) (hass-host "127.0.0.1") (hass-port 8124))
    (should (string= (hass--url "api/") "https://127.0.0.1:8124/api/"))))

(ert-deftest hass--apikey nil
  (let ((hass-apikey "Fake-API-Key"))
    (should (string= (hass--apikey) "Fake-API-Key")))

  (let ((hass-apikey (lambda () "Fake-API-Key")))
    (should (string= (hass--apikey) "Fake-API-Key"))))

(ert-deftest hass--entity-url nil
  (should (string= (hass--entity-url hass-test-entity)
                   (concat "http://homeassistant:8123/api/states/" hass-test-entity))))

(ert-deftest hass--service-url nil
  (should (string= (hass--service-url "the_domain.service")
                   (concat "http://homeassistant:8123/api/services/the_domain/service"))))

(ert-deftest hass--domain-of-entity nil
  (should (string= (hass--domain-of-entity "the_domain.entity_id") "the_domain")))

(ert-deftest hass--check-api-connection nil
  (setq hass-api-connected-hook nil)
  (hass-test-wait (hass--check-api-connection))
  (should hass--api-running))

;; TODO
;; (ert-deftest hass--icon-of-entity nil (should nil))

(ert-deftest hass--get-available-services nil
  (hass-test-wait (hass--get-available-services))
  (should (member 'toggle (hass--services-for-entity hass-test-entity))))

(ert-deftest hass--get-available-entities nil
  (hass-test-wait (hass--get-available-services))
  (hass-test-wait (hass--get-available-entities))
  (should (string= (hass-friendly-name hass-test-entity) "Hass test")))
