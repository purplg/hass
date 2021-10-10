;;; hass-polling.el --- Periodically Home Assistant for changes -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "25.1"))
;; Author: Ben Whitley
;; SPDX-License-Identifier: MIT
;;; Commentary:

;; Homepage: https://github.com/purplg/hass

;;; Code:
(require 'hass)

(defvar hass-polling-mode-map (make-sparse-keymap)
  "Keymap for hass-polling-mode.")

(defcustom hass-polling-frequency 60
  "Amount of seconds between watching HASS-ENTITIES."
  :group 'hass
  :type 'integer)

;;;###autoload
(define-minor-mode hass-polling-mode
  "Toggle mode for querying Home Assistant periodically.
Watching is a way to periodically query the state of entities you
want to hook into to capture when their state changes.

Use the variable `hass-polling-frequency' to change how
frequently (in seconds) the Home Assistant instance should be
queried.

Use the variable `hass-watched-entities' to set which entities you want
to query automatically."
  :lighter nil
  :group 'hass
  :global t
  (when hass--timer (hass-watch--cancel-timer))
  (when hass-polling-mode
    (hass--get-available-services 'hass--get-available-entities)
    (when hass--timer (hass-watch--cancel-timer))
    (setq hass--timer (run-with-timer
                       nil
                       hass-polling-frequency
                       'hass-watch--query-entities))))

(defun hass-polling--cancel-timer ()
  "Cancel watch without disabling it."
  (when hass--timer
    (cancel-timer hass--timer)
    (setq hass--timer nil)))

(defun hass-polling--query-entities ()
  "Update the current state all of the registered entities."
  (dolist (entity hass-watched-entities)
    (hass--get-entity-state entity)))


(provide 'hass-polling)
