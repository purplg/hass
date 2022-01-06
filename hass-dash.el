;;; hass-dash.el --- Dash for Home Assistant -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "25.1") (hass "2.0.0")
;; Version: 1.0.0
;; Author: Ben Whitley
;; Homepage: https://github.com/purplg/hass
;; SPDX-License-Identifier: MIT
;;; Commentary:

;;; Code:
(require 'hass)

(defvar hass-dash-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'hass-dash-refresh)
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map [tab] 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    map)
  "Keymap for hass-dash-mode.")

(defcustom hass-dash-buffer-name "*hass-dash*"
  "The name of the hass-dash buffer."
  :group 'hass-dash
  :type 'string)

(defgroup hass-dash '()
  "Minor mode for hass."
  :group 'hass-dash
  :prefix "hass-dash-")

(defun hass-dash--create-widget-switch (entity-id)
  (widget-create 'toggle
    :tag (concat "hass-dash--entity-" entity-id)
    :format (concat entity-id "\n%[%v%]")
    :value (hass-switch-p entity-id)
    :action (lambda (widget &rest ignore)
              (hass-call-service
                entity-id
                (concat (hass--domain-of-entity entity-id) ".toggle")))))

(defun hass-dash-refresh ()
  (interactive)
  (let ((dash-buffer (get-buffer-create hass-dash-buffer-name)))
    (with-current-buffer dash-buffer
      (let ((inhibit-read-only t)
            (prev-point (progn (beginning-of-line) (point))))
         (erase-buffer)
         (mapc (lambda (entity-id)
                 (hass-dash--create-widget-switch entity-id)
                 (insert "\n\n"))
               hass-tracked-entities)
         (goto-char prev-point))
      (hass-dash-mode))))

(defun hass-dash-open ()
  (interactive)
  (hass-dash-refresh)
  (let ((dash-buffer (get-buffer-create hass-dash-buffer-name)))
    (switch-to-buffer-other-window dash-buffer)))

(define-derived-mode hass-dash-mode special-mode "Home Assistant Dash"
  "Dashboard for Home Assistant."
  :group 'hass-dash
  :syntax-table nil
  :abbrev-table nil
  :interactive t)

(add-hook 'hass-entity-state-updated-functions (lambda (_) (hass-dash-refresh)))

(hass-dash-refresh)

(provide 'hass-dash)

;;; hass-dash.el ends here
