(setq package-user-dir (expand-file-name "hass-pkgs" "/tmp"))

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)
