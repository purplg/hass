[![MELPA](https://melpa.org/packages/hass-badge.svg)](https://melpa.org/#/hass)
[![MELPA Stable](https://stable.melpa.org/packages/hass-badge.svg)](https://stable.melpa.org/#/hass)

# hass-mode

`hass` enables you to control [Home Assistant](https://www.home-assistant.io/) entities from Emacs.

## Installation

This package is available on [MELPA](https://melpa.org/).

### package.el

``` emacs-lisp
(use-package hass
  :ensure t
  (setq hass-url "http://homeassistant:8123"
        hass-apikey "APIKEY-GOES-IN-HERE")
  (hass-setup))
```

### straight.el

``` emacs-lisp
(straight-use-package 'hass)
(setq hass-url "http://homeassistant:8123"
      hass-apikey "APIKEY-GOES-IN-HERE")
(hass-setup)
```

### Doom Emacs

Place in your `packages.el` then run `doom sync` to pull the repository:

``` emacs-lisp
(package! hass)
```

Then load the package in your main config file.

``` emacs-lisp
(use-package! hass
  :config
  (setq hass-url "http://homeassistant:8123"
        hass-apikey "APIKEY-GOES-IN-HERE")
  (hass-setup))
```

## Configuration

Both `hass-url` and `hass-apikey` must be set to use this package. Set `hass-url` to your Home
Assistant instance.

``` emacs-lisp
(setq hass-url "http://homeassistant:8123"
      hass-apikey "APIKEY-GOES-IN-HERE")
(hass-setup)
```

Alternatively, you can store a function inside `hass-apikey`.
This will be executed on every query.

``` emacs-lisp
(setq hass-apikey (lambda () (auth-source-pass-get 'secret "home/hass/emacs-apikey")))
```

Once those variables are set, you must call `(hass-setup)` before using this package so that it can
query the Home Assistance instance and populate available entities and services.

### Getting an API Key

Ensure that your Home Assistant instance is configured to support API
calls by following the instructions
[here](https://www.home-assistant.io/integrations/api/).

Retrieve your API key a.k.a. *Long-Lived Access Token* by logging into
your Home Assistant instance and going to your profile by selecting your
username in the lower-left corner or going to this URL:
`http://HOME-ASSISTANT-URL:8123/profile`. You can generate an API token
at the very bottom of this page.

## Usage

To call a service on Home Assistant, use the `hass-call-service`
function which has two required arguments: `entity-id` and `service`.

``` emacs-lisp
(hass-call-service "switch.bedroom_light" "switch.toggle")
```

If you call `hass-call-service` interactively, it will prompt you for an
entity ID and then the respective service you want to call.

### Payloads

For services that require additional data use the `hass-call-service-with-payload` function. The
second argument, `payload`, requires an JSON encoded string.

This example publishes to an MQTT topic:

``` emacs-lisp
(hass-call-service-with-payload
 "mqtt.publish"
 (json-encode '(("payload" . "PERFORM")
                ("topic" . "valetudo/vacuum/LocateCapability/locate/set"))))
```

You could pass a JSON string directly, but that would require escaping every quote which can be
cumbersome. Here's what the encoded list above looks like in JSON:

``` javascript
{
  "payload": "PERFORM",
  "topic": "valetudo/vacuum/LocateCapability/locate/set"
}
```

### Watching entities

`hass-watch-mode` is a mode that periodically queries the Home Assistant instance to get the current
state of a list of entities. The list of entity IDs that will be queried are found in the variable
`hass-watch-entities`.

``` emacs-lisp
(setq hass-watched-entities '("switch.bedroom_light" "switch.bedroom_fan"))
```

The frequency of the query can be adjusted by setting
`hass-watch-frequency` to the number of seconds you'd like.
Defaults to 60.

Watching is most useful with the function hook `hass-entity-state-updated-functions` explained in
the [Hooks](#Hooks) section.

### Hooks

The most useful hook is a function list named `hass-entity-state-updated-functions`. Functions in
this list are passed a single argument `entity-id` which is the entity id of the entity whose state
has changed since it was last updated. Using this function hook along side [watching
entities](#Watching entities) enables Emacs to react to changes to Home Assistant entities.

This example will display the state of an entity when it changes:

``` emacs-lisp
(add-hook 'hass-entity-state-updated-functions
  (lambda (entity-id)
    (message "The entity %s state has changed to %s." entity-id (hass-state-of entity-id))))
```

The other two hooks available are `hass-entity-state-updated-hook` and
`hass-service-called-hook`. `hass-entity-state-updated-hook` is called
when the state of an entity is updated, regardless of if it changed or
not. `hass-service-called-hook` is called when a service is called.

``` emacs-lisp
(add-hook 'hass-service-called-hook (lambda () (message "A service was called.")))
(add-hook 'hass-entity-state-updated-hook (lambda () (message "An entitys' state was updated.")))
```

# License

MIT
