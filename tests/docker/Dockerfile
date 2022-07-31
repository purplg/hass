FROM homeassistant/home-assistant

## This file enables authenticating via curl with the token located in `.env`
COPY auth /config/.storage/auth

## This file enables authenticating via the web interface with a username and password. Really just
## for debugging
## username: dev
## password: dev
# COPY auth_provider.homeassistant /config/.storage/auth_provider.homeassistant

## This is the main Home Assistant configuration file with some test entities added to test against
COPY configuration.yaml /config/configuration.yaml
