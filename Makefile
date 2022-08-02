compose := docker compose -f tests/docker/docker-compose.yml
compose-run := ${compose} run --rm
compose-down := ${compose} down

test-all: test-25 test-26 test-27 test-28 test-master
	${compose-down}

test-25: init_eldev
	@echo -e "\e[1;33m:::Testing Emacs 25\e[0m" && \
	${compose-run} emacs-25

test-26: init_eldev
	@echo -e "\e[1;33m:::Testing Emacs 26\e[0m" && \
	${compose-run} emacs-26

test-27: init_eldev
	@echo -e "\e[1;33m:::Testing Emacs 27\e[0m" && \
	${compose-run} emacs-27

test-28: init_eldev
	@echo -e "\e[1;33m:::Testing Emacs 28\e[0m" && \
	${compose-run} emacs-28

test-master: init_eldev
	@echo -e "\e[1;33m:::Testing Emacs master\e[0m" && \
	${compose-run} emacs-master

init_eldev:
	@[ -d "./tests/eldev" ] || git clone https://github.com/doublep/eldev.git --quiet --branch 1.1.3 tests/eldev

start-homeassistant:
	@${compose} up -d homeassistant
