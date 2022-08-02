compose := docker compose -f tests/docker/docker-compose.yml
compose-run := ${compose} run --rm
compose-down := ${compose} down

.PHONY: test-all
test-all: test-25 test-26 test-27 test-28 test-master
	${compose-down}

.PHONY: test-25
test-25: init_eldev
	@echo -e "\e[1;33m:::Testing Emacs 25\e[0m" && \
	${compose-run} emacs-25

.PHONY: test-26
test-26: init_eldev
	@echo -e "\e[1;33m:::Testing Emacs 26\e[0m" && \
	${compose-run} emacs-26

.PHONY: test-27
test-27: init_eldev
	@echo -e "\e[1;33m:::Testing Emacs 27\e[0m" && \
	${compose-run} emacs-27

.PHONY: test-28
test-28: init_eldev
	@echo -e "\e[1;33m:::Testing Emacs 28\e[0m" && \
	${compose-run} emacs-28

.PHONY: test-master
test-master: init_eldev
	@echo -e "\e[1;33m:::Testing Emacs master\e[0m" && \
	${compose-run} emacs-master

.PHONY: init_eldev
init_eldev:
	@[ -d "./tests/eldev" ] || git clone https://github.com/doublep/eldev.git --quiet --branch 1.1.3 tests/eldev

.PHONY: start-homeassistant
start-homeassistant:
	@${compose} up -d homeassistant
