compose := docker compose -f tests/docker/docker-compose.yml
compose-down := ${compose} down

.PHONY: test
test: start-homeassistant
	emacs -Q --batch -l targets/melpa.el -L ./ -l ./tests/test-*.el --eval="(ert-run-tests-batch-and-exit)"

.PHONY: start-homeassistant
start-homeassistant:
	@${compose} up -d homeassistant && \
	timeout 10 bash -c "until curl http://localhost:8123/api/a --silent > /dev/null ; do sleep 0.2 ; done"

.PHONY: deps
deps:
	emacs -Q --batch -l targets/melpa.el -l targets/deps.el
