docker-name := hass-test-homeassistant

.PHONY: test
test:
	emacs -Q --batch -l targets/melpa.el -L ./ -l ./tests/test-*.el --eval="(ert-run-tests-batch-and-exit)"

.PHONY: start-homeassistant
start-homeassistant:
	docker run -d --rm -p 8123:8123 --name ${docker-name} ${docker-name} && \
	timeout 10 bash -c "until curl http://localhost:8123/api/ --silent > /dev/null ; do sleep 0.2 ; done"

.PHONY: build-homeassistant
build-homeassistant:
	docker build -t ${docker-name} tests/docker/

.PHONY: deps
deps:
	emacs -Q --batch -l targets/melpa.el -l targets/deps.el
