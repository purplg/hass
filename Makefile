docker-name := hass-test-homeassistant
emacs := emacs -Q --batch -l targets/melpa.el

.PHONY: test
test:
	${emacs} -L ./ -l ./tests/test-*.el --eval="(ert-run-tests-batch-and-exit)"

.PHONY: start-homeassistant
start-homeassistant:
	docker run -d --rm -p 8123:8123 --name ${docker-name} ${docker-name} && \
	timeout 10 bash -c "until curl http://localhost:8123/api/ --silent > /dev/null ; do sleep 0.2 ; done"
	sleep 1

.PHONY: build-homeassistant
build-homeassistant:
	docker build -t ${docker-name} tests/docker/

.PHONY: deps
deps:
	${emacs} -l targets/deps.el
