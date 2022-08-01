set positional-arguments

compose := "docker compose -f tests/docker/docker-compose.yml"
compose-run := compose + " run --rm"
compose-down := compose + " down"

@test version:
	[ "all" == {{version}} ] && just test-all || just _test-{{version}}
	{{compose-down}}

@test-all: _test-25 _test-26 _test-27 _test-28 _test-master
	{{compose-down}}

@_test-25: _init_eldev
	echo -e "\e[1;33m:::Testing Emacs 25\e[0m"
	{{compose-run}} emacs-25

@_test-26: _init_eldev
	echo -e "\e[1;33m:::Testing Emacs 26\e[0m"
	{{compose-run}} emacs-26

@_test-27: _init_eldev
	echo -e "\e[1;33m:::Testing Emacs 27\e[0m"
	{{compose-run}} emacs-27

@_test-28: _init_eldev
	echo -e "\e[1;33m:::Testing Emacs 28\e[0m"
	{{compose-run}} emacs-28

@_test-master: _init_eldev
	echo -e "\e[1;33m:::Testing Emacs master\e[0m"
	{{compose-run}} emacs-master

@_init_eldev:
	[ -d "./tests/eldev" ] || git clone https://github.com/doublep/eldev.git --quiet --branch 1.1.3 tests/eldev

start-homeassistant:
	{{compose}} up -d homeassistant
