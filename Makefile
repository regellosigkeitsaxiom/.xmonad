all: build restart

build:
	@xmonad --recompile
	@echo -e '\e[1mBuild succesful\e[0m'

restart:
	@xmonad --restart
