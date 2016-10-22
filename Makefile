export PATH := $(shell npm bin):${PATH}

build:
	@pulp build
