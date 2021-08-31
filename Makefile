PACKAGE-NAME=gitcommit

all: setup

install:
	raco pkg install

setup:
	raco setup --tidy --avoid-main --pkgs $(PACKAGE-NAME)
