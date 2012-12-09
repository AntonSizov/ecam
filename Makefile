NAME=ecam

all: generate

generate: remove-release compile
	@./rebar generate

remove-release:
	@rm -rf ./rel/$(NAME)

compile: get-deps
	@./rebar compile

get-deps:
	@./rebar get-deps

clean: remove-release
	@./rebar clean

console:
	@./rel/$(NAME)/bin/$(NAME) console