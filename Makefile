ELM_FILES = $(wildcard src/*.elm) $(wildcard src/**/*.elm) $(wildcard src/**/*.js)

documentation.json: ${ELM_FILES}
	elm make --yes --docs=$@

elm-ops-tooling:
	git clone https://github.com/NoRedInk/elm-ops-tooling

tests/elm-stuff: elm-ops-tooling
	cd tests; ../elm-ops-tooling/with_retry.rb elm package install --yes

