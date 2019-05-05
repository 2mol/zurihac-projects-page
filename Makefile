.PHONY: build
.DEFAULT: build

build: ## compile elm to elm.json
	elm make src/Main.elm --optimize --output elm.js
