.PHONY: build dev-build
.DEFAULT: build

dev-build: ## compile elm to elm.json
	elm make src/Main.elm --output elm.js

build: ## compile elm to elm.json
	elm make src/Main.elm --optimize --output elm.js
