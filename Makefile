index.html: $(shell find src -name '*.elm') elm-package.json
	elm make --warn --output=index.html src/Main.elm
