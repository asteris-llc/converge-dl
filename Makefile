index.html: build/index.js html/*
	cat html/head.html build/index.js html/afterElm.html html/init.js html/afterInit.html > index.html

build:
	mkdir build

build/index.js: build $(shell find src -name '*.elm') elm-package.json
	elm make --warn --output=build/index.js src/Main.elm

manifest.txt:
	s3cmd ls --recursive s3://converge-builds-dl/ > manifest.txt
