ELM_SRC=$(wildcard src/**/*.elm) $(wildcard src/*.elm)

index.html: build/index.js build/styles.css html/*
	cat html/head.html build/index.js html/afterElm.html build/styles.css html/afterStyles.html html/init.js html/afterInit.html > index.html

build:
	mkdir build

build/index.js: build ${ELM_SRC} elm-package.json
	elm make --warn --output=build/index.js src/Main.elm

build/styles.css: build ${ELM_SRC} elm-package.json
	elm-css --output build src/Stylesheets.elm

manifest.txt:
	s3cmd ls --recursive s3://converge-builds-dl/ > manifest.txt
