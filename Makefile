index.html: src/*.elm elm-package.json
	elm make src/main.elm

all: index.html

clean:
	rm -f index.html
	rm -rf elm-stuff/

.PHONY: all, clean