main.js: src/*.elm elm-package.json
	elm make src/main.elm --output=main.js

all: main.js

clean:
	rm -f main.js
	rm -rf elm-stuff/

.PHONY: all, clean