all: sandbox
	cabal install
	bower install
	pulp browserify --to dist/todo.js

sandbox:
	cabal sandbox init
