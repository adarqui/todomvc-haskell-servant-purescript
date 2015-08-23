all: sandbox
	cabal install
	bower install

pure:
	pulp browserify --to dist/todo.js

sandbox:
	cabal sandbox init
