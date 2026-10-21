.PHONY: build
build:
	cd frontend && npm run build
	cabal build --project-file=cabal.project.release

.PHONY: build-dev
build-dev:
	cd frontend && npm run build
	cabal build

.PHONY: install
install:
	cd frontend && npm i
	cd frontend && elm2nix convert > elm-srcs.nix && elm2nix snapshot
