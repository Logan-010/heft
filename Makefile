.PHONY: build
build:
	cd frontend && npm run build
	cabal build --project-file=cabal.project.release

.PHONY: build-dev
build-dev:
	cd frontend && npm run build
	cabal build

.PHONY: run
run: build
	cabal run heft

.PHONY: run-dev
run-dev: build-dev
	cabal run heft

.PHONY: install
install:
	cd frontend && npm i
	cd frontend && elm2nix convert > elm-srcs.nix && elm2nix snapshot

.PHONY: fmt
fmt:
	ormolu app/**/*.hs -i
	cabal-fmt *.cabal -i

.PHONY: lint
lint:
	hlint app/**/*.hs
