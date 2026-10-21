.PHONY: build
build:
	cd frontend && npm run build
	cabal build exe:heft --project-file=cabal.project.release
	strip --strip-all $$(cabal list-bin exe:heft)

.PHONY: build-dev
build-dev:
	cd frontend && npm run build
	cabal build exe:heft

.PHONY: run
run: build
	cabal run exe:heft

.PHONY: run-dev
run-dev: build-dev
	cabal run exe:heft

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
