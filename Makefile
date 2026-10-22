.PHONY: build
build: build-frontend
	cabal build exe:heft --project-file=cabal.project.release
	strip --strip-all $$(cabal list-bin exe:heft --project-file=cabal.project.release)

.PHONY: build-frontend
build-frontend:
	cd frontend && npm run build

.PHONY: build-dev
build-dev: build-frontend
	cabal build exe:heft

.PHONY: run
run: build
	cabal run exe:heft --project-file=cabal.project.release

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
	elm-format frontend/src/**/*.elm --yes

.PHONY: lint
lint:
	hlint app/**/*.hs
