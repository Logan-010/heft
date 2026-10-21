{
  description = "Heft Flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };

          hs = pkgs.haskellPackages.override {
            overrides = hself: hsuper: {
              password = pkgs.haskell.lib.dontCheck (
                hself.callHackageDirect {
                  pkg = "password";
                  ver = "3.1.1.0";
                  sha256 = "sha256-LX9EQ0aUgA1zr6CwO+LteljyrgCaOag1n/dIvpPWeL4=";
                } { }
              );
            };
          };

          frontend = pkgs.buildNpmPackage {
            pname = "heft-frontend";
            version = "1.0.0";

            src = ./frontend;

            npmDepsHash = "sha256-j7ljm26/PWRKNTTjcpNkfdxBGLpyDaidpysFyKjq81E=";

            nativeBuildInputs = [
              pkgs.elmPackages.elm
            ];

            dontNpmBuild = true;

            buildPhase = ''
              runHook preBuild

              rm -rf elm-stuff .elm

              ${pkgs.elmPackages.fetchElmDeps {
                elmPackages = import ./frontend/elm-srcs.nix;
                elmVersion = pkgs.elmPackages.elm.version;
                registryDat = ./frontend/registry.dat;
              }}

              export ELM_HOME="$PWD/.elm"
              test -s "$ELM_HOME/$(elm --version)/packages/registry.dat"

              npm run build

              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall

              mkdir -p "$out"
              cp -r dist/. "$out/"

              runHook postInstall
            '';
          };

          projectSrc = pkgs.runCommand "heft-source" { } ''
            cp -r ${./.} $out
            chmod -R u+w $out

            rm -rf $out/frontend/dist
            mkdir -p $out/frontend/dist

            cp -r ${frontend}/. $out/frontend/dist/

            test -s $out/frontend/dist/index.html
            test -s $out/frontend/dist/main.js
            test -s $out/frontend/dist/style.css
            test -s $out/frontend/dist/icon.png
          '';

          haskellApp = pkgs.haskell.lib.dontCheck (hs.callCabal2nix "heft" projectSrc { });

          heft = pkgs.haskell.lib.doStrip haskellApp;
        in
        {
          default = heft;

          inherit frontend;
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              # Compiler and build tools
              haskellPackages.ghc
              cabal-install
              elm2nix

              # Language server
              haskell-language-server

              # Elm compiler and tooling
              elmPackages.elm
              elmPackages.elm-format
              elmPackages.elm-test
              elmPackages.elm-language-server

              # JavaScript / frontend tooling
              nodejs

              # Formatting
              ormolu
              haskellPackages.cabal-fmt

              # Linting
              hlint

              # Dependencies
              zlib

              # Utilities
              haskellPackages.ghcid
              git
              gnumake
              pkg-config
            ];
          };
        }
      );
    };
}
