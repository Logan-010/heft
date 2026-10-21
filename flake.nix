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

          hs = pkgs.haskellPackages;

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

          haskellApp = pkgs.haskell.lib.dontCheck (hs.callCabal2nix "project" projectSrc { });

          haskellEnv = haskellApp.env;

          heft = pkgs.stdenv.mkDerivation {
            pname = "heft";
            version = "1.0.0";

            src = projectSrc;

            nativeBuildInputs = [
              haskellEnv
              pkgs.cabal-install
              pkgs.pkg-config
            ];

            buildInputs = [
              pkgs.zlib
            ];

            dontConfigure = true;
            dontFixup = true;

            buildPhase = ''
              runHook preBuild

              export HOME="$TMPDIR"
              export CABAL_DIR="$TMPDIR/.cabal"

              cabal build exe:heft \
                --project-file=cabal.project.release \
                --offline

              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall

              mkdir -p "$out/bin"

              binary=$(cabal list-bin exe:heft \
                --project-file=cabal.project.release)

              cp "$binary" "$out/bin/heft"
              chmod +w "$out/bin/heft"

              ${
                if pkgs.stdenv.isDarwin then
                  ''
                    strip -x "$out/bin/heft"
                  ''
                else
                  ''
                    strip --strip-all "$out/bin/heft"
                  ''
              }

              chmod +x "$out/bin/heft"

              runHook postInstall
            '';
          };
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
