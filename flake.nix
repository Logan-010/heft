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

          #Nix has mostly old ass hackage packages so many need to be updated
          hs =
            hsuper:
            pkgs.haskellPackages.override {
              # Skip library test suites and their test-only dependencies.
              mkDerivation = args: hsuper.mkDerivation (args // { doCheck = false; });
              #overrides = hself: hsuper: {
              #  pkgname = pkgs.haskellPackages.callHackageDirect {
              #    pkg = "pkgname";
              #    ver = "pkgversion";
              #    sha256 = pkgs.lib.fakeHash;
              #  } { };
              #  pkgname2 = pkgs.haskellPackages.callHackage "pkgname2" "pkgversion2" { };
              #};
            };
        in
        {
          # Add dependencies
          default = pkgs.haskell.lib.addExtraLibraries (hs.callCabal2nix "project" ./. { }) [ pkgs.zlib ];
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

              # Language server
              haskell-language-server

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
              pkg-config
            ];
          };
        }
      );
    };
}
