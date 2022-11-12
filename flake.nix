{
  description = "Language Server for Agda";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = args@{ nixpkgs, flake-utils, ... }:
    let
      ghcVersion = "902";
      compiler = "ghc${ghcVersion}";
    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.${compiler};

        alster = haskellPackages.callCabal2nix "Alster" ./. { };
        alster-app = flake-utils.lib.mkApp { name = "alster"; drv = alster; };

      in
      rec {
        packages = {
          inherit alster;
          default = alster;
        };

        defaultPackage = packages.default;
        app = {
          alster = alster-app;
          default = alster-app;
        };
        defaultApp = alster-app;

        devShells = import ./nix/dev-shells.nix {
          inherit pkgs haskellPackages;
          packages = p: [ packages.default ];
        };
      }
    );
}
