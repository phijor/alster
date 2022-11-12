{ pkgs,
  haskellPackages ? pkgs.haskellPackages,
  packages,
  ...
}:
let
  cabal = pkgs.symlinkJoin {
    name = "cabal";
    paths = [ haskellPackages.cabal-install ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/cabal \
        --add-flags "\
          --enable-nix \
        "
    '';
  };
  buildInputs = [
    cabal
    haskellPackages.haskell-language-server
    haskellPackages.implicit-hie

    pkgs.pkgconfig
    pkgs.zlib.dev
    pkgs.zlib.out
    pkgs.icu
  ];

  extraTestInputs = [
  ];

  mkShell = buildInputs:
    haskellPackages.shellFor {
      inherit packages buildInputs;

      # Ensure nix commands do not use the global <nixpkgs> channel:
      NIX_PATH = "nixpkgs=" + pkgs.path;

      # Ensure system libraries (zlib.so, etc.) are visible to GHC:
      LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildInputs;
    };
in

{
  default = mkShell buildInputs;
  testShell = mkShell (buildInputs ++ extraTestInputs);
}
