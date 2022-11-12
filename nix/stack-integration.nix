{ ghc }:

let
  pkgs = import <nixpkgs> { };
in
pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "foo";
  buildInputs = [ pkgs.zlib ];
}
