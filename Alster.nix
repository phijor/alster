{ mkDerivation, Agda, base, co-log-core, either, lens, lib, lsp
, mtl, text, transformers, unliftio-core
}:
mkDerivation {
  pname = "Alster";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    Agda base co-log-core either lens lsp mtl text transformers
    unliftio-core
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  license = lib.licenses.mit;
  mainProgram = "Alster";
}
