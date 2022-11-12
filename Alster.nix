{ mkDerivation, Agda, base, lens, lib, lsp, text, unliftio-core }:
mkDerivation {
  pname = "Alster";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ Agda base lens lsp text unliftio-core ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base lens ];
  license = lib.licenses.mit;
  mainProgram = "Alster";
}
