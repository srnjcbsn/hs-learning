{ mkDerivation, ansi-terminal, base, containers, directory
, filepath, hspec, mtl, parsec, pretty-show, process, QuickCheck
, stdenv, temporary
}:
mkDerivation {
  pname = "hs-learning";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    ansi-terminal base containers directory filepath hspec mtl parsec
    pretty-show process QuickCheck temporary
  ];
  testDepends = [
    base containers directory filepath hspec mtl pretty-show process
    QuickCheck temporary
  ];
  license = stdenv.lib.licenses.unfree;
}
