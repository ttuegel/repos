{ mkDerivation, base, optparse-applicative, stdenv, system-filepath
, text, turtle, unix, vector, yaml
}:
mkDerivation {
  pname = "repos";
  version = "0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base optparse-applicative system-filepath text turtle unix vector
    yaml
  ];
  license = stdenv.lib.licenses.unfree;
}
