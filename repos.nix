{ mkDerivation, base, bytestring, containers, dhall
, insert-ordered-containers, optparse-applicative, stdenv
, system-filepath, text, text-format, trifecta, turtle, unix
, vector
}:
mkDerivation {
  pname = "repos";
  version = "0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers dhall insert-ordered-containers
    optparse-applicative system-filepath text text-format trifecta
    turtle unix vector
  ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
