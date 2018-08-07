{ mkDerivation, base, bytestring, containers, dhall, formatting
, insert-ordered-containers, optparse-applicative, stdenv
, system-filepath, text, trifecta, turtle, unix, vector
}:
mkDerivation {
  pname = "repos";
  version = "0.3.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers dhall formatting
    insert-ordered-containers optparse-applicative system-filepath text
    trifecta turtle unix vector
  ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
