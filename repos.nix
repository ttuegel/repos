*** found package.yaml. Using hpack...
{ mkDerivation, base, bytestring, containers, dhall, formatting
, hpack, insert-ordered-containers, optparse-applicative, stdenv
, system-filepath, text, trifecta, turtle, unix, vector
}:
mkDerivation {
  pname = "repos";
  version = "0.3.4";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring containers dhall formatting
    insert-ordered-containers optparse-applicative system-filepath text
    trifecta turtle unix vector
  ];
  preConfigure = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
