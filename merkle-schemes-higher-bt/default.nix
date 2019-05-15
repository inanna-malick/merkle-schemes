{ mkDerivation, aeson, base, base64-bytestring, bytestring
, deriving-compat, merkle-schemes-higher, singletons, stdenv
, text
}:
mkDerivation {
  pname = "merkle-schemes-higher-bt";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base64-bytestring bytestring deriving-compat
    merkle-schemes-higher singletons text
  ];
  homepage = "https://github.com/pkinsky/merkle-schemes#readme";
  description = "merkle all the things";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
