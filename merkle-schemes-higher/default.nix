{ mkDerivation, aeson, base, base16-bytestring, base64-bytestring
, bifunctors, bytestring, containers, deriving-compat, directory
, free, hpack, memory, mtl, pretty, random, safe-exceptions
, singletons, stdenv, text, transformers, unordered-containers
}:
mkDerivation {
  pname = "merkle-schemes-higher";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base64-bytestring
    bytestring containers deriving-compat free
    random safe-exceptions singletons text
    unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  preConfigure = "hpack";
  homepage = "https://github.com/pkinsky/merkle-schemes#readme";
  description = "merkle all the things";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
