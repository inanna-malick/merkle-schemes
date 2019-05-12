{ mkDerivation, aeson, aeson-pretty, base, bifunctors, bytestring
, containers, deriving-compat, directory, free, hpack, http-client
, lens, lens-aeson, memory, merkle-schemes-higher, mtl
, optparse-applicative, pretty, random, safe-exceptions, servant
, servant-client, servant-server, singletons, stdenv, text
, transformers, vector, warp, wreq
}:
mkDerivation {
  pname = "merkle-schemes-higher-ext";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty base bifunctors bytestring containers
    deriving-compat directory free http-client lens lens-aeson memory
    merkle-schemes-higher mtl optparse-applicative pretty random
    safe-exceptions servant servant-client servant-server singletons
    text transformers vector warp wreq
  ];
  libraryToolDepends = [ hpack ];
  preConfigure = "hpack";
  homepage = "https://github.com/pkinsky/merkle-schemes#readme";
  description = "merkle all the things (higher kinded, exts)";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
