{ mkDerivation, aeson, aeson-pretty, base, base16-bytestring
, base64-bytestring, bifunctors, bytestring, containers, cryptonite
, deriving-compat, directory, free, hedgehog, hpack, hspec
, hspec-discover, http-client, hw-hspec-hedgehog, lens, lens-aeson
, memory, mtl, optparse-applicative, pretty, random
, recursion-schemes, safe-exceptions, servant, servant-client
, servant-server, singletons, stdenv, temporary, text, transformers
, unordered-containers, vector, warp, wreq
}:
mkDerivation {
  pname = "merkle-schemes";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson aeson-pretty base base16-bytestring base64-bytestring
    bifunctors bytestring containers cryptonite deriving-compat
    directory free hedgehog http-client lens lens-aeson memory mtl
    optparse-applicative pretty random recursion-schemes
    safe-exceptions servant servant-client servant-server singletons
    text transformers unordered-containers vector warp wreq
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson aeson-pretty base base16-bytestring base64-bytestring
    bifunctors bytestring containers cryptonite deriving-compat
    directory free hedgehog hspec http-client hw-hspec-hedgehog lens
    lens-aeson memory mtl optparse-applicative pretty random
    recursion-schemes safe-exceptions servant servant-client
    servant-server singletons temporary text transformers
    unordered-containers vector warp wreq
  ];
  testToolDepends = [ hspec-discover ];
  preConfigure = "hpack";
  homepage = "https://github.com/pkinsky/merkle-schemes#readme";
  description = "merkle all the things";
  license = stdenv.lib.licenses.bsd3;
}
