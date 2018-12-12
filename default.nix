{ mkDerivation, base, bytestring, Cabal, comonad, cryptonite, dhall
, directory, extra, fgl, filepath, hashtables, hpack, http-client
, http-client-tls, http-types, network, network-uri
, optparse-applicative, safe-exceptions, stdenv, text
, unordered-containers, zlib
}:
mkDerivation {
  pname = "moby";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  setupHaskellDepends = [
    base bytestring Cabal cryptonite directory extra filepath
    http-client http-client-tls http-types safe-exceptions
    unordered-containers zlib
  ];
  libraryHaskellDepends = [
    base bytestring comonad cryptonite dhall directory fgl filepath
    hashtables http-client http-client-tls http-types network-uri text
    unordered-containers zlib
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring network optparse-applicative
  ];
  testHaskellDepends = [ base ];
  preConfigure = "hpack";
  homepage = "https://github.com/stites/moby#readme";
  license = stdenv.lib.licenses.bsd3;
}
