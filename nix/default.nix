{ mkDerivation
, stdenv

  # haskell deps
, base
, deepseq
, instant-generics
, tasty
, tasty-quickcheck
}:

mkDerivation {
  pname = "instant-deepseq";
  version = "0.2";
  homepage = "https://github.com/k0001/instant-deepseq";
  description = "Generic NFData instances through instant-generics";
  license = stdenv.lib.licenses.bsd3;
  src = ../.;
  isLibrary = true;
  isExecutable = false;
  doHaddock = true;
  buildDepends = [base deepseq instant-generics tasty tasty-quickcheck];
}
