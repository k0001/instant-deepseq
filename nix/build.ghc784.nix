# this file can be used with nix-build

with (import <nixpkgs> {}).pkgs;
with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; });

let hs = haskell-ng.packages.ghc784.override {
    overrides = self: super: {
      instant-deepseq = self.callPackage ./default.nix {};
    };
  };
in hs.instant-deepseq
