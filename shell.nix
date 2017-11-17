{ pinned      ? false
, nixpkgs     ? import (if pinned
                        then ./nixpkgs
                        else  <nixpkgs>) {}
, pkgs        ? nixpkgs.pkgs, haskell ? pkgs.haskell
, compiler    ? "ghc802"
, ghcOrig     ? pkgs.haskell.packages."${compiler}"
}:
let
  ghc       = ghcOrig.override (oldArgs: {
    overrides = with haskell.lib; new: old:
    let parent = (oldArgs.overrides or (_: _: {})) new old;
    in with new; parent // {
      # aeson      = dontHaddock (dontCheck old.aeson);
      # scientific = dontHaddock (dontCheck old.scientific);
      reflex = doJailbreak (new.callPackage
      ({ stdenv, mkDerivation, base, containers, data-default, dependent-map, dependent-sum
       , exception-transformers, filemanip, haskell-src-exts, haskell-src-meta, hlint
       , MemoTrie, lens, monad-control, mtl, primitive, prim-uniq, ref-tf, reflection, semigroups, semigroupoids, split, syb
       , template-haskell, these, transformers, transformers-compat, unbounded-delays
       }:
       mkDerivation {
           pname = "reflex";
           version = "0.5.0";
           src = pkgs.fetchFromGitHub {
             owner = "deepfire";
             repo = "reflex";
             rev = "60d2878142943487987feeeea108dbed5405f469";
             sha256 = "1gzyclfc18k881ww990fydgci3zcszy0rny0p979qdjfv4f50396";
           };
           libraryHaskellDepends = [
             base containers data-default dependent-map dependent-sum exception-transformers filemanip
             haskell-src-exts haskell-src-meta hlint lens MemoTrie monad-control mtl primitive prim-uniq ref-tf reflection
             semigroups semigroupoids split syb template-haskell these transformers transformers-compat unbounded-delays
           ];
           testHaskellDepends = [
             base containers dependent-map MemoTrie mtl ref-tf
           ];
           homepage = "https://github.com/reflex-frp/reflex";
           description = "Higher-order Functional Reactive Programming";
           license = stdenv.lib.licenses.bsd3;
           hydraPlatforms = stdenv.lib.platforms.none;
       }) {});
    };
  });
  drv = ghc.callPackage (import ./.) { };
  drv'    = haskell.lib.overrideCabal
            drv
            (old: {
              libraryHaskellDepends =
                old.libraryHaskellDepends
                ++ [ pkgs.cabal-install pkgs.stack ghc.intero ];
             });
in
  if nixpkgs.lib.inNixShell then drv'.env else drv'
