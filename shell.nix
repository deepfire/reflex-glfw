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
      reflex     = dontHaddock (dontCheck (new.callPackage
      ({ stdenv, mkDerivation, base, containers, data-default, dependent-map, dependent-sum
       , exception-transformers, haskell-src-exts, haskell-src-meta, hlint
       , MemoTrie, lens, monad-control, mtl, primitive, prim-uniq, ref-tf, reflection, semigroups, split, syb
       , template-haskell, these, transformers, transformers-compat
       }:
       mkDerivation {
           pname = "reflex";
           version = "0.5.0";
           src = pkgs.fetchFromGitHub {
             owner = "deepfire";
             repo = "reflex";
             rev = "ca928573e6d1a17fe02de2d89d410db8f24d34e8";
             sha256 = "1lmgfiz76cv5bh8ri9v7k2djzjd9rmm8lhgw8kd9x7hh9331f15a";
           };
           libraryHaskellDepends = [
             base containers data-default dependent-map dependent-sum exception-transformers
             haskell-src-exts haskell-src-meta hlint lens MemoTrie monad-control mtl primitive prim-uniq ref-tf reflection
             semigroups split syb template-haskell these transformers transformers-compat
           ];
           testHaskellDepends = [
             base containers dependent-map MemoTrie mtl ref-tf
           ];
           homepage = "https://github.com/reflex-frp/reflex";
           description = "Higher-order Functional Reactive Programming";
           license = stdenv.lib.licenses.bsd3;
           hydraPlatforms = stdenv.lib.platforms.none;
       }) {}));
    };
  });
  drv = (haskell.lib.addBuildTools
  (ghc.callPackage (import ./.) { })
  [ pkgs.cabal-install
    pkgs.stack
    ghc.intero
  ]);
in
  if nixpkgs.lib.inNixShell then drv.env else drv
