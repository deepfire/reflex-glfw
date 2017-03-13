{ nixpkgs     ? import ./nixpkgs {}
#, nixpkgs     ? import <nixpkgs> {}
, pkgs        ? nixpkgs.pkgs, haskell ? pkgs.haskell
, compiler    ? "ghc801"
, ghcOrig     ? pkgs.haskell.packages."${compiler}"
}:
let
  ghc       = ghcOrig.override (oldArgs: {
    overrides = with haskell.lib; new: old:
    let parent = (oldArgs.overrides or (_: _: {})) new old;
    in with new; parent // {
      reflex = new.callPackage
      ({ stdenv, mkDerivation, base, containers, data-default, dependent-map, dependent-sum
       , exception-transformers, haskell-src-exts, haskell-src-meta, hlint
       , MemoTrie, lens, monad-control, mtl, primitive, prim-uniq, ref-tf, reflection, semigroups, split, syb
       , template-haskell, these, transformers, transformers-compat
       }:
       mkDerivation {
           pname = "reflex";
           version = "0.5.0";
           src = pkgs.fetchFromGitHub {
             owner = "reflex-frp";
             repo = "reflex";
             rev = "d78ba4318c425ca9b942dc387d7c5c7ab2d2e095";
             sha256 = "10sryvwdf88ajkp35yma8llkb38cp63vjr5mq2hba4s2d8yg649q";
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
       }) {};
    };
  });
in

(haskell.lib.addBuildTools
  (ghc.callPackage (import ./.) { })
  [ pkgs.cabal-install
    pkgs.stack
    ghc.intero
  ]).env
