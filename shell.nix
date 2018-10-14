{ pinned      ? false
, nixpkgs     ? import (if pinned
                        then ./nix/nixpkgs
                        else  <nixpkgs>) {}
, compiler    ? import ./nix/default-compiler.nix
, tools       ? false
, local       ? false
}:
let
  pkgs    = nixpkgs.pkgs;
  drv     = import ./nix/package.nix  { inherit pinned nixpkgs compiler local; };
  ghc     = import ./nix/ghc.nix      { inherit        nixpkgs compiler local; };
  extras  =  [
               pkgs.cabal-install
             ];
  drv'    = pkgs.haskell.lib.overrideCabal
            drv
            (old: {
              libraryHaskellDepends   = old.libraryHaskellDepends ++ extras;
              doHaddock               = false;
             });
in
  drv'.env
