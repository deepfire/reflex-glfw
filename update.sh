#!/bin/sh

who=${1:-deepfire}

cabal2nix . > nix/default.nix

if test -n "$1"
then shift
fi
for x in "$@"
do case ${who} in
     local | home ) rbase=file://$HOME; who='';;
     * )            rbase=https://github.com/ ;;
   esac
   nix-prefetch-git $rbase${who}/$x > nix/$x/$x.src.json
done
