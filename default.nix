{ pkgs ? import <nixpkgs> {} }:

let
  inherit pkgs;
  purescript = pkgs.haskellPackages.callPackage ./purescript.nix {};
in (import ./purescript-modules/default.nix {
  pkgs = pkgs;
  purescript = purescript;
  stdenv = pkgs.stdenv;
}).callPackage ({ mkDerivation, prelude, eff, console }:
  mkDerivation {
    pname = "mine";
    version = "0.1.0";
    src = builtins.filterSource (path: type:
      type != "unknown"
      && baseNameOf path != ".git"
      && baseNameOf path != "result"
      && baseNameOf path != "output"
    ) ./.;
    purescriptDepends = [ prelude eff console ];
    systemDepends = [ pkgs.nodejs ];
    shellHook = ''
      export PATH=node_modules/.bin:$PATH
    '';
  }
) {}
