# This is used in the Travis build to install the Idris compiler.
let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
in {
  wl-pprint = stdenv.mkDerivation {
    name = "wl-pprint";
    src = ./.;
    buildInputs = with pkgs; [ haskellPackages.idris gmp ];
  };
}
