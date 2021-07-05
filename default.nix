{ nixpkgs ? import <nixpkgs> {}
, stdenv
, system
, racket
}:
stdenv.mkDerivation {
  name = "melodeon";
  inherit system;

  buildInputs = [ racket ];
  buildCommand = ''
    raco pkg install json-type-provider
  '';
}
