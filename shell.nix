{ pkgs ? import <nixpkgs> {} }:
let myGhc = pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
      list-filter
      split
    ]);
in
pkgs.mkShell {
  buildInputs = [ myGhc ];
}
