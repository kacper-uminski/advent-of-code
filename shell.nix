{ pkgs ? import <nixpkgs> {} }:
let myGhc = pkgs.haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
      split
    ]);
in
pkgs.mkShell {
  buildInputs = [ myGhc ];
}
