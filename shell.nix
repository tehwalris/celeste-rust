{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    cargo
    rustc
    rust-analyzer
    rustfmt
  ];
}
