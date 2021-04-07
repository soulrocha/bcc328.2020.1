{ nixpkgs ? import <nixpkgs> {} } :

let
  inherit (nixpkgs) pkgs;
  ocamlPackages = pkgs.ocamlPackages;
  #ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_10;
  #ocamlPackages = pkgs.ocamlPackages_latest;
in

pkgs.stdenv.mkDerivation {
  name = "my-ocaml-env";
  buildInputs = [
    ### tools outside of opam
    #pkgs.binutils
    #pkgs.gcc
    #pkgs.m4

    ### needed for ocaml-lsp-server
    #pkgs.clang-tools
    #pkgs.llvmPackages_latest.clang

    ### needed for llvm bindings
    #pkgs.llvmPackages_latest.llvm
    #pkgs.python2Full
    #pkgs.pkg-config
    #pkgs.cmake
    #pkgs.zlib
    #pkgs.ncurses

    pkgs.opam

    pkgs.rlwrap

    (pkgs.emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
      # pkgs.dune_2
      # pkgs.ocamlformat
    ])))

    pkgs.vscode
  ];
}
