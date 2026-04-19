{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    inputs@{ flake-parts, nixpkgs, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem =
        { system, ... }:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ (import ./overlays/tgls.nix) ];
          };
        in {
          devShells.default = pkgs.mkShell {
            packages = with pkgs; [
              ocaml
              opam
              dune_3
              ocamlPackages.findlib
              ocamlPackages.ocaml-lsp
              ocamlPackages.ocamlformat
              ocamlPackages.utop
              ocamlPackages.ounit2
              ocamlPackages.ppxlib
              ocamlPackages.bisect_ppx
              ocamlPackages.batteries
              ocamlPackages.odoc
              ocamlPackages.merlin
              ocamlPackages.topkg
              ocamlPackages.ocamlbuild
              ocamlPackages.tsdl
              ocamlPackages.ppx_blob
              ocamlPackages.tgls
              SDL2
              glslang
            ];
          };
        };
    };
}
