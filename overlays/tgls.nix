# tgls is not bundled in nixpkgs so we build it ourselves
final: prev: {
  ocamlPackages = prev.ocamlPackages.overrideScope (ofinal: oprev: {
    tgls = prev.stdenv.mkDerivation rec {
      pname = "ocaml${oprev.ocaml.version}-tgls";
      version = "0.9.1";

      src = prev.fetchurl {
        url = "https://erratique.ch/software/tgls/releases/tgls-${version}.tbz";
        hash = "sha256-5NmaZ03oECC420ceaDtcYDyY7gbxLReDQdnLIBFSH1Y=";
      };

      strictDeps = true;

      nativeBuildInputs = with oprev; [
        ocaml
        findlib
        ocamlbuild
        topkg
        prev.opaline
      ];

      buildInputs = with oprev; [
        topkg
        ctypes
        ctypes-foreign
      ];

      propagatedBuildInputs = with oprev; [
        ctypes
        ctypes-foreign
      ];

      createFindlibDestdir = true;
      buildPhase = "${oprev.topkg.run} build --with-gl true --with-gles false";
      installPhase = "${prev.opaline}/bin/opaline -prefix $out -libdir $OCAMLFIND_DESTDIR";
    };
  });
}
