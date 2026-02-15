{ CHaP, indexState, pkgs, mkdocs, asciinema, ... }:

let
  indexTool = { index-state = indexState; };
  fix-libs = { lib, pkgs, ... }: {
    packages.cardano-crypto-praos.components.library.pkgconfig =
      lib.mkForce [ [ pkgs.libsodium-vrf ] ];
    packages.cardano-crypto-class.components.library.pkgconfig =
      lib.mkForce [[ pkgs.libsodium-vrf pkgs.secp256k1 pkgs.libblst ]];
  };
  shell = { pkgs, ... }: {
    tools = {
      cabal = indexTool;
      cabal-fmt = indexTool;
      haskell-language-server = indexTool;
      hoogle = indexTool;
      fourmolu = indexTool;
      hlint = indexTool;
    };
    withHoogle = true;
    buildInputs = [
      pkgs.just
      pkgs.nixfmt-classic
      pkgs.shellcheck
      pkgs.mkdocs
      mkdocs.from-nixpkgs
      mkdocs.asciinema-plugin
      mkdocs.markdown-callouts
      asciinema.compress
      asciinema.resize
      pkgs.asciinema
    ];
    shellHook = ''
      echo "Entering haskell-mpfs dev shell"
    '';
  };

  mkProject = ctx@{ lib, pkgs, ... }: {
    name = "haskell-mpfs";
    src = ./..;
    compiler-nix-name = "ghc984";
    shell = shell { inherit pkgs; };
    modules = [ fix-libs ];
    inputMap = { "https://chap.intersectmbo.org/" = CHaP; };
  };

  project = pkgs.haskell-nix.cabalProject' mkProject;

in {
  devShells.default = project.shell;
  inherit project;
  packages.haskell-mpfs =
    project.hsPkgs.haskell-mpfs.components.library;
  packages.unit-tests =
    project.hsPkgs.haskell-mpfs.components.tests.unit-tests;
  packages.cardano-mpfs-offchain =
    project.hsPkgs.cardano-mpfs-offchain.components.library;
  packages.offchain-tests =
    project.hsPkgs.cardano-mpfs-offchain.components.tests.unit-tests;
}
