{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:

let
  inherit (pkgs) haskellPackages;

  haskellDeps = ps: with ps; [
    base
    megaparsec
  ];

  ghc = haskellPackages.ghcWithPackages haskellDeps;

  nixPackages = [
    ghc
    pkgs.ghcid
  ];

in pkgs.stdenv.mkDerivation {
    name = "haskell_test_env";
    buildInputs = nixPackages;
    shellHook = ''
      export LC_ALL=C.UTF-8
      export GHC_BASE=$(which ghc | cut -d '/' -f-4)
    '';
  }

