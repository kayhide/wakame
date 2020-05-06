let
  rev = "49750aa6cbad6f0bd38398c9338037e30a2b60f5"; # 2020-05-01 01:17:32
  haskellNix = import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/${rev}.tar.gz") {};
  nixpkgsSrc = haskellNix.sources.nixpkgs-1909;

  packagesOverlay = self: super: {
    buildPackages = super.buildPackages // {
      alex = super.haskellPackages.alex;
      happy = super.haskellPackages.happy;
    };
  };

  nixpkgsArgs = haskellNix.nixpkgsArgs // {
    overlays = haskellNix.nixpkgsArgs.overlays ++ [
      packagesOverlay
    ];
  };
in

{ pkgs ? import nixpkgsSrc nixpkgsArgs
}:

pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "wakame";
    src = ./.;
  };
  ghc = pkgs.haskell-nix.compiler.ghc865;
  modules = [
    { reinstallableLibGhc = true;
    }
  ];
}
