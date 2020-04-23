let
  rev = "0ef4b08b96cc6d57943a9fdc9b4499183b299f11"; # 2020-04-23 01:17:07
  haskellNix = import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/${rev}.tar.gz") {};
  nixpkgsSrc = haskellNix.sources.nixpkgs-1909;
  nixpkgsArgs = haskellNix.nixpkgsArgs;

in

{ pkgs ? import nixpkgsSrc nixpkgsArgs
}:

pkgs.haskell-nix.stackProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "wakame";
    src = ./.;
  };
  modules = [
    { reinstallableLibGhc = true;
    }
  ];
}
