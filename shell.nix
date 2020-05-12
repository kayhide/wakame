{ pkgs ? import <nixpkgs> {}
}:

pkgs.mkShell {
  buildInputs = with pkgs; [
    stack
    haskellPackages.hpack
    haskellPackages.ghcid
  ];
}

