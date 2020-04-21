let
  pkgs = import ./. {};

in

pkgs.shellFor {
  buildInputs = [
    pkgs.hpack.components.exes.hpack
    pkgs.doctest.components.exes.doctest
  ];
}
