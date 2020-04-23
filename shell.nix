let
  pkgs = import ./. {};

in

pkgs.shellFor {
  buildInputs = [
    pkgs.hpack.components.exes.hpack
    pkgs.doctest.components.exes.doctest
    pkgs.tasty-discover.components.exes.tasty-discover
  ];
}
