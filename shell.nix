let
  pkgs = import ./. {};

in

pkgs.shellFor {
  tools = {
    cabal = "3.0.0.0";
    hpack = "0.33.0";
    tasty-discover = "4.2.1";
  };
}
