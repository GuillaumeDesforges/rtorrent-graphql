let
  pkgs = import (
    builtins.fetchGit {
      url = "https://github.com/NixOS/nixpkgs";
      rev = "41c8f3c356ea8af2c3db3c83bc8a64416f641b58";
    }
  ) {};
in
pkgs.mkShell {
  buildInputs = [
    # dev env
    pkgs.haskell-language-server
    pkgs.stack
    # rTorrent program to run tests
    pkgs.rtorrent
  ];
}
