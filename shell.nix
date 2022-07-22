{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  name = "elm-time2";
  buildInputs = with pkgs; [ entr fd just ];
}
