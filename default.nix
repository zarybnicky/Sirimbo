{ pkgs ? import <nixpkgs> {} }:
import ./olymp-api { inherit pkgs; } // {}
