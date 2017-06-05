{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;
  inherit (pkgs.haskell.lib) dontCheck;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage ./repos.nix {
    yaml = dontCheck haskellPackages.yaml_0_8_23;
  };

in

  if pkgs.lib.inNixShell then drv.env else drv
