{ nixpkgs ? import ./nixpkgs {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  filterSource =
    let
      inherit (pkgs.haskell.lib) overrideCabal;
      overrideSrc = drv: f:
        overrideCabal drv (args: args // { src = f args.src; });
    in
      drv: pred: overrideSrc drv (src: builtins.filterSource pred src);

  omitDirs =
    let
      blacklistDirs = [ ".git" "dist" "nixpkgs" ];
      whitelistExts = [ ".cabal" ".hs" ];
      whitelistNames = [ "LICENSE" ];

      predicate = path: type:
        let inherit (nixpkgs.lib) any elem hasSuffix; in
        let baseName = baseNameOf path; in
        if type == "directory"
          then !(elem baseName blacklistDirs)
          else any (suf: hasSuffix suf baseName) whitelistExts
            || any (name: baseName == name) whitelistNames;
    in
      drv: filterSource drv predicate;

  drv = omitDirs (haskellPackages.callPackage ./repos.nix {
    yaml =
      let inherit (pkgs.haskell.lib) dontCheck; in
      dontCheck haskellPackages.yaml_0_8_23_1;
  });

in

  if pkgs.lib.inNixShell then drv.env else drv
