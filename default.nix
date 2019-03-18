let nixpkgs = import ./nixpkgs.nix; in

let

  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages lib;


  blacklistDirs = [ ".git" "dist" "dist-newstyle" ];
  whitelistExts = [ ".cabal" ".hs" ];
  whitelistNames = [ "LICENSE" "package.yaml" ];

  filterSrc =
    let
      overrideSrc = drv: f:
        let inherit (pkgs.haskell.lib) overrideCabal; in
        overrideCabal drv (args: args // { src = f args.src; });
      predicate = path: type:
        let inherit (lib) any elem hasSuffix; in
        let baseName = baseNameOf path; in
        if type == "directory"
          then !(elem baseName blacklistDirs)
          else any (suf: hasSuffix suf baseName) whitelistExts
            || any (name: baseName == name) whitelistNames;
    in
      drv: overrideSrc drv (src: builtins.filterSource predicate src);

  drv = filterSrc (haskellPackages.callPackage ./package.nix {});

in

  if lib.inNixShell then drv.env else drv
