self: super:
let
  inherit (self.haskell.lib) doJailbreak;
in
{
  haskellPackages = super.haskellPackages.override (args: {
    overrides = self: super_:
      let
        overrides = args.overrides self super_;
        super = super_ // overrides;
      in
        overrides // {
          dhall = self.callPackage ./dhall.nix {};
          pipes-group = doJailbreak super.pipes-group;
        };
  });
}
