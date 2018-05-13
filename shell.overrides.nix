self: super:
{
  haskellPackages = super.haskellPackages.override (args: {
    overrides = self: super_:
      let
        overrides = args.overrides self super_;
        super = super_ // overrides;
      in
        overrides // {
          ghcWithPackages = self.ghcWithHoogle;
        };
  });
}
