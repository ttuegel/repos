let
  inherit ((import <nixpkgs> {}).pkgs) lib;
  lock = builtins.fromJSON (builtins.readFile ./nixpkgs.lock.json);
  bootstrap = builtins.fetchTarball {
    url = lock.url + "/archive/${lock.rev}.tar.gz";
    inherit (lock) sha256;
  };
  defaultOverrides =
    let file = ./default.overrides.nix; in
    lib.optional
    (builtins.pathExists file)
    (import file);
  shellOverrides =
    let file = ./shell.overrides.nix; in
    lib.optional
    (lib.inNixShell && builtins.pathExists file)
    (import file);
  userShellOverrides =
    let
      file =
        builtins.getEnv "HOME" + "/.config/nixpkgs/shell.overrides.nix";
    in
      lib.optional
      (lib.inNixShell && builtins.pathExists file)
      (import file);
in
  import bootstrap
  {
    config.allowUnfree = true;
    overlays = defaultOverrides ++ shellOverrides ++ userShellOverrides;
  }
