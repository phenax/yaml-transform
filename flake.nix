{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, lib, config, ... }: {
        haskellProjects.default = {
          projectRoot = builtins.toString (lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./src
              ./yaml-transform.cabal
              # ./LICENSE
              # ./README.md
            ];
          });

          packages = {
            # aeson.source = "1.5.0.0"; # Hackage version override
            # shower.source = inputs.shower;
          };
          settings = {
            #  aeson = {
            #    check = false;
            #  };
            #  relude = {
            #    haddock = false;
            #    broken = false;
            #  };
          };

          devShell = {
            # tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };
            hlsCheck.enable = pkgs.stdenv.isDarwin;
          };

          autoWire = [ "packages" "apps" "checks" ];
        };

        packages.default = self'.packages.yaml-transform;
        apps.default = self'.apps.yaml-transform;

        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
          packages = with pkgs; [
            just
          ];
        };
      };
    };
}
