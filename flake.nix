{
  description = "shs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    # flake-utils.lib.eachDefaultSystem
    flake-utils.lib.eachSystem [ flake-utils.lib.system.x86_64-linux ] (system:
      let
        pkgs = import nixpkgs { inherit system; };

        _jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.unmarkBroken pkg));

        haskellPackages = pkgs.haskellPackages.override
          {
            overrides = hself: hsuper: {
            };
          };

        nixpkgsOverlay = _final: _prev: {
          shs = self.packages.${system}.shs;
        };

      in
      rec {
        packages.shs =
          pkgs.haskell.lib.justStaticExecutables (haskellPackages.callCabal2nix "shs" ./. {});

        packages.default = packages.shs;

        overlays = nixpkgsOverlay;

        nixosModules.default =
          { pkgs, lib, config, ... }:
          let
            cfg = config.services.shs;
          in
          {
            options = with lib; {
                services.shs = {
                  enable = mkEnableOption "Simple HTTP Haskell Server";

                  package = lib.mkPackageOption pkgs "shs" {};

                  openFirewall = lib.mkOption {
                    type = types.bool;
                    default = false;
                  };

                  user = mkOption {
                    type = types.str;
                  };

                  group = mkOption {
                    type = types.str;
                  };

                  host = mkOption {
                    type = types.str;
                    default = "127.0.0.1";
                    description = "Listening host";
                  };

                  port = mkOption {
                    type = types.port;
                    default = 8080;
                    description = "Listening port";
                  };

                  baseUrl = mkOption {
                    type = types.str;
                    default = "/";
                    description = "Base URL";
                  };

                  root = mkOption {
                    type = types.path;
                    description = "Root directory";
                  };

                  sortingOrder = mkOption {
                    type = types.enum [ "asc" "desc" ];
                    default = "asc";
                    description = "Sorting order";
                  };

                  sortingField = mkOption {
                    type = types.enum [ "name" "mtime" "size" ];
                    default = "name";
                    description = "Sorting field";
                  };

                  dateFormat = mkOption {
                    type = types.enum [ "absolute" "relative" ];
                    default = "absolute";
                    description = "Date format";
                  };
              };
            };

            config = lib.mkIf cfg.enable {
              nixpkgs.overlays = [ nixpkgsOverlay ];

              networking.firewall.allowedTCPPorts = lib.optional cfg.openFirewall cfg.listenPort;

              systemd.services.shs = {
                description = "Simple HTTP Haskell Server";
                after = [ "network.target" ];
                wantedBy = [ "multi-user.target" ];
                serviceConfig = {
                  User = cfg.user;
                  Group = cfg.group;
                  ExecStart = ''
                    ${lib.getExe cfg.package} \
                      --host ${cfg.host} \
                      --port ${toString cfg.port} \
                      --base-url ${cfg.baseUrl} \
                      --root ${cfg.root} \
                      --sorting-order ${cfg.sortingOrder} \
                      --sorting-field ${cfg.sortingField} \
                      --date-format ${cfg.dateFormat}
                  '';
                  Restart = "always";
                };
              };
            };
          };

        devShells.default =
          pkgs.mkShell {
            buildInputs = with haskellPackages; [
              haskell-language-server
              ghcid
              cabal-install
              ormolu
            ];
            inputsFrom = [ self.packages.${system}.default.env ];
          };
      });
}
