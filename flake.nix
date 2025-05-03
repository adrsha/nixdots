
{
    description = "NixOS System Configuration Flake";

    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
        zen-browser.url = "github:MarceColl/zen-browser-flake";

        lanzaboote = {
            url = "github:nix-community/lanzaboote";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        prism.url = "github:Diegiwg/PrismLauncher-Cracked";
        matugen = {
            url = "github:/InioX/Matugen";

        };   
        ags = {
            url = "github:aylur/ags";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        astal = {
            url = "github:aylur/astal";
            inputs.nixpkgs.follows = "nixpkgs";
        };
        battery-notifier = {
            url = "github:aylur/battery-notifier";
            inputs.nixpkgs.follows = "nixpkgs";
        };
    };

    outputs = { 
        self,
        nixpkgs,
        lanzaboote,
        ...  
    } @ inputs: let

    system = "x86_64-linux";

    pkgs = nixpkgs.legacyPackages.${system};
    in {

        nixosConfigurations = {
            nixos = nixpkgs.lib.nixosSystem {
                inherit system;

                specialArgs = { inherit inputs; };

                modules = [
                    ./configuration.nix
                        ./hardware-configuration.nix
                        lanzaboote.nixosModules.lanzaboote
                        ({ pkgs, lib, ... }: {
# Lanzaboote currently replaces the systemd-boot module.
# This setting is usually set to true in configuration.nix
# generated at installation time. So we force it to false
# for now.
                         boot.loader.systemd-boot.enable = lib.mkForce false;

                         boot.lanzaboote = {
                         enable = true;
                         pkiBundle = "/var/lib/sbctl";
                         };
                         })
                ];
            };
        };

# NOT a fan

#   homeConfigurations."chilly" = inputs.home-manager.lib.homeManagerConfiguration {
#     inherit pkgs;
#
#     extraSpecialArgs = {inherit inputs;};
#     modules = [ ./home.nix ];
#
#   };
#
    };

}
