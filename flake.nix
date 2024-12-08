
{
  description = "NixOS System Configuration Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    zen-browser.url = "github:MarceColl/zen-browser-flake";
    ags.url = "github:Aylur/ags/v1";
    hyprpanel.url = "github:Jas-SinghFSU/HyprPanel";
  };

  outputs = { 
    self,
    nixpkgs,
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
