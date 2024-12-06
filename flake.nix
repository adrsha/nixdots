
{
  description = "NixOS System Configuration Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    zen-browser.url = "github:MarceColl/zen-browser-flake";
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
