{
    description = "Chilly's NixOS Systems Configuration Flake";

    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    };

    outputs = {
        self,
        nixpkgs,
        ...
    } @ inputs : let 

    system = "x86_64-linux";

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
    };
}
