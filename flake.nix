{
    description = "Optimizing a SECD machine";

    inputs = {
        nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
	};

    outputs = { self, ... } @ inputs: 
    let
        pkgs = import inputs.nixpkgs { inherit system; };
        ROOT = builtins.getEnv "PWD";
        name = "SECD_Optimization";
        system = "x86_64-linux";
    in {
        devShells."${system}".default = pkgs.mkShell {
            inherit name ROOT;

            buildInputs = with pkgs; [
                haskell.compiler.ghc96
                haskellPackages.cabal-install 
                happy
            ];

            shellHook = ''
                export NIX_SHELL_NAME="${name}" 
                echo -ne "\033]0;${name}\007"

                alias build="cabal --enable-nix build"
                alias run="cabal --enable-nix run -v0"
                alias test="cabal --enable-nix test --test-show-details=streaming --test-option=--color"
            '';

        };
    };
}
