{
  description = "Stronger classes than monad-control";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "master";
    };
  };

  outputs = { self, nixpkgs }: {

    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      let src = nix-gitignore.gitignoreSource [] ./.;
      in haskellPackages.callCabal2nix "monad-control-identity" src {};

    devShell.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      haskellPackages.shellFor {
        buildInputs = with haskellPackages; [
          cabal-install
          ghcid
          haskell-language-server
          hlint
          hnix
          implicit-hie
          rnix-lsp
          weeder
        ];
        packages = haskellPackages: [
          self.defaultPackage.x86_64-linux
        ];
        withHoogle = true;
      };

  };
}
