{
  inputs.nixpkgs.url = "github:nix-ocaml/nix-overlays";

  outputs = {
    self,
    nixpkgs,
  }: let
    system = "x86_64-linux";
    pkgs = nixpkgs.legacyPackages.${system};
    fs = pkgs.lib.fileset;
  in {
    defaultApp.${system} = pkgs.hello;
    packages.${system}.default = with pkgs.ocamlPackages;
      buildDunePackage {
        pname = "caju";
        version = "0.0.1";
        src = fs.toSource {
          root = ./.;
          fileset = fs.unions [
            ./dune-project
            ./caju.opam
            ./bin
            ./lib
            ./test
          ];
        };
        propagatedBuildInputs = [ocaml fmt];
        doCheck = true;
        checkInputs = [alcotest];
        checkPhase = ''
          dune runtest
        '';
      };
    devShell.${system} = pkgs.mkShell {
      buildInputs = with pkgs.ocamlPackages; [ocaml findlib dune utop ocamlformat ocaml-lsp];
      propagatedBuildInputs = with pkgs.ocamlPackages; [alcotest];
    };
  };
}
