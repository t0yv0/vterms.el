{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-24.05;
    nixpkgs_darwin.url = github:NixOS/nixpkgs/nixpkgs-24.05-darwin;
  };

  outputs = { self, nixpkgs, nixpkgs_darwin }: let

    version = self.rev or "dirty";

    packages = nixpkgs: sys: emacs-flavor: let
      pkgs = import nixpkgs { system = sys; };
      epkgs = pkgs.emacsPackagesFor (emacs-flavor pkgs);

      vterms = epkgs.elpaBuild {
        pname = "vterms";
        ename = "vterms";
        version = version;
        src = [ ./vterms.el ];
        packageRequires = [ epkgs.vterm ];
        meta = {};
      };

    in {
      default = vterms;
    };

  in {
    packages = {
      "x86_64-darwin" = packages nixpkgs_darwin "x86_64-darwin"  (pkgs: pkgs.emacs29-macport);
      "aarch64-darwin" = packages nixpkgs_darwin "aarch64-darwin" (pkgs: pkgs.emacs29-macport);
      "x86_64-linux" = packages nixpkgs "x86_64-linux" (pkgs: pkgs.emacs29);
      "aarch64-linux" = packages nixpkgs "aarch64-linux" (pkgs: pkgs.emacs29);
    };
  };
}
