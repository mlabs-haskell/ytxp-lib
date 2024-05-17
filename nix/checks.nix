{
  imports = [
    ./pre-commit.nix
  ];
  perSystem = { lib, config, pkgs, ... }: {
    checks = {
      run-serialization-test = pkgs.runCommandNoCC "run-serialization-test" { } ''
        ${lib.getExe config.packages."ytxp-plutarch:test:serialization"} && touch $out
      '';
    };
  };
}
