{
  imports = [
    ./pre-commit.nix
  ];
  perSystem = { lib, config, pkgs, ... }: {
    checks = {
      run-ytxp-plutarch-test = pkgs.runCommandNoCC "run-ytxp-plutarch-test" { } ''
        ${lib.getExe config.packages."ytxp-plutarch:test:ytxp-lib-test"} && touch $out
      '';
      run-ytxp-sdk-test = pkgs.runCommandNoCC "run-ytxp-sdk-test" { } ''
        ${lib.getExe config.packages."ytxp-sdk:test:ytxp-sdk-test"} && touch $out
      '';
    };
  };
}
