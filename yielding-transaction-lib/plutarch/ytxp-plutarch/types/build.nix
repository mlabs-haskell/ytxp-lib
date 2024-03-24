{ inputs, ... }: {
  perSystem = { system, ... }:
    {
      packages.ytxp-lib-types-haskell = inputs.lbf.lib."${system}".lbfPlutusHaskell {
        name = "ytxp-lib-types-haskell";
        src = ./.;
        files =
          [
            "YieldList.lbf"
          ];
      };

      packages.ytxp-lib-types-purescript = inputs.lbf.lib."${system}".lbfPlutusPurescript {
        name = "ytxp-lib-types-purescript";
        src = ./.;
        files =
          [
            "YieldList.lbf"
          ];
      };
    };
}
