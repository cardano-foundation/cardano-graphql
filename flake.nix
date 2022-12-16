{
  inputs = {
    nixpkgs.follows = "tullia/nixpkgs";
    std.follows = "tullia/std";
    tullia.url = github:input-output-hk/tullia;
    flake-compat = {
      url = "github:input-output-hk/flake-compat/fixes";
      flake = false;
    };
    haskellNix = {
      url = github:input-output-hk/haskell.nix/fix-input-map-support-with-no-rev;
    };
    graphql-engine = {
      url = github:hasura/graphql-engine/v2.16.1;
    };
    # graphql-engine dependencies:
    kriti-lang = {
      url = github:hasura/kriti-lang/c8fd863d359876af3a0fce47db8a5393dc556667;
      flake = false;
    };
    ci-info-hs = {
      url = github:hasura/ci-info-hs/be578a01979fc95137cc2c84827f9fafb99df60f;
      flake = false;
    };
    pool = {
      url = github:hasura/pool/c5faf9a358e83eaf15fef0c1e890f463d0022565;
      flake = false;
    };
    odbc = {
      url = github:fpco/odbc/38e04349fe28a91f189e44bd7783220956d18aae;
      flake = false;
    };
    ekg-core = {
      url = github:hasura/ekg-core/b0cdc337ca2a52e392d427916ba3e28246b396c0;
      flake = false;
    };
    ekg-prometheus = {
      url = github:hasura/ekg-prometheus/v0.2.0.0;
      flake = false;
    };
    ekg-json = {
      url = github:hasura/ekg-json/1fab6837e1dd98317a3c2f5bf9deedf4dfcac51b;
      flake = false;
    };
    text = {
      url = github:hasura/text/ba0fd2bf256c996a6c85dbdc8590a6fcde41b8f8;
      flake = false;
    };
  };

  outputs = {
    self,
    std,
    tullia,
    ...
  } @ inputs:
    std.growOn {
      inherit inputs;
      cellsFrom = nix/cells;
      cellBlocks = [
        (std.functions "library")
        (std.installables "packages")
        (std.functions "hydraJobs")
        (tullia.tasks "pipelines")
        (std.functions "actions")
      ];
    }
    (
      tullia.fromStd {
        actions = std.harvest self ["cloud" "actions"];
        tasks = std.harvest self ["automation" "pipelines"];
      }
    )
    {
      hydraJobs = std.harvest self ["automation" "hydraJobs"];
    };

  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = true;
  };
}
