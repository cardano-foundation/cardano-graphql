{ inputs
, cell
,
}:
let
  inherit
    (inputs)
    std
    nixpkgs
    ;
  filteredSrc = std.incl inputs.self [
    "yarn.lock"
    ".yarnrc"
    "package.json"
    "packages"
    "packages-cache"
    "tsconfig.json"
    "docker-compose.yml"
  ];

in {
  cardano-graphql = nixpkgs.callPackage ./cardano-graphql.nix {
    inherit filteredSrc;
  };
  persistgraphql = (nixpkgs.callPackage ./persistgraphql { nodejs = nixpkgs.nodejs-14_x; }).persistgraphql;
}
