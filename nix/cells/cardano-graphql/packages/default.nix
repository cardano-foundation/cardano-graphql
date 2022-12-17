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
    "${inputs.self}/yarn.lock"
    "${inputs.self}/.yarnrc"
    "${inputs.self}/package.json"
    "${inputs.self}/packages"
    "${inputs.self}/packages-cache"
    "${inputs.self}/tsconfig.json"
    "${inputs.self}/docker-compose.yml"
  ];

in {
  cardano-graphql = nixpkgs.callPackage ./cardano-graphql.nix {
    inherit filteredSrc;
  };
  persistgraphql = (nixpkgs.callPackage ./persistgraphql { nodejs = nixpkgs.nodejs-14_x; }).persistgraphql;
}
