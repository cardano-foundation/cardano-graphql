{ buildGoModule, hasura-cli, src }:

buildGoModule (rec {
  inherit (hasura-cli.drvAttrs) pname modRoot subPackages doCheck postInstall;
  inherit (hasura-cli) meta;
  inherit src;
  name = "hasura-${version}";
  version = "2.16.1";
  buildFlagsArray = [''-ldflags=
    -X github.com/hasura/graphql-engine/cli/version.BuildVersion=${version}
    -s
    -w
  ''];
  vendorSha256 = "sha256-vZKPVQ/FTHnEBsRI5jOT6qm7noGuGukWpmrF8fK0Mgs=";
})
