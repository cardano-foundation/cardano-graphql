{ stdenv
, nix-inclusive
, nodejs
, nodePackages
, runtimeShell
, sources
, yarn
}:

let
  packageJSON = builtins.fromJSON (builtins.readFile ../package.json);

  src = stdenv.mkDerivation {
    pname = "${packageJSON.name}-src";
    version = packageJSON.version;
    buildInputs = [ yarn nodejs ];
    src = nix-inclusive ./.. [
      ../yarn.lock
      ../.yarnrc
      ../package.json
      ../packages
      ../packages-cache
      ../tsconfig.json
      ../docker-compose.yml
    ];
    buildCommand = ''
      mkdir -p $out
      cp -r $src/. $out/
      cd $out
      chmod -R u+w .
      yarn --offline --frozen-lockfile --non-interactive
    '';
  };

in stdenv.mkDerivation {
  pname = packageJSON.name;
  version = packageJSON.version;
  inherit src;
  buildInputs = [ nodejs yarn ];
  buildCommand = ''
    mkdir -p $out
    cp -r $src/. $out/
    chmod -R u+w $out
    patchShebangs $out

    cd $out

    yarn build
    find . -name node_modules -type d -print0 | xargs -0 rm -rf
    yarn --production --offline --frozen-lockfile --non-interactive

    mkdir -p $out/bin
    cat <<EOF > $out/bin/cardano-graphql
    #!${runtimeShell}
    exec ${nodejs}/bin/node $out/packages/server/dist/index.js
    EOF
    chmod +x $out/bin/cardano-graphql
  '';
}
