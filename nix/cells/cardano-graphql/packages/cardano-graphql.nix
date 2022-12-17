{ stdenv
, nodejs
, nodePackages
, runtimeShell
, yarn
, filteredSrc
}:

let
  packageJSON = builtins.fromJSON (builtins.readFile "${filteredSrc}/package.json");

  src = stdenv.mkDerivation {
    pname = "${packageJSON.name}-src";
    version = packageJSON.version;
    buildInputs = [ yarn nodejs ];
    src = filteredSrc;
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

    cat <<EOF > $out/bin/cardano-graphql-background
    #!${runtimeShell}
    exec ${nodejs}/bin/node $out/packages/api-cardano-db-hasura/dist/background.js
    EOF
    chmod +x $out/bin/cardano-graphql-background
  '';
}
