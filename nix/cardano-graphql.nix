{ mkYarnPackage
, lib
, cardano-graphql-src
, runtimeShell
, nodejs
, python
}:

let
  packageJSON = cardano-graphql-src + "/package.json";
  version = (__fromJSON (__readFile packageJSON)).version;

in mkYarnPackage {
  pname = "cardano-graphql";
  inherit packageJSON version;
  yarnLock = cardano-graphql-src + "/yarn.lock";
  src = lib.cleanSourceWith {
    filter = name: type: let
      baseName = baseNameOf (toString name);
      sansPrefix = lib.removePrefix (toString ../.) name;
      in_blacklist =
        lib.hasPrefix "/node_modules" sansPrefix ||
        lib.hasPrefix "/build" sansPrefix ||
        lib.hasPrefix "/.git" sansPrefix;
      in_whitelist =
        (type == "directory") ||
        (lib.hasSuffix ".yml" name) ||
        (lib.hasSuffix ".ts" name) ||
        (lib.hasSuffix ".json" name) ||
        (lib.hasSuffix ".graphql" name) ||
        baseName == "package.json" ||
        baseName == "yarn.lock" ||
        (lib.hasPrefix "/deploy" sansPrefix);
    in (
      (!in_blacklist) && in_whitelist
    );
    src = cardano-graphql-src;
  };
  yarnPreBuild = ''
    mkdir -p $HOME/.node-gyp/${nodejs.version}
    echo 9 > $HOME/.node-gyp/${nodejs.version}/installVersion
    ln -sfv ${nodejs}/include $HOME/.node-gyp/${nodejs.version}
  '';

  installPhase = ''
    export PATH="$PATH:$node_modules/.bin"

    yarn run build

    pwd

    ls -hal

    cp -r deps/cardano-graphql/dist $out

    mkdir -p $out/bin
    cat <<EOF > $out/bin/cardano-graphql
    #!${runtimeShell}
    exec ${nodejs}/bin/node $out/index.js
    EOF
    chmod +x $out/bin/cardano-graphql
  '';

  distPhase = ''
    cp -r . $out
  '';
}
