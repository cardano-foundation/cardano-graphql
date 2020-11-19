{ stdenv, lib, fetchurl }:

stdenv.mkDerivation rec {
  pname = "hasura-cli-ext";
  version = "1.3.3";
  src = fetchurl {
    url = "https://github.com/hasura/graphql-engine/releases/download/v${version}/cli-ext-hasura-linux.tar.gz";
    sha256 = "0a49w1lb3j33i8yz232inr7lm8j6vxg6y52izljnn71w2l25mqlw";
  };
  sourceRoot = ".";
  buildPhase = ":";
  installPhase = ''
    mkdir -p $out/bin
    cp cli-ext-hasura-linux $out/bin/
    chmod +x $out/bin/cli-ext-hasura-linux
  '';
  preFixup = let
    libPath = lib.makeLibraryPath [ stdenv.cc.cc ];
  in ''
    orig_size=$(stat --printf=%s $out/bin/cli-ext-hasura-linux)

    patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" $out/bin/cli-ext-hasura-linux
    patchelf --set-rpath ${libPath} $out/bin/cli-ext-hasura-linux
    new_size=$(stat --printf=%s $out/bin/cli-ext-hasura-linux)

    ###### zeit-pkg fixing starts here.
    # we're replacing plaintext js code that looks like
    # PAYLOAD_POSITION = '1234                  ' | 0
    # [...]
    # PRELUDE_POSITION = '1234                  ' | 0
    # ^-----20-chars-----^^------22-chars------^
    # ^-- grep points here
    #
    # var_* are as described above
    # shift_by seems to be safe so long as all patchelf adjustments occur
    # before any locations pointed to by hardcoded offsets

    var_skip=20
    var_select=22
    shift_by=$(expr $new_size - $orig_size)

    function fix_offset {
      # $1 = name of variable to adjust
      location=$(grep -obUam1 "$1" $out/bin/cli-ext-hasura-linux | cut -d: -f1)
      location=$(expr $location + $var_skip)

      value=$(dd if=$out/bin/cli-ext-hasura-linux iflag=count_bytes,skip_bytes skip=$location \
                 bs=1 count=$var_select status=none)
      value=$(expr $shift_by + $value)

      echo -n $value | dd of=$out/bin/cli-ext-hasura-linux bs=1 seek=$location conv=notrunc
    }

    fix_offset PAYLOAD_POSITION
    fix_offset PRELUDE_POSITION
  '';
  dontStrip = true;
}
