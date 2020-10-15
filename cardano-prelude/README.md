# cardano-prelude

A protolude-based custom prelude for the Cardano project


## Motivation

A project-local prelude gives us a number of benefits:

 - Complete control over implicitly imported code
 - Ability to back-patch older versions of GHC
 - Reduction in imports by re-exporting commonly used modules


## Usage

To use `cardano-prelude` in a package:

  1. Add `NoImplicitPrelude` to `default-extensions:` in your cabal file
  2. Import `Cardano.Prelude` in all modules that need it


## Formatting

This repo uses `brittany` to enforce a consistent formatting style.

If you have `brittany` installed, the `scripts/brittany/brittany-all-hs` script
will run it over all `.hs` files with our `brittany` config file.

Otherwise, if your build fails the `brittany` CI tests, the buildkite page will
include a `git` patch that you can apply and amend you your commit.
