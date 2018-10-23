# cardano-prelude

A protolude-based custom prelude for the Cardano project


## Motivation

A project-local prelude gives us a number of benefits:

 - Complete control over implicitly imported code
 - Ability to back-patch older versions of GHC
 - Reduction in imports by re-exporting commonly used modules


## Usage

To use `cardano-prelude` in a package:

  1. Replace dependency on `base` with `base-no-prelude`
  2. Add dependency on `cardano-prelude`

GHC finds the `Prelude` module exported by `cardano-prelude` and uses it as the
implicit prelude. This method means we don't need `NoImplicitPrelude` enabled on
all projects and we don't need to explicitly import the main prelude module.
