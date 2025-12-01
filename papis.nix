# This nix file is callPackage-able or can be used directly with nix-build/nix-shell:

# To build 'papis.el', run:
# $ nix-build papis.nix

# To test 'papis.el', run:
# $ nix-build papis.nix -A tests

{
  pkgs ? import <nixpkgs> {},
  papis ? null,
  emacsPackages ? null,
} @ args :
let
  lib = pkgs.lib;
  papis = args.papis or pkgs.papis;
  emacsPackages = args.emacsPackages or pkgs.emacsPackages;
  trivialBuild = emacsPackages.trivialBuild;
  papis-el =
    trivialBuild {
      pname = "papis";
      version = "current";
      src = ./papis.el;
    };
  papis-test-el =
    trivialBuild {
      pname = "papis-test";
      version = "current";
      src = with lib.fileset; toSource {
        root = ./test;
        fileset = union ./test/papis-test-common.el ./test/papis-test.el;
      };
      packageRequires = [ papis-el ];
    };
  example-lib = import ./example-lib.nix { inherit (lib) fileset; };
  run-papis-ert-tests =
    pkgs.runCommand "run-papis-ert-tests" {
      nativeBuildInputs = [ (emacsPackages.withPackages [ papis-test-el ]) papis ];
    }
    ''
      cp --no-preserve=mode -r ${example-lib}/* ./ # Writable copy
      export PAPIS_CONFIG_DIR=. # Without that, it doesn't try the given file
      export XDG_CACHE_HOME=. # Put the Papis DB in the current build dir
      emacs -q -batch -l ert -l papis-test \
          --eval "(let ((ert-batch-print-level 10) \
                        (ert-batch-print-length 120)) \
                   (ert-run-tests-batch-and-exit))" |& tee $out
    '';
  papis-with-tests =
    papis-el.overrideAttrs (finalAttrs: {
      passthru.tests.run = run-papis-ert-tests;
    });
in papis-with-tests
