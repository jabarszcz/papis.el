# This nix file is callPackage-able or can be used directly with nix-build/nix-shell:

# To build 'papis.el', run:
# $ nix-build papis-citar.nix

# To test 'papis.el', run:
# $ nix-build papis-citar.nix -A tests

{
  pkgs ? import <nixpkgs> {},
  papis ? null,
  papis-el ? null,
  emacsPackages ? null,
  citar ? null,
  el-mock ? null,
  with-simulated-input ? null,
} @ args :
let
  lib = pkgs.lib;
  papis = args.papis or pkgs.papis;
  emacsPackages = args.emacsPackages or pkgs.emacsPackages;
  citar = args.citar or emacsPackages.citar;
  el-mock = args.el-mock or emacsPackages.el-mock;
  with-simulated-input =
    args.with-simulated-input or emacsPackages.with-simulated-input;
  trivialBuild = emacsPackages.trivialBuild;
  papis-el =
    args.papis-el or (import ./papis.nix {
      inherit pkgs papis emacsPackages el-mock;
    });
  papis-citar-el = trivialBuild {
    pname = "papis-citar";
    version = "current";
    src = ./papis-citar.el;
    packageRequires = [ citar papis-el with-simulated-input ];
  };
  papis-citar-test-el =
    trivialBuild {
      pname = "papis-citar-test";
      version = "current";
      src = with lib.fileset; toSource {
        root = ./test;
        fileset = union ./test/papis-test-common.el ./test/papis-citar-test.el;
      };
      packageRequires = [ papis-citar-el el-mock ];
    };
  example-lib = import ./example-lib.nix { inherit (lib) fileset; };
  run-papis-citar-ert-tests =
    pkgs.runCommand "run-papis-citar-ert-tests" {
      nativeBuildInputs = [ (emacsPackages.withPackages [ papis-citar-test-el ]) papis ];
    }
    ''
      cp --no-preserve=mode -r ${example-lib}/* ./ # Writable copy
      export PAPIS_CONFIG_DIR=. # Without that, it doesn't try the given file
      export XDG_CACHE_HOME=. # Put the Papis DB in the current build dir
      emacs -q -batch -l ert -l papis-citar-test \
          --eval "(let ((ert-batch-print-level 10) \
                        (ert-batch-print-length 120)) \
                   (ert-run-tests-batch-and-exit))" |& tee $out
    '';
  papis-citar-with-tests =
    papis-citar-el.overrideAttrs (finalAttrs: {
      passthru.tests.run = run-papis-citar-ert-tests;
    });
in papis-citar-with-tests
