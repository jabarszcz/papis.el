{ fileset }: with fileset; toSource {
  root = ./.;
  fileset = union ./papis.config (
    union ./notes-template.org
      ./example-lib
  );
}
