{
  epkgs,
  version
}:

epkgs.elpaBuild {
    pname = "vterms";
    ename = "vterms";
    version = version;
    src = [ ./vterms.el ];
    packageRequires = [ epkgs.vterm ];
    meta = {};
}
