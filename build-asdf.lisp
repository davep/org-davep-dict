(require :asdf-install)
(require :asdf-packaging-tools)
(asdf:oos 'asdf-packaging-tools:release-op :org-davep-dict :directory "asdf-package/" :force t)
