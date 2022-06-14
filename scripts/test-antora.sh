#!/bin/bash
#
# A simple script which builds a dummy Antora site
# from the docs folder.
# This can be used to check that the docs within this repo
# successfully compile.
# 
# Requires the Antora be installed:
# https://docs.antora.org/antora/latest/install/install-antora/
#
# TODO
# - make more script general for use in other repos
# - add tear down option (for pure smoke test)
# - add option to automatically open file in a browser

# Create temp place for docs-site
# This directory should not be committed to git.
# 
# Note: If linking to other sites in the noviverse,
# add a new url section for each git project linked.
builddir=docs-site
mkdir -p "$builddir"

cat > $builddir/playbook.yml <<- EOM
site:
  title: dummy-site
  start_page: asclepias::introduction.adoc
content:
  sources:
  - url: ../
    start_path: docs
    branches: [HEAD]
  - url: https://gitlab+user-guide:11HiN9THLsxPVHSPpzxB@gitlab.novisci.com/nsStat/nsBuild.git
    start_path: docs
    branches: [HEAD]
ui:
  bundle:
    url: https://gitlab.com/antora/antora-ui-default/-/jobs/artifacts/HEAD/raw/build/ui-bundle.zip?job=bundle-stable
    snapshot: true
EOM

# If you have a local installation of antora then use that, otherwise use the
# global version. See the following link for the distinction:
# https://docs.antora.org/antora/latest/install/install-antora/
# if [[ -d $builddir/node_modules/@antora ]]; then
  cd "$builddir" && npx antora --fetch playbook.yml
# else
#   cd "$builddir" && antora --fetch playbook.yml
# fi

# TODO: add tear down option
# cd .. && rm -rf $builddir
