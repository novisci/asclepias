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
# 
# NOTE: Install asciidoctor-kroki with:
# npm i  asciidoctor-kroki 
# or globally with
# npm i asciidoctor-kroki -g
builddir=docs-site
mkdir -p "$builddir"

# Check that asciidoctor-kroki is installed
cd "$builddir" && npm ls asciidoctor-kroki && cd ..
status=$?
[ $status -ne 0 ] && 
  echo "you need to install: npm i asciidoctor asciidoctor-kroki" &&
  exit 1


cat > $builddir/playbook.yml <<- EOM
site:
  title: dummy-site
  start_page: asclepias::introduction.adoc
asciidoc:
  extensions:
  - asciidoctor-kroki
content:
  sources:
  - url: ../
    start_path: docs
    branches: [HEAD]
  - url: https://gitlab+user-guide:11HiN9THLsxPVHSPpzxB@gitlab.novisci.com/nsStat/nsBuild.git
    start_path: docs
    branches: [HEAD]
  - url: https://gitlab+deploy-token-antora-build:glpat-n2fySz3M7AY_9Z4W7vAz@gitlab.novisci.com/nsStat/event-data-model.git
    start_path: docs
    branches: [v3.0]
ui:
  bundle:
    url: https://gitlab.com/antora/antora-ui-default/-/jobs/artifacts/HEAD/raw/build/ui-bundle.zip?job=bundle-stable
    snapshot: true
EOM

# If you have a local installation of antora then use that, otherwise use the
# global version. See the following link for the distinction:
# https://docs.antora.org/antora/latest/install/install-antora/
if [[ -d $builddir/node_modules/@antora ]]; then
  cd "$builddir" && npx antora --fetch playbook.yml
else
  cd "$builddir" && antora --fetch playbook.yml
fi

# TODO: add tear down option
# cd .. && rm -rf $builddir
