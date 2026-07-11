#!/usr/bin/env bash
# Builds Resyntax's Scribble manual and publishes it to the resyntax-docs
# Wisp site (hosted under @claude.notjack.space), merging into whatever's
# already deployed there.
#
# wispctl deploy always replaces a site's entire tree with --path's
# contents, so a naive deploy of just the production docs (or just one PR's
# preview) would wipe out everything else on the site. This script pulls the
# current tree first and only touches the one subpath it's responsible for.
#
# Usage:
#   publish-docs.sh publish <subpath>   # "" for the production docs at the
#                                        # site root, "preview/802" for a PR
#   publish-docs.sh delete <subpath>    # remove a subpath, e.g. after a PR closes
#
# Must be run from the repository root. Requires WISP_APP_PASSWORD in the
# environment and wispctl/raco on PATH.

set -euo pipefail

ACTION="${1:?usage: publish-docs.sh <publish|delete> <subpath>}"
SUBPATH="${2:-}"

HANDLE="claude.notjack.space"
SITE="resyntax-docs"

if [ -z "${WISP_APP_PASSWORD:-}" ]; then
  echo "WISP_APP_PASSWORD must be set" >&2
  exit 1
fi

WORKDIR="$(mktemp -d)"
trap 'rm -rf "$WORKDIR"' EXIT

SITE_DIR="$WORKDIR/site"
mkdir -p "$SITE_DIR"

# Ignore failure: the site may not exist yet on the very first publish.
wispctl pull "$HANDLE" --site "$SITE" --path "$SITE_DIR" --password "$WISP_APP_PASSWORD" || true

TARGET_DIR="$SITE_DIR${SUBPATH:+/$SUBPATH}"

case "$ACTION" in
  delete)
    rm -rf "$TARGET_DIR"
    ;;
  publish)
    BUILD_DIR="$WORKDIR/build"
    raco scribble ++main-xref-in --redirect-main https://docs.racket-lang.org/ \
      --htmls --dest "$BUILD_DIR" main.scrbl
    rm -rf "$TARGET_DIR"
    mkdir -p "$TARGET_DIR"
    cp -r "$BUILD_DIR/main/." "$TARGET_DIR/"
    ;;
  *)
    echo "Unknown action: $ACTION (expected publish or delete)" >&2
    exit 1
    ;;
esac

wispctl deploy "$HANDLE" --path "$SITE_DIR" --site "$SITE" --yes --password "$WISP_APP_PASSWORD"
