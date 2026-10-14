#!/usr/bin/env bash
# Write the fpm registry entry for this release.
# --dry-run prints it. --submit opens a pull request on fortran-lang/fpm-registry.
# See docs/publishing.md.
set -euo pipefail

root="$(cd "$(dirname "$0")/.." && pwd)"
cd "${root}"

mode="publish"
if [[ "${1:-}" == "--dry-run" ]]; then
  mode="dry-run"
elif [[ "${1:-}" == "--submit" ]]; then
  mode="submit"
elif [[ -n "${1:-}" ]]; then
  echo "usage: scripts/publish.sh [--dry-run | --submit]" >&2
  exit 2
fi

name="$(sed -n 's/^name = "\([^"]*\)"/\1/p' fpm.toml | head -n 1)"
version="$(sed -n 's/^version = "\([^"]*\)"/\1/p' fpm.toml | head -n 1)"
if [[ -z "${name}" || -z "${version}" ]]; then
  echo "fpm.toml is missing name or version" >&2
  exit 1
fi

origin="$(git remote get-url origin)"
origin="${origin%.git}"
origin="${origin#git@github.com:}"
origin="${origin#https://github.com/}"
repo_url="https://github.com/${origin}"

if [[ "${GITHUB_REF_TYPE:-}" == "tag" ]]; then
  tag_name="${GITHUB_REF_NAME}"
  tag_version="${tag_name#v}"
  if [[ "${tag_version}" != "${version}" ]]; then
    echo "git tag ${tag_name} does not match fpm.toml version ${version}" >&2
    exit 1
  fi
else
  tag_name="${version}"
fi

snippet="$(cat <<EOF
[${name}]
"${version}" = {git="${repo_url}", tag="${tag_name}"}
"latest" = {git="${repo_url}", tag="${tag_name}"}
EOF
)"
block="$(cat <<EOF
fpm dependency for tag \`${tag_name}\`:

\`\`\`toml
[dependencies]
${name}.git = "${repo_url}"
${name}.tag = "${tag_name}"
\`\`\`

Registry table for fortran-lang/fpm-registry \`registry.toml\`:

\`\`\`toml
${snippet}
\`\`\`
EOF
)"
printf '%s\n' "${block}"

snippet_file="${TMPDIR:-/tmp}/${name}-fpm-registry.toml"
printf '%s\n' "${snippet}" > "${snippet_file}"
echo "snippet: ${snippet_file}"

if [[ "${mode}" == "dry-run" ]]; then
  echo "dry-run: registry pull request was not opened"
  exit 0
fi

if [[ "${GITHUB_ACTIONS:-}" == "true" && "${GITHUB_REF_TYPE:-}" == "tag" ]]; then
  printf '%s\n' "${block}" | python3 scripts/github_release.py notes --tag "${tag_name}"
  python3 scripts/github_release.py upload --tag "${tag_name}" --file "${snippet_file}"
fi

if [[ "${mode}" != "submit" ]]; then
  echo "Run scripts/publish.sh --submit on a machine logged in as leo-gan to open the fpm-registry pull request."
  exit 0
fi

if ! command -v gh >/dev/null 2>&1; then
  echo "gh is not on PATH" >&2
  exit 1
fi
if [[ -n "${FPM_REGISTRY_TOKEN:-}" ]]; then
  export GH_TOKEN="${FPM_REGISTRY_TOKEN}"
fi

export GH_PROMPT_DISABLED=1
if ! gh repo fork fortran-lang/fpm-registry --clone=false --default-branch-only; then
  gh repo view leo-gan/fpm-registry >/dev/null
fi

work="$(mktemp -d)"
cleanup() { rm -rf "${work}"; }
trap cleanup EXIT
gh repo clone fortran-lang/fpm-registry "${work}/registry" -- --depth 1
git -C "${work}/registry" checkout -B "gld-arrow-fortran-${version}"
python3 "${root}/scripts/fpm_registry.py" \
  --registry "${work}/registry/registry.toml" \
  --name "${name}" \
  --version "${version}" \
  --git "${repo_url}" \
  --tag "${tag_name}" \
  --in-place
git -C "${work}/registry" add registry.toml
if git -C "${work}/registry" diff --cached --quiet; then
  echo "registry.toml already has ${name} ${version}"
else
  git -C "${work}/registry" commit -m "Register ${name} ${version}."
fi
if [[ -z "${GH_TOKEN:-}" ]]; then
  GH_TOKEN="$(gh auth token)"
  export GH_TOKEN
fi
askpass="${work}/askpass"
cat > "${askpass}" <<'EOF'
#!/bin/sh
case "$1" in
  *Username*) printf '%s\n' "x-access-token" ;;
  *) printf '%s\n' "${GH_TOKEN}" ;;
esac
EOF
chmod +x "${askpass}"
GIT_ASKPASS="${askpass}" GIT_TERMINAL_PROMPT=0 \
  git -C "${work}/registry" push "https://github.com/leo-gan/fpm-registry.git" \
  "HEAD:gld-arrow-fortran-${version}"

existing="$(gh pr list --repo fortran-lang/fpm-registry --head "leo-gan:gld-arrow-fortran-${version}" --json url --jq '.[0].url // empty')"
if [[ -n "${existing}" ]]; then
  echo "pull request already open: ${existing}"
  exit 0
fi

gh pr create --repo fortran-lang/fpm-registry \
  --head "leo-gan:gld-arrow-fortran-${version}" \
  --base master \
  --title "Register ${name} ${version}" \
  --body "$(cat <<EOF
Add \`${name}\` ${version} from ${repo_url} tag \`${tag_name}\`.

The package is Fortran, MIT licensed, and builds with fpm. \`fpm test\` runs \`tests/test_roundtrip.f90\`.
EOF
)"
