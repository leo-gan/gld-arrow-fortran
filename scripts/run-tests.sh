#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
export PATH="${HOME}/.local/bin:${PATH}"
fpm test --flag "-ffree-line-length-none"
fpm run export_array --flag "-ffree-line-length-none"
