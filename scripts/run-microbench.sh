#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")/.."
export PATH="${HOME}/.local/bin:${PATH}"
fpm run bench_round --profile release --flag "-ffree-line-length-none"
