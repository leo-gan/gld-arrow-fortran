#!/usr/bin/env python3
"""Insert one package table into fortran-lang/fpm-registry registry.toml."""

import argparse
import sys


def table_headers(lines):
    headers = []
    for index, line in enumerate(lines):
        stripped = line.strip()
        if stripped.startswith("[") and stripped.endswith("]") and not stripped.startswith("[["):
            headers.append((index, stripped[1:-1]))
    return headers


def block_lines(name, version, git, tag):
    return [
        f"[{name}]",
        f'"{version}" = {{git="{git}", tag="{tag}"}}',
        f'"latest" = {{git="{git}", tag="{tag}"}}',
    ]


def insert_package(text, name, version, git, tag):
    lines = text.splitlines()
    headers = table_headers(lines)
    block = block_lines(name, version, git, tag)
    for index, (start, header) in enumerate(headers):
        if header != name:
            continue
        end = headers[index + 1][0] if index + 1 < len(headers) else len(lines)
        lines[start:end] = block + [""]
        return "\n".join(lines).rstrip() + "\n"
    insert_at = len(lines)
    for start, header in headers:
        if header.casefold() > name.casefold():
            insert_at = start
            break
    piece = block + [""]
    if insert_at > 0 and lines[insert_at - 1] != "":
        piece = [""] + piece
    lines[insert_at:insert_at] = piece
    return "\n".join(lines).rstrip() + "\n"


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--registry", required=True)
    parser.add_argument("--name", required=True)
    parser.add_argument("--version", required=True)
    parser.add_argument("--git", required=True)
    parser.add_argument("--tag", required=True)
    parser.add_argument("--in-place", action="store_true")
    args = parser.parse_args()
    with open(args.registry, encoding="utf-8") as handle:
        original = handle.read()
    updated = insert_package(original, args.name, args.version, args.git, args.tag)
    if args.in_place:
        with open(args.registry, "w", encoding="utf-8") as handle:
            handle.write(updated)
    else:
        sys.stdout.write(updated)


if __name__ == "__main__":
    main()
