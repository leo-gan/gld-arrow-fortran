# Publishing

fpm installs this library from the Git tag. The public catalog is [fortran-lang/fpm-registry](https://github.com/fortran-lang/fpm-registry). It is a `registry.toml` file. Publishing means adding one table and opening a pull request. There is no upload token and no package server.

The fpm name is `gld-arrow-fortran`. The version in `fpm.toml` is `0.2.0`. Release tags match the Mojo libraries and have no `v` prefix: `0.2.0`.

A dependency works as soon as the tag exists. The registry listing is how `fpm search` finds the name.

## Register once

The registry README asks a package to meet the [fortran-lang.org package criteria](https://github.com/fortran-lang/fortran-lang.org/blob/HEAD/PACKAGES.md). Those criteria include an open-source license, a README, working code, and at least five GitHub stars. Reviewers on the registry may wait for that. The git dependency below does not wait for the pull request.

No account besides GitHub is required. `scripts/publish.sh --submit` uses the `gh` login of the machine that runs it, which needs permission to fork `fortran-lang/fpm-registry` and open a pull request. That is the normal `leo-gan` GitHub login.

To open the pull request from GitHub Actions instead, add one repository secret:

| Name | Value |
| --- | --- |
| `FPM_REGISTRY_TOKEN` | A classic personal access token with the `public_repo` scope, or a fine-grained token that can write to your fork of `fpm-registry` and open pull requests on `fortran-lang/fpm-registry`. |

The default `GITHUB_TOKEN` inside Actions cannot push to that fork. Leave the secret unset and run `--submit` locally if you prefer not to store a token.

## Publish a version

Merge `.github/workflows/publish.yml` to `main` before creating the release.

```bash
git checkout main
git pull
git tag 0.2.0
git push origin 0.2.0
gh release create 0.2.0 --target main --title 0.2.0 --generate-notes
```

The Publish workflow runs `fpm test`, writes the registry table onto the release, and uploads `gld-arrow-fortran-fpm-registry.toml`.

Open the registry pull request after the tag exists:

```bash
bash scripts/publish.sh --submit
```

The script forks `fortran-lang/fpm-registry` if needed, inserts the table in order, and opens a pull request against `master`. Run it again to update the same branch. Check the table without opening the request:

```bash
bash scripts/publish.sh --dry-run
```

Actions workflow_dispatch has `dry_run` and `submit` inputs. `submit` uses `FPM_REGISTRY_TOKEN`.

## Install

This works before the registry pull request is merged:

```toml
[dependencies]
gld-arrow-fortran.git = "https://github.com/leo-gan/gld-arrow-fortran"
gld-arrow-fortran.tag = "0.2.0"
```

The module is `gld_arrow`.
