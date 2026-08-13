---
okf_version: 0.1
---

# abmdash OKF Knowledge Bundle

Curated knowledge map of the `abmdash` R package (ABM project dashboard).
Agents: read this index first, then the concept docs, before falling back to
raw source under `R/`.

## Subdirectories

- [modules](modules/index.md) - R/ module concept docs (9 modules, one per source file)
- [services](services/index.md) - deployable service docs (Docker + CI dashboard build)

## Conventions

- Concept boundaries == `R/` file boundaries; one concept doc per source file.
- `pure: true` frontmatter marks pure-core modules; `false` marks effectful API shells.
- Cross-links use relative paths — same-dir links as `name.md`, cross-dir links as `../modules/name.md`. Bundle-absolute style (`/modules/...`) is avoided because the okf-bundle skill Step-7 link validator falsely reports such links as broken (see log.md).
- Source ground truth: `DESCRIPTION`, `NAMESPACE`, `R/*.R`, `Dockerfile`,
  `.github/workflows/build-dashboard.yml`, `build-dashboard.sh`.
