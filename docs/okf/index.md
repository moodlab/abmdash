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
- Cross-links use bundle-relative absolute paths (`/modules/redcap_api.md`).
- Source ground truth: `DESCRIPTION`, `NAMESPACE`, `R/*.R`, `Dockerfile`,
  `.github/workflows/build-dashboard.yml`, `build-dashboard.sh`.
