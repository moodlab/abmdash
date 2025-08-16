source("renv/activate.R")
# Ensure renv library is added to .libPaths() in multi-stage Docker builds
lib <- Sys.glob('/project/renv/library/project-*/linux-*/R-*/x86_64-pc-linux-gnu')
if (length(lib) > 0) {
  .libPaths(c(lib, .libPaths()))
}

# If renv activation script exists, source it (optional, keeps normal renv behavior)
activate <- '/project/renv/activate.R'
if (file.exists(activate)) {
  source(activate)
}

