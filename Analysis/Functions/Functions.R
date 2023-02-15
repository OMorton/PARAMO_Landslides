sessionInfo()
## Unload all the rubbish that gets loaded (causes issues for terra/raster etc.)
unload_pkgs <- \(exc=NULL) {
  bpk <- c("compiler", "graphics", "tools", "utils", "grDevices", "stats", 
           "datasets", "methods", "base", "rstudioapi") |> c(exc)
  while (length(setdiff(loadedNamespaces(), bpk)) > 0) {
    lapply(setdiff(loadedNamespaces(), bpk), \(x) {
      try(unloadNamespace(x), silent=TRUE)
    })
  }
}
unload_pkgs()
sessionInfo()
