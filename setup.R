dir.create(".r-library", showWarnings = FALSE)
packages <- c("shiny", "pdftools", "xml2")
.libPaths(c(".r-library", .libPaths()))
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, lib = ".r-library", repos = "https://cloud.r-project.org")
stopifnot(all(vapply(packages, requireNamespace, logical(1), quietly = TRUE)))
