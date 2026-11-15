if (dir.exists(".r-library")) .libPaths(c(".r-library", .libPaths()))
shiny::runApp(".", host = "127.0.0.1", port = NULL, launch.browser = interactive())
