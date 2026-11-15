source("app.R")

has <- function(output, needle) grepl(needle, output$html, fixed = TRUE)
pdf <- tempfile(fileext = ".pdf")
grDevices::pdf(pdf)
graphics::plot.new()
graphics::text(.5, .5, "Reporting analyst: SQL and Tableau")
invisible(grDevices::dev.off())
stopifnot(grepl("SQL and Tableau", read_resume(pdf, "sample.pdf"), fixed = TRUE))
unlink(pdf)

txt <- tempfile(fileext = ".txt")
writeLines(c("EXPERIENCE", "Software developer with JavaScript and Git",
             "PROJECTS: Built a web application with APIs and testing"), txt)
shiny::testServer(server, {
  stopifnot(has(output$matches, "Add your"))
  session$setInputs(demo = 1, method = "external", external_type = "bogus")
  stopifnot(is.null(profile()), has(output$matches, "Complete your assessment"))
  session$setInputs(external_type = " intj-t ")
  stopifnot(identical(profile(), "INTJ-T"), identical(names(ranked())[1], "data"),
            has(output$matches, "see career path suggestions"),
            has(output$score, "Data Analyst"), has(output$portrait, "<svg"),
            has(output$suggestions, "Portfolio project"), has(output$suggestions, "PL-300"))
  initial_image <- image_svg()
  session$setInputs(skin = "deep", hair = "curls", glasses = TRUE)
  stopifnot(!identical(initial_image, image_svg()),
            xml2::xml_name(xml2::read_xml(image_svg())) == "svg")
  if (length(ranked()) > 1L) {
    alternate <- ranked()[[2L]]
    session$setInputs(chosen_role = names(ranked())[2L])
    stopifnot(has(output$suggestions, alternate$title), has(output$portrait, alternate$title))
  }
  session$setInputs(method = "quiz", q1 = "b", q2 = "b", q3 = "b", q4 = "b",
                    q5 = "a", q6 = "a", q7 = "a", q8 = "b")
  stopifnot(identical(profile(), "INT-"))
  session$setInputs(cv = list(datapath = txt, name = "software.txt"))
  stopifnot(identical(names(ranked())[1], "software"), has(output$score, "Software Developer"))
  session$setInputs(cv = list(datapath = txt, name = "unsupported.jpg"))
  stopifnot(is.null(ranked()), has(output$upload_status, "UTF-8 text file"))
})
unlink(txt)
stopifnot(length(rank_roles(section_resume("GENERAL\nNo relevant evidence"), "ENFP")) == 0)
cat("Career Atlas workflow checks passed.\n")
