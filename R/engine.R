max_upload <- 8 * 1024^2
weights <- c(Skills = 40, Experience = 30, Projects = 15, Education = 10, Certifications = 5)
resume_share <- .85
type_pattern <- "^[EI][SN][TF][JP](-[AT])?$"

read_resume <- function(path, name) {
  if (!file.exists(path) || file.size(path) > max_upload)
    stop("Choose a r\u00e9sum\u00e9 or CV smaller than 8 MB.")
  text <- switch(tolower(tools::file_ext(name)),
    txt = readLines(path, encoding = "UTF-8", warn = FALSE),
    pdf = pdftools::pdf_text(path),
    docx = {
      info <- utils::unzip(path, list = TRUE)
      entry <- info[info$Name == "word/document.xml", , drop = FALSE]
      if (nrow(entry) != 1L || entry$Length > max_upload)
        stop("This Word document could not be read.")
      con <- unz(path, "word/document.xml", open = "rb")
      on.exit(close(con), add = TRUE)
      xml <- xml2::read_xml(readBin(con, "raw", n = max_upload), options = "NONET")
      ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
      paragraphs <- xml2::xml_find_all(xml, ".//w:p", ns)
      vapply(paragraphs, function(p) {
        nodes <- xml2::xml_find_all(p, ".//w:t | .//w:tab | .//w:br", ns)
        paste0(ifelse(xml2::xml_name(nodes) == "t", xml2::xml_text(nodes), " "), collapse = "")
      }, character(1))
    },
    stop("Use a PDF, DOCX, or UTF-8 text file."))
  text <- paste(text, collapse = "\n")
  text <- iconv(text, from = "UTF-8", to = "UTF-8", sub = " ")
  if (!grepl("[[:alpha:]]{2,}", text))
    stop("No readable text found. For a scanned PDF, upload an OCR or text version.")
  text
}

headings <- c(
  Skills = "skills|technical skills|core competencies|technologies|tools",
  Experience = "experience|work experience|professional experience|employment|employment history|work history|professional background",
  Projects = "projects|selected projects|personal projects|academic projects|portfolio|research|publications",
  Education = "education|education and training|academic background|qualifications",
  Certifications = "certifications|certificates|licenses|credentials|licenses and certifications",
  Other = "summary|profile|professional summary|objective|interests|awards|languages|volunteering|references"
)

section_resume <- function(text) {
  blocks <- setNames(as.list(rep("", length(headings))), names(headings))
  current <- "Other"
  for (line in strsplit(text, "\n", fixed = TRUE)[[1]]) {
    line <- trimws(line)
    for (section in names(headings)) {
      pattern <- paste0("^(", headings[[section]], ")([[:space:]]*:[[:space:]]*|[[:space:]]*$)")
      if (grepl(pattern, line, ignore.case = TRUE, perl = TRUE)) {
        current <- section
        line <- sub(pattern, "", line, ignore.case = TRUE, perl = TRUE)
        break
      }
    }
    if (nzchar(line)) blocks[[current]] <- paste(blocks[[current]], line, sep = "\n")
  }
  blocks
}

has_skill <- function(text, skill) {
  skill <- gsub("[[:space:]]+", "[[:space:]-]+", skill)
  grepl(paste0("(?<![[:alnum:]_])", skill, "(?![[:alnum:]_])"),
        text, ignore.case = TRUE, perl = TRUE)
}

quiz <- list(
  c("When tackling a new problem, I think best by...", "Talking it through", "Reflecting on my own"),
  c("After a full day, I recharge by...", "Spending time with others", "Taking quiet time"),
  c("At the start of a project, I look for...", "Concrete examples", "Patterns and possibilities"),
  c("I prefer tasks that emphasize...", "Practical details", "New concepts"),
  c("In a difficult decision, I first weigh...", "Consistent criteria", "Effects on people"),
  c("When giving feedback, I start with...", "What the evidence shows", "What the person needs"),
  c("I do my best work when I can...", "Plan steps up front", "Adapt as I go"),
  c("As a deadline approaches, I tend to...", "Close decisions early", "Keep options open")
)
axes <- list(c("E", "I"), c("S", "N"), c("T", "F"), c("J", "P"))
style_words <- c(E = "talk it through", I = "reflect first", S = "concrete details",
                 N = "patterns and possibilities", T = "consistent criteria",
                 F = "impact on people", J = "plan ahead", P = "adapt as you go")

quiz_result <- function(answers) {
  if (length(answers) != 8L || !all(answers %in% c("a", "b"))) return(NULL)
  paste(vapply(seq_along(axes), function(i) {
    pair <- answers[c(2L*i-1L, 2L*i)]
    if (pair[1] != pair[2]) return("-")
    axes[[i]][[if (pair[1] == "a") 1L else 2L]]
  }, character(1)), collapse = "")
}

rank_roles <- function(blocks, profile) {
  all_text <- paste(unlist(blocks), collapse = "\n")
  letters <- strsplit(substr(profile, 1L, 4L), "", fixed = TRUE)[[1]]
  ranked <- lapply(catalog, function(r) {
    skill_matches <- r$skills[vapply(r$skills, function(s) has_skill(all_text, s), logical(1))]
    work <- function(text) grepl(r$work, text, ignore.case = TRUE, perl = TRUE)
    count <- function(text) mean(vapply(r$skills, function(s) has_skill(text, s), logical(1)))
    section <- function(name) blocks[[name]]
    coverage <- c(Skills = length(skill_matches) / length(r$skills),
      Experience = if (work(section("Experience"))) 1 else if (work(all_text)) .45 else count(section("Experience")),
      Projects = if (work(section("Projects"))) 1 else count(section("Projects")),
      Education = if (work(section("Education"))) 1 else count(section("Education")),
      Certifications = if (work(section("Certifications"))) 1 else count(section("Certifications")))
    style <- mean(ifelse(letters == "-", .5, letters == strsplit(r$style, "", fixed = TRUE)[[1]]))
    points <- weights * coverage
    r$matched <- skill_matches
    r$gaps <- setdiff(r$skills, skill_matches)
    r$points <- points
    r$resume <- sum(points)
    r$style_points <- 100 * style
    r$score <- resume_share * r$resume + (1 - resume_share) * r$style_points
    r
  })
  ranked <- Filter(function(r) r$resume > 0, ranked)
  if (!length(ranked)) return(ranked)
  ranked[order(-vapply(ranked, function(r) r$score, numeric(1)),
               -vapply(ranked, function(r) r$resume, numeric(1)))]
}
