if (dir.exists(".r-library")) .libPaths(c(".r-library", .libPaths()))
library(shiny)
for (file in c("catalog", "engine", "portrait")) source(paste0("R/", file, ".R"), encoding = "UTF-8", local = TRUE)
options(shiny.maxRequestSize = max_upload + 65536)

external <- function(label, url) tags$a(label, href = url, target = "_blank", rel = "noopener noreferrer")
card <- function(...) div(class = "card", ...)
kicker <- function(text) div(class = "kicker", text)
hero <- function(chapter, title, subtitle) div(class = "hero", kicker(chapter), h1(title), p(subtitle))
meter <- function(label, value, max = 100) div(class = "meter",
  div(class = "meter-label", span(label), strong(sprintf("%g / %g", round(value, 1), max))),
  tags$progress(value = value, max = max, "aria-label" = label))

ui <- navbarPage(
  title = span(class = "brand", span(class = "brand-icon", "\u2726"), "Career Atlas"),
  id = "page", collapsible = TRUE,
  header = tags$head(tags$link(rel = "stylesheet", href = "atlas.css")),
  tabPanel("R\u00e9sum\u00e9", value = "resume",
    hero("01 / YOUR EVIDENCE", "Find a direction worth exploring.",
      "Start with what you've done. Add how you like to work. Leave with a role to explore and a project to try."),
    div(class = "split",
      card(kicker("YOUR STARTING POINT"), h2("Add your r\u00e9sum\u00e9 or CV"),
        fileInput("cv", "Upload a file", accept = c(".pdf", ".docx", ".txt"),
                  buttonLabel = "Choose file", placeholder = "PDF, DOCX or TXT"),
        p(class = "muted", "Up to 8 MB. PDFs must contain selectable text."),
        actionButton("demo", "Try a sample r\u00e9sum\u00e9", class = "secondary"),
        div(class = "upload-status", "aria-live" = "polite", uiOutput("upload_status")),
        actionButton("to_quiz", "Continue to assessment", class = "primary")),
      card(kicker("SCORING BREAKDOWN"), h2("What carries weight"),
        p("These are the maximum points for each r\u00e9sum\u00e9 element. The points earned depend on the role."),
        lapply(names(weights), function(label) meter(label, weights[[label]])),
        p(class = "muted", "The r\u00e9sum\u00e9 makes up 85% of the role score; work style makes up 15%.")))),
  tabPanel("Assessment", value = "assessment",
    hero("02 / YOUR WORK STYLE", "How do you like to work?",
      "Answer eight prompts for an instant result, or enter your existing 16Personalities type."),
    div(class = "split",
      card(radioButtons("method", "Assessment method",
        c("Quick work-style quiz" = "quiz", "Enter my 16Personalities result" = "external")),
        conditionalPanel("input.method === 'quiz'",
          p(class = "muted", "An informal reflection exercise for career exploration."),
          lapply(seq_along(quiz), function(i) {
            q <- quiz[[i]]
            radioButtons(paste0("q", i), paste0(i, ". ", q[1]),
                         setNames(c("a", "b"), q[-1]), selected = character(0))
          })),
        conditionalPanel("input.method === 'external'",
          p(external("Take the official test \u2197", "https://www.16personalities.com/free-personality-test"),
            " \u00b7 ", external("Explore the 16 types \u2197", "https://www.16personalities.com/personality-types")),
          textInput("external_type", "Your type", placeholder = "e.g. ENFP-T"),
          p(class = "muted", "Enter the result you received on the official site."))),
      card(kicker("INSTANT RESULT"), div("aria-live" = "polite", uiOutput("profile")),
        actionButton("to_match", "Explore role matches", class = "primary")))),
  tabPanel("Role matches", value = "matches",
    hero("03 / YOUR DIRECTION", "See where your evidence points.",
      "Explore the strongest signals in your r\u00e9sum\u00e9 alongside the kinds of work you prefer."),
    uiOutput("matches")),
  tabPanel("Suggestions", value = "suggestions",
    hero("04 / YOUR NEXT MOVES", "Turn a possibility into a plan.",
      "Start with a project, practice a skill, then decide whether a course or credential is useful."),
    uiOutput("suggestions")),
  footer = tags$footer("Career Atlas  /  A starting point for career exploration.")
)

server <- function(input, output, session) {
  cv <- reactiveVal(NULL)
  observeEvent(input$cv, {
    file <- input$cv
    cv(tryCatch({
      text <- suppressWarnings(read_resume(file$datapath, file$name))
      list(name = file$name, sections = section_resume(text))
    }, error = function(e) {
      known <- c("Choose a r\u00e9sum\u00e9 or CV smaller than 8 MB.", "Use a PDF, DOCX, or UTF-8 text file.",
                "No readable text found. For a scanned PDF, upload an OCR or text version.")
      msg <- conditionMessage(e)
      list(error = if (msg %in% known) msg else "Couldn't read that file. Try exporting it again or uploading a TXT version.")
    }))
  })
  observeEvent(input$demo, {
    cv(list(name = "Sample r\u00e9sum\u00e9", sections = section_resume(
      "SKILLS\nSQL, Excel, Power BI, R, statistics, dashboards\nEXPERIENCE\nReporting analyst. Built monthly dashboards and analyzed customer trends.\nPROJECTS\nBuilt a sales forecast dashboard with SQL and Power BI.\nEDUCATION\nBS in Statistics")))
  })
  output$upload_status <- renderUI({
    value <- cv()
    if (is.null(value)) return(p(class = "muted", "Uploads are processed on the app server and aren't sent to an external service."))
    if (!is.null(value$error)) return(p(class = "error", value$error))
    detected <- names(weights)[vapply(value$sections[names(weights)], nzchar, logical(1))]
    tagList(strong(paste("Ready:", value$name)),
      p(class = "muted", if (length(detected)) paste("Sections detected:", paste(detected, collapse = ", "))
        else "No standard headings found. Skills and role terms can still match anywhere in the document."))
  })
  profile <- reactive({
    if (identical(input$method, "external")) {
      code <- toupper(trimws(if (is.null(input$external_type)) "" else input$external_type))
      return(if (grepl(type_pattern, code)) code else NULL)
    }
    quiz_result(vapply(seq_along(quiz), function(i) {
      x <- input[[paste0("q", i)]]
      if (length(x) == 1L) x else ""
    }, character(1)))
  })
  output$profile <- renderUI({
    code <- profile()
    if (is.null(code)) {
      if (identical(input$method, "external")) return(p("Enter a four-letter type, with an optional -A or -T suffix."))
      completed <- sum(vapply(seq_along(quiz), function(i)
        isTRUE(input[[paste0("q", i)]] %in% c("a", "b")), logical(1)))
      return(p(sprintf("%d of %d answered. Your result will appear here.", completed, length(quiz))))
    }
    letters <- strsplit(substr(code, 1L, 4L), "", fixed = TRUE)[[1]]
    tagList(h2(class = "profile-code", gsub("-", "\u00b7", substr(code, 1L, 4L), fixed = TRUE),
               if (nchar(code) > 4L) substr(code, 5L, 6L)),
      tags$ul(lapply(letters, function(letter) tags$li(if (letter == "-") "Mixed preference" else style_words[[letter]]))),
      p(class = "muted", "Mixed preferences count as neutral. The optional -A/-T suffix is displayed only."))
  })
  ranked <- reactive({
    value <- cv()
    code <- profile()
    if (is.null(value$sections) || is.null(code)) return(NULL)
    rank_roles(value$sections, code)
  })
  active_role <- reactive({
    results <- ranked()
    req(length(results) > 0)
    id <- input$chosen_role
    if (length(id) && id %in% names(head(results, 3L))) results[[id]] else results[[1]]
  })
  guard <- function(prefix) {
    empty <- function(title, message, suffix, label)
      card(h2(title), p(message), actionButton(paste0(prefix, suffix), label, class = "primary"))
    if (is.null(cv()$sections)) return(empty("Add your r\u00e9sum\u00e9 first.", "We need some experience or skills to work from.", "_cv", "Go to r\u00e9sum\u00e9"))
    if (is.null(profile())) return(empty("Complete your assessment.", "Add your work style to see tailored suggestions.", "_quiz", "Go to assessment"))
    if (!length(ranked())) return(empty("No role evidence found yet.",
      "This prototype covers eight roles. Try adding more specific skills, or explore the sample r\u00e9sum\u00e9.", "_cv", "Go to r\u00e9sum\u00e9"))
    NULL
  }
  navigate <- function(button, page) observeEvent(input[[button]], updateNavbarPage(session, "page", selected = page))
  navigate("to_quiz", "assessment")
  navigate("to_match", "matches")
  navigate("see_suggestions", "suggestions")
  navigate("match_cv", "resume")
  navigate("match_quiz", "assessment")
  navigate("suggest_cv", "resume")
  navigate("suggest_quiz", "assessment")
  navigate("back_match", "matches")
  output$matches <- renderUI({
    missing <- guard("match")
    if (!is.null(missing)) return(missing)
    results <- head(ranked(), 3L)
    div(class = "split",
      card(kicker("RANKED ROLE MATCHES"),
        selectInput("chosen_role", "Explore a role", choices = setNames(names(results),
          vapply(results, function(r) sprintf("%s  \u00b7  %.0f/100", r$title, r$score), character(1)))),
        uiOutput("score"),
        actionButton("see_suggestions", "see career path suggestions", class = "primary")),
      card(kicker("THE CAREER CARICATURE"), h2("Picture yourself in the role"),
        uiOutput("portrait"),
        tags$details(tags$summary("Customize your caricature"),
          div(class = "controls",
            selectInput("skin", "Skin tone", c("Light" = "light", "Warm" = "warm", "Brown" = "brown", "Deep" = "deep"), selected = "warm"),
            selectInput("hair", "Hair", c("Swept" = "sweep", "Curly" = "curls", "Close crop" = "close", "None" = "none")),
            checkboxInput("glasses", "Glasses"))),
        downloadButton("download_portrait", "Download SVG", class = "secondary")))
  })
  output$score <- renderUI({
    r <- active_role()
    tagList(h2(r$title), div(class = "score", sprintf("%.0f", r$score), span(" / 100")),
      p(class = "muted", sprintf("%.1f from r\u00e9sum\u00e9 evidence + %.1f from work style",
        resume_share * r$resume, (1 - resume_share) * r$style_points)),
      lapply(names(weights), function(label) meter(label, r$points[[label]], weights[[label]])),
      p(class = "muted", "Bars show r\u00e9sum\u00e9 points before the 85% multiplier. Use the score to explore career options."),
      p(strong("Skills spotted: "), if (length(r$matched)) paste(r$matched, collapse = ", ") else "None from this role's skill list."))
  })
  image_svg <- reactive({
    skin <- if (isTRUE(input$skin %in% c("light", "warm", "brown", "deep"))) input$skin else "warm"
    hair <- if (isTRUE(input$hair %in% c("sweep", "curls", "close", "none"))) input$hair else "sweep"
    portrait(active_role(), skin, hair, input$glasses)
  })
  output$portrait <- renderUI(div(class = "portrait", HTML(image_svg())))
  output$download_portrait <- downloadHandler(
    filename = function() paste0(tolower(gsub("[^A-Za-z0-9]+", "-", active_role()$title)), ".svg"),
    content = function(file) writeLines(image_svg(), file), contentType = "image/svg+xml")
  output$suggestions <- renderUI({
    missing <- guard("suggest")
    if (!is.null(missing)) return(missing)
    r <- active_role()
    tagList(h2(r$title), div(class = "recommendations",
      card(kicker("01 / BUILD"), h3("Portfolio project"), p(r$project)),
      card(kicker("02 / PRACTICE"), h3("Skills to explore"),
        if (length(r$gaps)) tagList(p("These skills were not detected in your r\u00e9sum\u00e9:"),
          tags$ul(lapply(head(r$gaps, 4), tags$li))) else p("Your r\u00e9sum\u00e9 mentions all tracked skills. Use the project to deepen one and show your work.")),
      card(kicker("03 / STUDY"), h3("Course"), p(external(r$course, r$course_url))),
      card(kicker("04 / VALIDATE"), h3("Certificate or certification"),
        p(external(r$credential, r$credential_url)), p(class = "muted", "Check prerequisites and cost before enrolling."))),
      actionButton("back_match", "Back to role matches", class = "secondary"))
  })
}
shinyApp(ui, server)
