# Career Atlas

Career Atlas is an R Shiny app that suggests roles to explore based on your résumé and work preferences. It shows why each role ranked where it did and gives you a project to try, skills to practice, and links to a course and credential.

## Run locally

Install [R](https://cran.r-project.org/), then run these commands from the project folder:

~~~sh
Rscript setup.R
Rscript run.R
~~~

Run setup once to install the dependencies. Open the URL printed after **Listening on** and keep the terminal open while you use the app. The launcher picks an available local port. In RStudio, run setup once, then open `app.R` and click **Run App**.

## Explore a career

1. Upload a text-based PDF, DOCX, or UTF-8 TXT up to 8 MB, or try the sample résumé. Scanned PDFs need OCR before upload.
2. Answer eight work-style questions for an instant result, or enter your result from the [official 16Personalities test](https://www.16personalities.com/free-personality-test).
3. Compare role matches, inspect the score breakdown, and customize or download the career caricature as an SVG.
4. Click **see career path suggestions** for the selected role’s portfolio project, learning priorities, course, and credential.

## How matching works

Résumé evidence contributes 85% of the role score. The résumé breakdown allocates up to 40 points to skills, 30 to experience, 15 to projects, 10 to education, and 5 to certifications. The app uses section headings and keywords to look for evidence relevant to each role. Work preferences contribute the remaining 15%.

The catalog covers eight roles. Scores are heuristic guides for career exploration. Uploads are processed by the Shiny server without sending them to an external service or saving them to a database.

Edit role profiles and recommendations in [`R/catalog.R`](R/catalog.R) and scoring rules in [`R/engine.R`](R/engine.R). Run the workflow checks with `Rscript tests/workflow.R`.
