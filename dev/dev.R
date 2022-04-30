## Document
devtools::document()

## Load all functions
devtools::load_all()

## Run app
run_app()

## Renv
renv::init()
renv::status()

## Run tests
devtools::test()

## Bump version number
usethis::use_version()

## Package dependency
usethis::use_package("dplyr")
usethis::use_package("shiny")
usethis::use_package("shinydashboard")
usethis::use_package("shinyBS")
usethis::use_package("shinyjs")
usethis::use_package("readxl")
usethis::use_package("DT")
usethis::use_package("plotly")
usethis::use_package("kb.utils")

remotes::install_github("kristian-bak/kb.utils")

## Add test
usethis::use_test("calculate_earnings")

## Check everything
devtools::check()

## Add NEWS
usethis::use_news_md()

## Add pipe
usethis::use_pipe()
