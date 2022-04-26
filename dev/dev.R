## Document
devtools::document()

## Load all functions
devtools::load_all()

## Run app
run_app()

## Renv
renv::init()
renv::status()

## Bump version number
usethis::use_version()

## Package dependency
usethis::use_package("shiny")
usethis::use_package("shinydashboard")
usethis::use_package("readxl")

remotes::install_github("kristian-bak/kb.utils")

## Add NEWS
usethis::use_news_md()

## Add pipe
usethis::use_pipe()
