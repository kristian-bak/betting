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

## Add test
usethis::use_test("add_kr")
usethis::use_test("add_pct")
usethis::use_test("cumsum_with_reset")
usethis::use_test("calculate_earnings")
usethis::use_test("calculate_earnings_for_bets_in_game")
usethis::use_test("plotly_double_yaxis")

## Check everything
devtools::check()

## Bump version number
usethis::use_version()

## Deploy
rsconnect::deployApp()

## Code coverage (restart first, once devtools::install() needed)
devtools::test_coverage()

## Install locally
devtools::install()

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
usethis::use_package("htmltools")
usethis::use_package("curl")
usethis::use_package("shinyWidgets")
usethis::use_package("glue")

remotes::install_github("kristian-bak/kb.utils")

## Add module
golem::add_module(name = "upload_data")
golem::add_module(name = "calculator")
golem::add_module(name = "infobox")
golem::add_module(name = "filters")
golem::add_module(name = "dropdown_filters")
golem::add_module(name = "plot_earnings")
golem::add_module(name = "plot_distribution")

## Add NEWS
usethis::use_news_md()

## Add pipe
usethis::use_pipe()
