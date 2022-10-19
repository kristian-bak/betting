# Betting 2.8.0

* Adding home vs away summary

# Betting 2.7.0

* Adding plot with earnings by week

# Betting 2.6.0

* Color adjustment and stressing earnings for all data

# Betting 2.5.0

* Reducing number of columns in earnings summary by adding stress switch (All tabs but Bets per match)

# Betting 2.4.1

* Fixing bug in stake tab related to full join with non-unique column names
* Allowing NA values in Bookmaker column

# Betting 2.4.0

* Adding median odds to earning table

# Betting 2.3.0

* Adding calculation of stressed earnings 
* Updating unit tests such that they reflect stressed calculations as well

# Betting 2.2.0

* Adding earnings tab for days betwen betday and matchday

# Betting 2.1.0

* Adding earnings tab for country and bookmaker
* Adding filter options for country and bookmaker
* Refactoring server code in filter module such that copy pasted code is replaced a function used multiple times

# Betting 2.0.5

* Adjusting input validation such that more flexible datasets are allowed

# Betting 2.0.4

* Generalizing the code related to cutpoints for odds and stake inputs
* Generalizing the code related to text paragraphs
* Adding dependency towards `{glue}`

# Betting 2.0.3

* Adding modules for plotting earnings and distributions

# Betting 2.0.2

* Adding hover on drop down button

# Betting 2.0.1

* Including `shinyWidgets` to dependency list

# Betting 2.0.0

* Initial split of summary module into multiple modules
* Hiding filters using the `dropdownButton` in order to simplify the interface

# Betting 1.14.0

* Adding a `dropdownButton` which allows to group game types together

# Betting 1.13.0

* Changing exposure definition such that exposure is defined based on $\text{stake} > 0$
* Adding smart stake calculator tab

# Betting 1.12.0

* Reducing number of columns in the output of loaded data
* Ensuring subsetting works for x-lings 

# Betting 1.11.4

* Simplifying output of loaded data

# Betting 1.11.3

* Included input validation when uploading excel sheet

# Betting 1.11.2

Changes made related to:

* `show_earning_panel` allows additional UI now (used to add a `p()`)
* NA value in stake group has been removed
* Odds group is more intuitive now (capped at 5 and last interval indeed is (4, 5])

# Betting 1.11.1

* Fixing a bug related to the ordering of the stats table in odds group tab

# Betting 1.11.0

Changes made related to:

* Added the possibility to observe the initial loaded dataset
* Fixing a bug in calculate the winning streak

# Betting 1.10.1

* Fixing bug in `cumsum_with_reset` for scalar inputs

# Betting 1.10.0

* Allowing multiple selections for filters

# Betting 1.9.0

* Adding info box related to winning streak

# Betting 1.8.1

* Reverting automatic update of data due to HTTP error 404

# Betting 1.8.0

* Adding automatic update of data

# Betting 1.7.1

* Adding multiple unit tests

# Betting 1.7.0

* Adding a tab for earnings calculations grouped by `stake`

# Betting 1.6.0

* Adding upload data module

# Betting 1.5.0

* Adding `calculate_smart_stake`
* Including subgames for doubles and tribles 
* Initializing single plotting rather than having earnings twice

# Betting 1.4.0

* Adding double y axis plotting feature

# Betting 1.3.0

* Adding `color_by` which allows for simple coloring of `DT::datatable`

# Betting 1.2.0

* Adding summary of selected covariate using `summary`

# Betting 1.1.1

* Fixing bug related to number of rows in pop up dataset for "number of bets per game" does not match the summarised earnings table

# Betting 1.1.0

* Adding tab related to number of bets per game

# Betting 1.0.1

* Adding app.R in order to deploy the app to shinyapps.io

# Betting 1.0.0

* Translating columns in data from danish to English
* Creating unit test for `calculate_earnings`
* Adding info circle to stake selectizeInput

# Betting 0.4.0

* Adding more filter options

# Betting 0.3.0

* Adding filter options on historical bets (time period and team selection)

# Betting 0.2.0

* Adding several UI elements

# Betting 0.1.0

* Setting up empty dashboard

# Betting 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
