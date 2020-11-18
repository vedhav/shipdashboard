
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shipdashboard

The goal of shipdashboard is to display a simple shiny app using the
[ships
data](https://drive.google.com/file/d/1IeaDpJNqfgUZzGdQmR6cz2H3EQ3_QfCV)

## Database Setup

  - If you do not wish to populate the MySQL database and access this
    app using local data please switch to **local\_data** branch which
    does not require this setup procedure
  - Use the **init.sql** file inside the **setup** directory to create
    the ships database and tables required for this shiny app
  - After you create the tables you need to make sure to update the
    **app\_constants.R** file with the database constants like hostname,
    username and password.
  - To populate the database with the ships data run the
    **populate\_ships.R** in the **setup** directory.

## Demo

This app is hosted here: [main
branch](https://vedha.tech/shipdashboard),
[local\_data](https://vedha.tech/ship_dashboard)
