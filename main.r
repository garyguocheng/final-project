### Run all code for the project

# Get data
source("./code/clean_data.r")

# Run regression
source("./code/run_reg.r")

# Make figure
source("./code/make_figure.r")

# Run test
testthat::local_edition(3)
testthat::test_dir("test")
