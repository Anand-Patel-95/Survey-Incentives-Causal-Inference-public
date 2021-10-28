source("./src/data_pathes.R")

external_folder <- external_data_folder()
# Change to official response when ready
response_path <- file.path(external_folder, "test_example_survey_response.csv")

d <- fread(response_path)
