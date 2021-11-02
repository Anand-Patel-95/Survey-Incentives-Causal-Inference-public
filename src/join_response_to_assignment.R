library(data.table)
source("./src/data_pathes.R")
library(dtplyr)

interim_folder <- interim_data_folder()

responses <- file.path(interim_folder, "pilot_processed_survey_results.csv")
assignment <- file.path(interim_folder, "treatment_assignment.csv") 

responses <- fread(responses)
assignment <- fread(assignment)

responses[, response:=1]
assignment[, join_email := gsub('ischool.',"", email)]


# Simple Join -- leaves unmatched responses 
assignment_response <- left_join(assignment, responses,
                                 by=c('join_email' = 'recorded_email')) %>%
  as.data.table()

csv_path <- file.path(interim_folder, 'simple_assignment_response_match.csv')
write.csv(assignment_response[!is.na(submit_time), ], csv_path)

## Logic for matching 

length(which(responses[, recorded_email] %in% assignment_response[, join_email]))

# Unmatched emails
unmatched_email_idx <- which(!(responses[, recorded_email] %in% assignment_response[, join_email]))
unmatched_responses <- responses[unmatched_email_idx, ]

# Our personal responses that we do not expect to have a match for 
experimenter_emails <- c('alexandradrossos@berkeley.edu', 
                         'anand.patel@berkeley.edu',
                         'sweston@berkeley.edu',
                         'carlos.moreno@berkeley.edu')
unmatched_responses <- unmatched_responses[!(recorded_email %in% experimenter_emails), ]

## Possbile Matching strategies
# Convert names from `First Last` to 'firstlast@berkeley.edu` 

# Strip trailing digits `chars123@berkeley.edu` to `chars@berkeley.edu`
