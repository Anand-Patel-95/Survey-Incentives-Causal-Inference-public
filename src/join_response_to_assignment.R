library(data.table)
source("./src/data_pathes.R")
library(dtplyr)
library(dplyr)


# ------------------------------------------------------------------------------
## Initial data work
interim_folder <- interim_data_folder()

responses <- file.path(interim_folder, "pilot_processed_survey_results.csv")
assignment <- file.path(interim_folder, "treatment_assignment.csv") 

responses <- fread(responses)
assignment <- fread(assignment)

## Response data cleaning
responses[, response:=1] # Set all responses to response == 1

# Drop experimenter emails from response
experimenter_emails <- c('alexandradrossos@berkeley.edu', 
                         'anand.patel@berkeley.edu',
                         'sweston@berkeley.edu',
                         'carlos.moreno@berkeley.edu',
                         'madeline_94@berkeley.edu')
responses <- responses[!(recorded_email %in% experimenter_emails), ]

# Assign `join_email` column
responses[ , join_email := recorded_email]

## Assignment Data Cleaning
# Remove 'ischool.' from emails
assignment[, join_email := gsub('ischool.',"", email)] 


no_permutation_match_count <- assignment[join_email %in% responses[, join_email], .N]
response_count <- responses[, .N]
print(paste0(no_permutation_match_count, "/", response_count, ' emails matched with no permutations.'))
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
## Iteratively add email permutations. 
# If we don't find a match, 
# we update the 'join_email' column with a new permutation in the rows where 
# we do NOT already have a match. Once matches for a permutation have been 
# identified, we reset unmatched `join_email`s to their original state.
# We repeat this process with email permutations until all 'recorded_emails' in response
#  have a match in the assignment_response 'join_email' column 

### Permutation 1
name_to_email <- function(full_name){
  # Convert names from `First Last` to 'firstlast@berkeley.edu` in the assignment
  # data table
  no_space_name <- gsub(' ', '', full_name)
  no_space_lowercase_name <- tolower(no_space_name)
  email <- paste0(no_space_lowercase_name, "@berkeley.edu")
  return(email)
}

assignment[!(join_email %in% responses[, join_email]),
           join_email := name_to_email(full_name)] 

name_match_count <- assignment[join_email %in% responses[, join_email], .N]
print(paste0(name_match_count, "/", response_count, ' emails matched with name_to_email() permutation.'))

# Reset join emails where we do not have a match
assignment[!(join_email %in% responses[, join_email]), join_email := gsub('ischool.', "", email)]
### 

### Permutation 2
strip_digits_email <- function(email){
  # Strip trailing digits `chars123@berkeley.edu` to `chars@berkeley.edu`
  no_digit_email <- gsub('*\\d', '', email)
  return(no_digit_email)
}
responses[!(join_email %in% assignment[, join_email]), 
          join_email := strip_digits_email(recorded_email)]

digit_strip_count <- assignment[join_email %in% responses[, join_email], .N]
print(paste0(digit_strip_count, "/", response_count, ' emails matched with strip_digits_email() permutation.'))

# Reset join emails where we do not have a match
responses[!(join_email %in% assignment[, join_email]), 
          join_email := recorded_email]
### 

### Permutation 3
first_name_last_initial_email <- function(full_name){
  lowercase_name <- tolower(full_name)
  split_names <- strsplit(lowercase_name, ' ')
  emails <- rep('', length(split_names))
  for(i in 1:length(split_names)){
    name <- split_names[[i]]
    first_name <- name[1]
    # Some people have more than two names; this might need to be fixed later
    last_name <- name[2]
    last_initial <- substr(last_name, 1, 1)
    email <- paste0(first_name, last_initial, "@berkeley.edu")
    emails[i] <- email
  }
  return(emails)
}

assignment[!(join_email %in% responses[, join_email]),
           join_email := first_name_last_initial_email(full_name)] 

first_last_initial_count <- assignment[join_email %in% responses[, join_email], .N]
print(paste0(first_last_initial_count, "/", response_count, ' emails matched with strip_digits_email() permutation.'))

# Reset join emails where we do not have a match
assignment[!(join_email %in% responses[, join_email]),
           join_email := gsub('ischool.', "", email)]
###

### Permutation 4
first_initial_last_name_email <- function(full_name){
  lowercase_name <- tolower(full_name)
  split_names <- strsplit(lowercase_name, ' ')
  emails <- rep('', length(split_names))
  for(i in 1:length(split_names)){
    name <- split_names[[i]]
    first_name <- name[1]
    # Some people have more than two names; this might need to be fixed later
    last_element <- length(name)
    last_name <- name[last_element]
    first_initial <- substr(first_name, 1, 1)
    email <- paste0(first_initial, last_name, "@berkeley.edu")
    emails[i] <- email
  }
  return(emails)
}
assignment[!(join_email %in% responses[, join_email]),
           join_email:= first_initial_last_name_email(full_name)] 

first_initial_last_count <- assignment[join_email %in% responses[, join_email], .N]
print(paste0(first_initial_last_count, "/", response_count, ' emails matched with first initial last name permutation.'))

# Reset join emails where we do not have a match
assignment[!(join_email %in% responses[, join_email]),
           join_email := gsub('ischool.', "", email)]
###

### Permutation 5 
# In the assignment table, we have lawiskoh@berkeley.edu which matches
# lkoh003@berkeley.edu in the response table. We need to do a first initial last
# name permutation on the assignment table and a digit strip on the response table
# to create the match. 

assignment[!(join_email %in% responses[, join_email]),
           join_email := first_initial_last_name_email(full_name)] 
responses[!(join_email %in% assignment[, join_email]), 
          join_email := strip_digits_email(recorded_email)]

strip_and_first_initial_count <- assignment[join_email %in% responses[, join_email], .N]
print(paste0(strip_and_first_initial_count, "/", response_count, ' emails matched with digit strip and first initial permutations.'))

assignment[!(join_email %in% responses[, join_email]),
           join_email := gsub('ischool.', "", email)]
responses[!(join_email %in% assignment[, join_email]), 
          join_email := recorded_email]
###

### Permutation 6
name_to_email_strip_dashes <- function(full_name){
  # Convert names from `First Last` to 'firstlast@berkeley.edu` in the assignment
  # data table
  no_space_name <- gsub(' ', '', full_name)
  no_space_dash_name <- gsub('-', '', no_space_name)
  no_space_dash_lowercase_name <- tolower(no_space_dash_name)
  email <- paste0(no_space_dash_lowercase_name, "@berkeley.edu")
  return(email)
}
assignment[!(join_email %in% responses[, join_email]),
           join_email := name_to_email_strip_dashes(full_name)] 

strip_dash_count <- assignment[join_email %in% responses[, join_email], .N]
print(paste0(strip_dash_count, "/", response_count, ' emails matched with strip dash permutation.'))

# Reset join emails where we do not have a match
assignment[!(join_email %in% responses[, join_email]), join_email := gsub('ischool.', "", email)]
### 
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
## Manual Matching - Any unmatched emails will be recorded here. A function 
# will print an unmatched email to the console and the user will have the option
# to input a correct email 

match_at_prompt <- function(unmatched_email){
  matched_emails <- rep(NA, length(unmatched_email))
  for (i in  1:length(unmatched_email)){
    email <- unmatched_email[i]
    prompt <- paste0("'",email,"'", "is an unmatched response email. \n", 
                   "Enter a new email to associate with this response. Press 's' to skip or 'q' to quit." )
    response <- readline(prompt)
    if (tolower(response) == 's'){
      matched_emails[i] <- NA
      next
    }
    if (tolower(response) == 'q'){break}
    if(!grepl("@", response)){
      stop(paste("Breaking function:", response, "is not a valid email."))
    }
    
    matched_emails[i] <- response
  }
  return(matched_emails)
}

manual_match <- function(unmatched_emails, match_path){
  # Unmatched emails - a vector of unmatched emails
  # match path - path to both load and save updated matches
  
  if (file.exists(match_path)){
    existing_matches <- fread(match_path)
  }
  # Check for existing matches; clean data table
  matches <- data.table(recorded_email = unmatched_emails, matched_email = "")
  if (file.exists(match_path)){
    matches <- left_join(matches, existing_matches, by=c('recorded_email' = 'recorded_email')) %>% as.data.table()
    matches[ , matched_email.x := ifelse(!is.na(matched_email.y), matched_email.y, NA)]
    matches <- matches[, !c('matched_email.y')]
    names(matches) <- c('recorded_email', 'matched_email')  
  }
  
  # In this situation, all emails have been matched in the join. Manual matching
  # is not necessary
  if(matches[, all(!is.na(matched_email))]){
    write.csv(matches, match_path, row.names = FALSE)
    return(matches)
  }
  
  #matches[is.na(matched_email), matched_email := match_at_prompt(recorded_email)]
  matches[ , matched_email := as.character(matched_email)][is.na(matched_email), matched_email := match_at_prompt(recorded_email)]
  write.csv(matches, match_path, row.names = FALSE)
  return(matches)
}

# Unmatched emails
unmatched_email_idx <- which(!(responses[, join_email] %in% assignment[, join_email]))
unmatched_responses <- responses[unmatched_email_idx, ]

match_csv_path<- file.path(interim_folder, "manual_matches.csv")
responses[recorded_email %in% unmatched_responses[, recorded_email],
          join_email := manual_match(unmatched_responses[, recorded_email],
                                      match_csv_path)[, matched_email]]

manual_match_count <- assignment[join_email %in% responses[, join_email], .N]
print(paste0(manual_match_count, "/", response_count, ' emails matched with manual match permutation.'))
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Join and save data

# Simple Join -- leaves unmatched responses 
assignment_response <- left_join(assignment, responses,
                                 by=c('join_email' = 'join_email')) %>%
  as.data.table()

csv_path <- file.path(interim_folder, 'preliminary_assignment_response_match.csv')
write.csv(assignment_response[!is.na(submit_time), ], csv_path)