source("./src/data_pathes.R")

external_folder <- external_data_folder()
# Change to official response .csv when ready
response_path <- file.path(external_folder, "test_example_survey_response.csv")

d <- fread(response_path)

# Standard prefix to the course number 
hours_text <- "Of the MIDS courses you've taken please indicate how many hours a week each course took you (this includes asynchronous material, studying, homework and office hours). "

# Create usable/readable table names
rename_list <- list(submit_time = "Timestamp",
                    recorded_email = "Email Address",
                    submitted_email = "Please enter your Berkeley student email address",
                    w200_hours = paste(hours_text,  "[W200]"),
                    w201_hours = paste(hours_text,  "[W201]"),
                    w203_hours = paste(hours_text,  "[W203]"),
                    w205_hours = paste(hours_text,  "[W205]"),
                    w207_hours = paste(hours_text,  "[W207]"),
                    w209_hours = paste(hours_text,  "[W209]"),
                    w210_hours = paste(hours_text,  "[W210]"),
                    w231_hours = paste(hours_text,  "[W231]"),
                    w233_hours = paste(hours_text,  "[W233]"),
                    w241_hours = paste(hours_text,  "[W241]"),
                    w251_hours = paste(hours_text,  "[W251]"),
                    w261_hours = paste(hours_text,  "[W261]"),
                    w266_hours = paste(hours_text,  "[W266]"),
                    w271_hours = paste(hours_text,  "[W271]"),
                    time_requirement = "For the classes that took the most time, what requirement was the most time consuming?",
                    useful_courses = "Which courses did you find the most useful?",
                    useful_explanation = "Please explain your answer from above in more detail. Why were the classes you selected useful?",
                    new_courses = "What kind of new course would you like to see in the program?"
                    )

for (label in names(rename_list)){
  new_name <- label
  old_name <- rename_list[[label]]
  setnames(d, old_name, new_name)
}


# Create an empty "useful classes" table to put the response for "useful" class
# into it's own column by class
create_useful_classes_table <- function(n){
  class_list <- c("w200_useful", "w201_useful", "w203_useful", "w205_useful",
                  "w207_useful", "w209_useful", "w210_useful",
                  "w231_useful", "w241_useful", "w251_useful", "w261_useful", "w266_useful", "w277_useful")
  
  dt <- data.table(idx=c(1:n))
  dt[, (class_list) :=0]
  dt[, recorded_email :=""]

  return(dt[, .SD, .SDcols = !(c('idx'))])  
}

# Return, in list form, the recorded email (which will be used to rejoin the useful
# class tables) and the useful classes 
parse_useful_classes <- function(email, useful_course_response){
  list(recorded_email=email, useful_courses= useful_course_response)
}

match_class_pattern <- function(class_str, reference_classes){
  split_str_list <- str_split(class_str, ',')
  split_str_vect <- split_str_list[[1]]
  result_vect <- rep(0, length(reference_classes))
  for (word in split_str_vect){
    for (i in 1:length(reference_classes)){
      print(reference_classes[i])
      print(word)
      match <- grepl(reference_classes[i], word, ignore.case=TRUE)
      print(match)
      print(paste(result_vect[i], i))
      if (match){
        result_vect[i] <- match
      }
    }
  }
  return(result_vect)
}

assign_values_to_useful_classes <- function(useful_class_dt, response_dt){
  class_vect <- c("w200", "w201", "w203", "w205", "w207", "w209", "w210",
                  "w231", "w241", "w251", "w261", "w266", "w277", "recorded_email")
  l <- vector("list", length(class_vect))
  names(l) <- class_vect
  useful_class_list <- parse_useful_classes(response_dt[, recorded_email], response_dt[, useful_courses])
  
  for (i in 1:response_dt[, .N]){
    email <- useful_class_list[["recorded_email"]][i]
    class_string <- useful_class_list[['useful_courses']][i]
    class_match <- match_class_pattern(class_string, class_vect[1:length(class_vect)-1])
    row <- c(class_match, email)
    row_list <- as.list(row)
    names(row_list) <- names(useful_class_dt)
    set(useful_class_dt, i, names(useful_class_dt), as.list(row))
  }
  return(useful_class_dt)
}

