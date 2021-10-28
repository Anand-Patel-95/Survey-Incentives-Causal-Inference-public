# by: Spencer Weston
## Simple functions that return data directories
## To ensure they work:
## 1. Make sure you have opened the .Rproj file
## 2. Go to Tools -> Global Options -> R Markdown -> Evaluate Chunks in Directory.
## Set this option to "Project".

library(here)

data_folder <- function(){
  project_directory <- here()
  path = file.path(project_directory, "data")
  if (!dir.exists(path)){
    stop("Data folder not found")
  }
  return(path)
}

external_data_folder <- function(){
  data_directory <- data_folder()
  path = file.path(data_directory, "external")
  if (!dir.exists(path)){
    stop("External data folder not found")
  }
  return(path)
}

processed_data_folder <- function(){
  data_directory <- data_folder()
  path = file.path(data_directory, "processed")
  if (!dir.exists(path)){
    stop("Processed data folder not found")
  }
  return(path)
}

interim_data_folder <- function(){
  data_directory <- data_folder()
  path = file.path(data_directory, "interim")
  if (!dir.exists(path)){
    stop("Interim data folder not found")
  }
  return(path)
}