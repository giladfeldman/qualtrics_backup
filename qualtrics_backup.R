this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)

# this is the only one that has the fetch_qsf function, it's a minor tweak. The other version did not export QSFs correctly.
# see https://github.com/ropensci/qualtRics/issues/299#issuecomment-1342823502 for an explanation
remotes::install_github("ropensci/qualtRics", ref = "0060364")

#
# Load packages
#


if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(
  tidyverse, 
  httr, 
  jsonlite, 
  fs, 
  here, 
  qualtRics, 
  rlist, 
  haven)

#
# Functions
#

# Function to check if a folder exists, and create it if it doesn't
create_folder_if_not_exists <- function(folder_name) {
  if (!dir.exists(folder_name)) {
    dir.create(folder_name)
    message(paste("Folder", folder_name, "created."))
  } else {
    message(paste("Folder", folder_name, "already exists."))
  }
}

clean_names <- function(names) {
  # Replace spaces and any other invalid characters with an underscore
  names <- gsub("[^a-zA-Z0-9_]+", "_", names)
  # Ensure names do not start with a number
  names <- gsub("^[0-9]+", "_", names)
  # Ensure no leading or trailing underscores
  names <- gsub("^_|_$", "", names)
  
  names <- tolower(names)
  
  # Truncate names to a shorter length (e.g., 60 characters) to leave
  # room for `make.names` to add a unique suffix (like .1, .2, etc.).
  names <- substr(names, 1, 60)
  
  # Make names unique and valid for R. This is now more reliable since
  # we ensured the base names are short enough to prevent truncation issues.
  unique_names <- make.names(names, unique = TRUE)
  
  # SPSS does not allow periods, so we replace them with underscores.
  final_names <- gsub("\\.", "_", unique_names)
  
  # Finally, ensure the names do not exceed the 64-character limit after this step
  # (this is a failsafe, but should not be necessary with the previous steps).
  final_names <- substr(final_names, 1, 64)
  
  return(final_names)
}

save_to_spss <- function(df, file_path) {
  # Check if the input is a data frame
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }
  
  # Check if the file path is provided
  if (missing(file_path)) {
    stop("Please provide a file path to save the data.")
  }
  
  # Apply the name cleaning to the data frame columns
  names(df) <- clean_names(names(df))
  
  # Truncate factor levels to a maximum of 120 characters to meet SPSS requirements
  for (i in seq_along(df)) {
    if (is.factor(df[[i]])) {
      levels(df[[i]]) <- substr(levels(df[[i]]), 1, 50)
    }
  }

  # Use haven::write_sav to save the data frame
  tryCatch({
    write_sav(df, file_path)
    cat(paste0("Successfully saved data frame to '", file_path, "'.\n"))
    return(invisible(file_path))
  }, error = function(e) {
    stop(paste("An error occurred while saving the file:", e$message))
  })
}

#Process a survey - save its data to the data folder, 
#metadata to the metadata folder
#Input the survey_id
process_survey = function(survey_id){
  
  survey1 = fetch_survey(surveyID = survey_id, 
                         save_dir = here("data"), 
                         verbose = TRUE, 
                         force_request = TRUE,
                         include_display_order = T)
  #Save the data for the survey
  s1_mdata = metadata(survey_id)
  s1_qdata = survey_questions(survey_id)
  s1_rdata = survey1

  #Set the filenames for the surveys
  metadata_filename = paste0(here("metadata//"), "metadata-", survey_id, '.yaml')
  qdata_filename = paste0(here("question_data//"), "question_data-", survey_id, '.Rds')
  responsedata_filename = paste0(here("data//"), "response_data-", survey_id, '.Rds')
  responsedata_sav_filename = paste0(here("data//"), "response_data-", survey_id, '.sav')
  
  #Save all of the data into the appropriate folders
  list.save(s1_mdata, metadata_filename)
  saveRDS(s1_qdata, qdata_filename)
  saveRDS(s1_rdata, responsedata_filename)
  
  # Also saving it as an SPSS .SAV.
  save_to_spss(s1_rdata, responsedata_sav_filename)
  
}

#
# Input needed data
#


# Set up your Qualtrics API credentials
api_token <- "<YOUR_API_HERE>" #Put your api token here, replace the bracketed text
base_url <- "<YOUR_BASE_URL_HERE>" #Put your datacenter here in the bracketed text -- e.g. for me it is ca1.qualtrics.com


#
# Run below here to download surveys
#


#Create folders we need if they do not already exist
needed_folders = c("data", "metadata", "question_data", "qsf")
lapply(needed_folders, create_folder_if_not_exists)

#Save the API credentials for use with qualtRics
qualtrics_api_credentials(api_key = api_token, 
                          base_url = base_url,
                          install = TRUE,
                          overwrite=TRUE)


# Set up headers for the API requests
headers = c(
  "X-API-TOKEN" = api_token,
  "Content-Type" = "application/json"
)

#Helped some people avoid 504 errors
options(qualtrics_timeout = 1200)

# # Main script to download all survey qsfs
surveys = all_surveys()
# 
# #Save a file with the surveys for mapping ID to name later
backup_name = paste(Sys.time(), "_surveybackups")
backup_name = gsub("[^A-Za-z0-9]", "_", backup_name)
backup_name = paste0(backup_name, ".Rds")
saveRDS(surveys, backup_name)

# If you want to read a specific backup, use something like this:
# surveys <-readRDS("2025_08_23_13_19_04_955285__surveybackups.Rds")

#Get all the QSFs
for (i in 1:nrow(surveys)) {
  survey_id = surveys$id[i] #Get ID
  survey_name = gsub("[^A-Za-z0-9]", "_", surveys$name[i]) # Clean up survey
  # download_qsf(survey_id, survey_name) #Download the qsf
  qsfsurvey <- qualtRics::fetch_qsf(survey_id)
  writeLines(qsfsurvey,  paste0("qsf/", survey_name, ".qsf"))
  message(paste("Successfuly downloaded .qsf:",  paste0("qsf/", survey_name, ".qsf")))
  Sys.sleep(5) #Sleep to avoid getting throttled
}

# Get all the data and metadata

#Save if each survey succeeded or failed
survey_status = rep(NA, nrow(surveys))

#Loop and try to get all surveys
for(i in 1:nrow(surveys)){
  print(paste("Processing survey ", i, " of ", nrow(surveys)))
  #Set attempts counter and max attempts
  attempts = 0
  max_attempts = 5
  success = FALSE
  #While we have not succeeded and attempts is less than max attempts,
  #run the function
  while(attempts < max_attempts & !success){
    attempts = attempts + 1 #increment attempt counter
    tryCatch({ #try to process the survey
      process_survey(surveys$id[i])
      success = TRUE #if it does process, set success to true
      survey_status[i] = "Complete"
    }, error = function(e) { #otherwise...
      #Print that you got an error
      print(paste("Error on ",i))
      if(attempts < max_attempts){
        print("Retrying...")
      } else {
        print("Failed")
        survey_status[i] = "Failed"
      }
    }
    )
  }
}

#Check if any failed?
sum(is.na(survey_status))
#Print which failed
print(surveys$id[is.na(survey_status)])
