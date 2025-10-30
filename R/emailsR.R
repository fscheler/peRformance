

search_and_save_emails<-function(search_for_subject,search_in_folder,save_in_folder, keep_all="Last")
{
  library(Microsoft365R)
  
  # Authenticate
  outlook <- get_business_outlook()
  
  # Get inbox
  inbox <- outlook$get_inbox()
  
  
  # Function to recursively search Inbox + all subfolders
  search_in_all_inbox_folders <- function(folder, search_for_subject, is_top_level = FALSE) {
    # If it's the top-level Inbox, include it too
    if (is_top_level) {
      cat("Searching main Inbox...\n")
      messages <- folder$list_emails(search = paste0("subject:'", search_for_subject, "'"), n = 1000)
    } else {
      messages <- folder$list_emails(search = paste0("subject:'", search_for_subject, "'"), n = 1000)
    }
    
    # Get and search subfolders
    subfolders <- folder$list_folders()
    for (sf in subfolders) {
      cat("Searching subfolder:", sf$displayName, "\n")
      messages <- c(messages, search_in_all_inbox_folders(sf, search_for_subject))
    }
    
    messages
  }

  
  # ---- Main logic ----
  if (search_in_folder == "") {
    cat("Searching Inbox and all subfolders...\n")
    emails <- search_in_all_inbox_folders(inbox, search_for_subject, is_top_level = TRUE)
  } else {
    cat("Searching only in folder:", search_in_folder, "\n")
    nav_folder <- inbox$get_folder(search_in_folder)
    emails <- nav_folder$list_emails(search = paste0("subject:'", search_for_subject, "'"), n = 1000)
  }

  emails <- Filter(function(m) grepl(search_for_subject, m$properties$subject %||% "", fixed = TRUE, ignore.case = TRUE), emails)
  
  if(keep_all=="Last")
  {
    emails <- list(emails[[which.max(sapply(emails, function(m) as.POSIXct(m$properties$receivedDateTime)))]])
  }

  
  # Define output folder
  output_dir <- save_in_folder
  
  # List all existing Excel files
  existing_files <- list.files(output_dir, pattern = "\\.pdf", ignore.case = TRUE)
  
  # Pre-filter emails: keep only those with at least one attachment not already saved
  emails_to_download <- Filter(function(em) {
    attachments <- tryCatch(em$list_attachments(), error = function(e) NULL)
    if (is.null(attachments) || length(attachments) == 0) return(FALSE)
    
    # Check if any attachment is a new Excel file
    any(sapply(attachments, function(att) {
      att_name <- att$properties$name
      grepl("\\.pdf", att_name, ignore.case = TRUE) && !(att_name %in% existing_files)
    }))
  }, emails)
  
  message("Emails to process: ", length(emails_to_download))
  
  
  # Loop only through filtered emails
  for (i in seq_along(emails_to_download)) {
    em <- emails_to_download[[i]]
    
    tryCatch({
      message("Processing email ", i, ": ", em$subject)
      attachments <- em$list_attachments()
      
      for (att in attachments) {
        att_name <- att$properties$name
        
        # Skip if file exists
        if (att_name %in% existing_files) next
        
        # Only download Excel files
        if (grepl("\\.pdf", att_name, ignore.case = TRUE)) {
          save_path <- file.path(output_dir, att_name)
          att$download(dest = save_path, overwrite = TRUE)
          message("Saved: ", att_name)
          existing_files <- c(existing_files, att_name)  # update existing_files
        }
      }
      
    }, error = function(e) {
      message("Error processing email '", em$subject, "': ", e$message)
    })
  }
  
  
  print("Done processing e-mails")
  
}



