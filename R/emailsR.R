get_quarter_strings <- function(base_name, suffix = "Investor Letter") {
  # Current date
  today <- Sys.Date()
  current_year <- as.numeric(format(today, "%Y"))
  current_month <- as.numeric(format(today, "%m"))

  # Determine current and previous quarters
  current_quarter <- ceiling(current_month / 3)
  previous_quarter <- if (current_quarter == 1) 4 else current_quarter - 1
  previous_year <- if (current_quarter == 1) current_year - 1 else current_year

  # Construct strings
  current_quarter_str <- paste(base_name, paste0(current_quarter, "Q"), current_year, suffix)
  previous_quarter_str <- paste(base_name, paste0(previous_quarter, "Q"), previous_year, suffix)

  c(current_quarter_str, previous_quarter_str)
}


search_and_save_emails_multi_search <- function(search_terms, search_in_folder, save_in_folder) {

  library(Microsoft365R)
  library(pagedown)

  # Authenticate
  outlook <- get_business_outlook()
  inbox <- outlook$get_inbox()
  output_dir <- save_in_folder
  existing_files <- list.files(output_dir, pattern = "\\.pdf$", ignore.case = TRUE)

  # Recursive search function
  search_in_all_inbox_folders <- function(folder) {
    messages <- folder$list_emails(n = 1000)
    subfolders <- folder$list_folders()
    for (sf in subfolders) {
      messages <- c(messages, search_in_all_inbox_folders(sf))
    }
    messages
  }

  # Get emails
  if (search_in_folder == "MainInbox") {
    emails <- inbox$list_emails(n = 1000)
  } else if (search_in_folder == "") {
    emails <- search_in_all_inbox_folders(inbox)
  } else {
    nav_folder <- inbox$get_folder(search_in_folder)
    emails <- nav_folder$list_emails(n = 1000)
  }

  # Filter emails containing any of the search strings
  emails <- Filter(function(m) {
    subj <- m$properties$subject rlang::`%||%` ""
    any(sapply(search_terms, function(term) grepl(term, subj, fixed = TRUE, ignore.case = TRUE)))
  }, emails)

  if (length(emails) == 0) {
    message("No matching emails found.")
    return(NULL)
  }

  # Keep only the last received email across all matches
  latest_email <- emails[[which.max(sapply(emails, function(m) as.POSIXct(m$properties$receivedDateTime)))]]

  # Process the email
  tryCatch({
    attachments <- tryCatch(latest_email$list_attachments(), error = function(e) NULL)

    if (!is.null(attachments) && length(attachments) > 0) {
      # Download PDF attachments
      for (att in attachments) {
        att_name <- att$properties$name
        if (grepl("\\.pdf$", att_name, ignore.case = TRUE) && !(att_name %in% existing_files)) {
          save_path <- file.path(output_dir, att_name)
          att$download(dest = save_path, overwrite = TRUE)
          message("Saved attachment: ", att_name)
          existing_files <- c(existing_files, att_name)
        }
      }
    } else {
      # Save email body as PDF
      body_text <- latest_email$properties$body$content
      body_html <- paste0("<html><body><pre>", body_text, "</pre></body></html>")
      temp_html <- tempfile(fileext = ".html")
      writeLines(body_html, temp_html)

      safe_subject <- gsub("[/:*?\"<>|]", "_", latest_email$properties$subject)
      pdf_file <- file.path(output_dir, paste0(safe_subject, ".pdf"))

      chrome_print(input = temp_html, output = pdf_file, wait = 1, timeout = 60)
      message("Saved body as PDF: ", pdf_file)
    }
  }, error = function(e) {
    message("Error processing email '", latest_email$properties$subject, "': ", e$message)
  })

  message("Done processing the latest matching email.")
}



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
  if(search_in_folder=="MainInbox")
  {
    emails <- inbox$list_emails(search = paste0("subject:'", search_for_subject, "'"), n = 1000)
  }else{
    if (search_in_folder == "") {
      cat("Searching Inbox and all subfolders...\n")
      emails <- search_in_all_inbox_folders(inbox, search_for_subject, is_top_level = TRUE)
    } else {
      cat("Searching only in folder:", search_in_folder, "\n")
      nav_folder <- inbox$get_folder(search_in_folder)
      emails <- nav_folder$list_emails(search = paste0("subject:'", search_for_subject, "'"), n = 1000)
    }
  }

  emails <- Filter(function(m) grepl(search_for_subject, m$properties$subject rlang::`%||%` "", fixed = TRUE, ignore.case = TRUE), emails)

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


  if(keep_all=="Last")
  {
    emails_to_download <- list(emails_to_download[[which.max(sapply(emails_to_download, function(m) as.POSIXct(m$properties$receivedDateTime)))]])
  }

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

search_and_save_emails_no_attachment <- function(search_for_subject, search_in_folder, save_in_folder, keep_all = "Last") {

  library(Microsoft365R)
  library(pagedown)

  # Authenticate
  outlook <- get_business_outlook()

  # Get inbox
  inbox <- outlook$get_inbox()

  # Recursive search function
  search_in_all_inbox_folders <- function(folder, search_for_subject, is_top_level = FALSE) {
    if (is_top_level) {
      cat("Searching main Inbox...\n")
      messages <- folder$list_emails(search = paste0("subject:'", search_for_subject, "'"), n = 1000)
    } else {
      messages <- folder$list_emails(search = paste0("subject:'", search_for_subject, "'"), n = 1000)
    }

    subfolders <- folder$list_folders()
    for (sf in subfolders) {
      cat("Searching subfolder:", sf$displayName, "\n")
      messages <- c(messages, search_in_all_inbox_folders(sf, search_for_subject))
    }
    messages
  }

  # ---- Main logic ----
  if (search_in_folder == "MainInbox") {
    emails <- inbox$list_emails(search = paste0("subject:'", search_for_subject, "'"), n = 1000)
  } else if (search_in_folder == "") {
    cat("Searching Inbox and all subfolders...\n")
    emails <- search_in_all_inbox_folders(inbox, search_for_subject, is_top_level = TRUE)
  } else {
    cat("Searching only in folder:", search_in_folder, "\n")
    nav_folder <- inbox$get_folder(search_in_folder)
    emails <- nav_folder$list_emails(search = paste0("subject:'", search_for_subject, "'"), n = 1000)
  }

  # Filter emails by subject
  emails <- Filter(function(m) grepl(search_for_subject, m$properties$subject rlang::`%||%` "", fixed = TRUE, ignore.case = TRUE), emails)

  # Keep only emails without attachments
  emails_to_process <- Filter(function(em) {
    attachments <- tryCatch(em$list_attachments(), error = function(e) NULL)
    is.null(attachments) || length(attachments) == 0
  }, emails)

  message("Emails to process: ", length(emails_to_process))

  # Keep only the last if requested
  if (keep_all == "Last" && length(emails_to_process) > 0) {
    emails_to_process <- list(emails_to_process[[which.max(sapply(emails_to_process, function(m) as.POSIXct(m$properties$receivedDateTime)))]])
  }

  # Output folder
  output_dir <- save_in_folder

  # Loop through filtered emails
  for (i in seq_along(emails_to_process)) {
    em <- emails_to_process[[i]]
    tryCatch({
      message("Processing email ", i, ": ", em$properties$subject)

      # Get plain text or HTML content
      body_text <- em$properties$body$content
      body_html <- paste0("<html><body><pre>", body_text, "</pre></body></html>")  # wrap in <pre> for simple formatting

      # Temporary HTML file
      temp_html <- tempfile(fileext = ".html")
      writeLines(body_html, temp_html)

      # Create a safe PDF file name
      safe_subject <- gsub("[/:*?\"<>|]", "_", em$properties$subject)
      pdf_file <- file.path(output_dir, paste0(safe_subject, ".pdf"))

      # Convert to PDF with a short wait and increased timeout
      chrome_print(input = temp_html, output = pdf_file, wait = 1, timeout = 60)

      message("Saved PDF: ", pdf_file)

    }, error = function(e) {
      message("Error processing email '", em$properties$subject, "': ", e$message)
    })
  }

  print("Done processing e-mails without attachments")
}

fileemoveR<-function(folder_path,names)
{
  # Define folder path

  # List all files (adjust pattern if you only want PDFs, etc.)
  files <- list.files(folder_path, full.names = TRUE)

  # Define undesired pattern (example: files containing 'temp' or 'old')
  undesired_files <- files[grepl(names, basename(files), ignore.case = TRUE)]

  # Delete them
  file.remove(undesired_files)

}

foldeRarchiveR <- function(latest_folder = paste0(peRformance::get_root_path(), "/Documents - AMCs/8_W_DStrats/9) Fund_Updates/Latest/")) {
  # Load package
  library(fs)

  # Define parent folder
  parent_folder <- dirname(latest_folder)

  # Today's date
  today <- Sys.Date()
  today_str <- format(today, "%Y%m%d")      # e.g., 20251030
  today_yyyymm <- format(today, "%Y%m")     # e.g., 202510

  # Create new folder
  new_folder <- file.path(parent_folder, today_str)
  if (!dir_exists(new_folder)) dir_create(new_folder)

  # Copy all files from Latest to new folder
  files_to_copy <- dir_ls(latest_folder, type = "file")
  if (length(files_to_copy) > 0) {
    file_copy(files_to_copy, new_folder, overwrite = TRUE)
    cat("Copied files to:", new_folder, "\n")
  } else {
    cat("No files to copy from Latest folder.\n")
  }

  # Delete old folders with the same yyyymm (except today's folder)
  all_folders <- dir_ls(parent_folder, type = "directory")
  folders_to_delete <- all_folders[grepl(today_yyyymm, basename(all_folders)) & basename(all_folders) != today_str]

  if (length(folders_to_delete) > 0) {
    for (f in folders_to_delete) {
      Sys.chmod(f, mode = "0777")                  # make sure folder is writable
      unlink(f, recursive = TRUE, force = TRUE)   # delete folder and contents
      cat("Deleted folder:", basename(f), "\n")
    }
  } else {
    cat("No folders to delete.\n")
  }

  invisible(new_folder)  # return the path of the new folder
}
