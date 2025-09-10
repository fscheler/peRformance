
generate_html_table_calendar_years<-function(df)
{
  
  
  
  # ---- Helper: compute calendar year return ----
  compute_calendar_return <- function(dates, nav, year) {
    end_date   <- as.Date(paste0(year, "-12-31"))
    start_date <- as.Date(paste0(year - 1, "-12-31"))
    
    if (min(dates) > start_date) return("")
    
    end_idx   <- which(dates <= end_date)
    start_idx <- which(dates <= start_date)
    if (length(end_idx) == 0 || length(start_idx) == 0) return("")
    
    end_nav   <- nav[max(end_idx)]
    start_nav <- nav[max(start_idx)]
    
    if (is.na(start_nav) || start_nav == 0) return("")
    
    as.character(scales::percent(end_nav / start_nav - 1, accuracy = 0.1))
  }
  
  # ---- Main results ----
  results_base <- df %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(Category, IB_ID, AccountAlias, currency) %>%
    arrange(Date) %>%
    summarise(
      last_date = max(Date, na.rm = TRUE),
      last_nav  = last(NAV),
      all_dates = list(Date),
      all_nav   = list(NAV),
      .groups = "drop"
    )
  
  # Define global last date for consistency
  global_last_date <- max(results_base$last_date, na.rm = TRUE)
  
  # Which years to show
  years_to_show <- (year(global_last_date) - 1):(year(global_last_date) - 4)
  
  results <- results_base %>%
    rowwise() %>%
    mutate(
      # YTD return
      YTD = {
        current_year <- year(global_last_date)
        start_date <- as.Date(paste0(current_year - 1, "-12-31"))
        
        if (min(all_dates) > start_date) "" else {
          ref_idx <- max(which(all_dates <= start_date))
          ref_nav <- all_nav[ref_idx]
          if (is.na(ref_nav) || ref_nav == 0) "" else
            as.character(scales::percent(last_nav / ref_nav - 1, accuracy = 0.1))
        }
      },
      
      # Volatility 1Y
      `Vol 1Y` = {
        one_year_ago <- global_last_date %m-% years(1)
        
        # subset only last 1Y of data
        nav_1y <- all_nav[all_dates >= one_year_ago]
        dates_1y <- all_dates[all_dates >= one_year_ago]
        
        # Require at least ~1 year of coverage
        if (length(nav_1y) < 2 || max(dates_1y) - min(dates_1y) < 365) {
          ""
        } else {
          rets <- diff(log(nav_1y))
          freq <- ifelse(mean(diff(sort(unique(all_dates)))) <= 8, 252, 52)
          as.character(scales::percent(sd(rets, na.rm = TRUE) * sqrt(freq), accuracy = 0.1))
        }
      }
      
    ) %>%
    ungroup()
  
  # Add calendar year returns dynamically
  for (yr in years_to_show) {
    results[[as.character(yr)]] <- mapply(
      function(d, n) compute_calendar_return(d, n, yr),
      results$all_dates,
      results$all_nav
    )
  }
  
  # Format output
  results <- results %>%
    mutate(
      `As of` = format(last_date, "%d-%b"),
      NAV = comma(last_nav, accuracy = 0.01)
    ) %>%
    dplyr::select(Category, IB_ID, AccountAlias, currency, `As of`, NAV, YTD,
                  all_of(as.character(years_to_show)), `Vol 1Y`)
  
  results <- results %>%
    dplyr::select(-IB_ID) %>% 
    dplyr::rename(
      Strategy = AccountAlias,
      Currency = currency
    )
  
  # Use your results
  results_no_currency <- results %>% dplyr::select(-Currency)
  
  table_html <- "<table style='border-collapse: collapse; width: 100%; font-family: Arial, sans-serif;'>"
  
  # Header
  table_html <- paste0(
    table_html,
    "<tr style='background-color: #04103b; color: white; text-align: center;'>",
    paste0("<th style='padding: 5px;'>", colnames(results_no_currency), "</th>", collapse = ""),
    "</tr>"
  )
  
  # Process currency blocks
  unique_currencies <- unique(results$Currency)
  
  for (curr in unique_currencies) {
    # Spacer row before currency block (except first)
    if (curr != unique_currencies[1]) {
      table_html <- paste0(
        table_html,
        "<tr style='height:15px;'><td colspan='", ncol(results_no_currency), "'></td></tr>"
      )
    }
    
    # Currency row with grey line below
    table_html <- paste0(
      table_html,
      "<tr><td colspan='", ncol(results_no_currency),
      "' style='font-weight:bold; text-align:left; padding:5px; border-bottom:0.5px solid #264478;'>",
      curr,
      "</td></tr>"
    )
    
    # Add rows for this currency
    curr_rows <- results %>% filter(Currency == curr) %>% dplyr::select(-Currency)
    for (i in 1:nrow(curr_rows)) {
      table_html <- paste0(table_html, "<tr style='height:22px;'>") # row height
      
      for (col in colnames(curr_rows)) {
        value <- curr_rows[[col]][i]
        
        # Color positive % green (except Vol 1Y)
        if (grepl("%", value) & col != "Vol 1Y") {
          numeric_val <- as.numeric(gsub("%","",value))
          color <- ifelse(!is.na(numeric_val) & numeric_val > 0, "green", "black")
          display <- ifelse(is.na(numeric_val) | value == "", "", value)
          table_html <- paste0(
            table_html,
            "<td style='padding:5px; color:", color, "; text-align:right;'>", display, "</td>"
          )
        } else if (col %in% c("NAV", "Vol 1Y")) {
          table_html <- paste0(
            table_html,
            "<td style='padding:5px; text-align:right; color:#04103b;'>", value, "</td>"
          )
        } else {
          table_html <- paste0(
            table_html,
            "<td style='padding:5px; text-align:left; color:#04103b;'>", value, "</td>"
          )
        }
      }
      table_html <- paste0(table_html, "</tr>")
    }
  }
  
  table_html <- paste0(table_html, "</table>")
  
  return(table_html)
}


generate_html_table_periods<-function(df)
{
  
  # Helper for multi-year returns
  compute_return <- function(dates, nav, years) {
    last_date <- max(dates)
    
    # Reference date: day before the previous year date
    target_date <- last_date %m-% years(years) - 1
    
    # Filter all NAVs <= target_date (closest available before target)
    valid_idx <- which(dates <= target_date)
    if (length(valid_idx) == 0) return("")  # not enough history
    
    # Take the NAV closest to target_date but not after it
    ref_idx <- valid_idx[which.max(dates[valid_idx])]
    
    last_idx <- which.max(dates)
    
    ref_nav <- nav[ref_idx]
    last_nav <- nav[last_idx]
    
    print(ref_nav)
    
    if (is.na(ref_nav) || ref_nav == 0) return("")
    
    scales::percent(last_nav / ref_nav - 1, accuracy = 0.01)
  }
  
  library(dplyr)
  library(lubridate)
  library(scales)
  
  # Safe compute_return: always returns character
  compute_return <- function(dates, nav, years) {
    last_date <- max(dates)
    ref_date <- (last_date %m-% years(years)) - 1  # day before the same day last year
    
    # Find closest previous date if exact ref_date not available
    if (min(dates) > ref_date) return("")  
    ref_idx <- which.min(abs(dates - ref_date))
    ref_nav <- nav[ref_idx]
    last_nav <- nav[which.max(dates)]
    
    if (is.na(ref_nav) || ref_nav == 0) return("")
    as.character(scales::percent(last_nav / ref_nav - 1, accuracy = 0.1))
  }
  
  # Main results calculation
  results <- df %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(Category,IB_ID, AccountAlias, currency) %>%
    arrange(Date) %>%
    summarise(
      last_date = max(Date),
      last_nav = last(NAV),
      
      # YTD return
      ytd = {
        year_start <- as.Date(paste0(year(max(Date))-1, "-12-31"))
        if (min(Date) > year_start) "" else {
          ref_idx <- which.min(abs(Date - year_start))
          ref_nav <- NAV[ref_idx]
          if (is.na(ref_nav) || ref_nav == 0) "" else
            as.character(scales::percent(last(NAV) / ref_nav - 1, accuracy = 0.1))
        }
      },
      
      # Multi-year returns
      ret_1y = compute_return(Date, NAV, 1),
      ret_2y = compute_return(Date, NAV, 2),
      ret_3y = compute_return(Date, NAV, 3),
      ret_5y = compute_return(Date, NAV, 5),
      
      # 1-year annualized volatility
      #vol_1y = {
      #  nav_1y <- NAV[Date >= max(Date) %m-% years(1)]
      #  if (length(nav_1y) < 2) "" else {
      #    rets <- diff(log(nav_1y))
      #    freq <- ifelse(mean(diff(sort(unique(Date)))) <= 8, 252, 52)
      #    as.character(scales::percent(sd(rets, na.rm = TRUE) * sqrt(freq), accuracy = 0.1))
      #  }
      #},
      
      vol_1y = {
        one_year_ago <- max(Date) %m-% years(1)
        nav_1y <- NAV[Date >= one_year_ago]
        dates_1y <- Date[Date >= one_year_ago]
        
        # Require at least 1 year of span and min 2 observations
        if (length(nav_1y) < 2 || (max(dates_1y) - min(dates_1y)) < 365) {
          ""
        } else {
          rets <- diff(log(nav_1y))
          freq <- ifelse(mean(diff(sort(unique(Date)))) <= 8, 252, 52)
          as.character(scales::percent(sd(rets, na.rm = TRUE) * sqrt(freq), accuracy = 0.1))
        }
      },
      
      .groups = "drop"
    ) %>%
    arrange(Category,AccountAlias, currency) %>%
    mutate(
      `As of` = format(last_date, "%d-%b"),
      NAV = comma(last_nav, accuracy = 0.01)
    ) %>%
    dplyr::select(Category,IB_ID, AccountAlias, currency, `As of`, NAV, ytd, ret_1y, ret_2y, ret_3y, ret_5y, vol_1y)
  
  
  # Prepare data
  results_chr <- results %>%
    mutate(
      Strategy = gsub("_", " ", AccountAlias)
    ) %>%
    dplyr::select(currency,Category, Strategy, `As of`, NAV, ytd, ret_1y, ret_2y, ret_3y, ret_5y, vol_1y) %>%
    mutate(across(everything(), as.character))
  
  # Add empty row between currencies
  results_with_blanks <- results_chr %>%
    dplyr::group_split(currency) %>%
    map_dfr(~ bind_rows(.x, tibble(
      currency = "",
      Strategy = "",
      `As of` = "",
      NAV = "",
      ytd = "", ret_1y = "", ret_2y = "", ret_3y = "", ret_5y = "", vol_1y = ""
    )))
  
  
  results<-results_with_blanks
  
  #------------------------------------------------------------------------------------------------------------------------
  # Remove completely empty rows
  results <- results %>% filter(!(currency == "" & Strategy == "" & `As of` == ""))
  
  # Remove currency column from main table
  results_no_currency <- results %>% dplyr::select(-currency)
  
  # Column name mapping
  col_names_map <- c(
    "Category"="Category",
    "Strategy" = "Strategy",
    "As of" = "As of",
    "NAV" = "NAV",
    "ytd" = "YTD",
    "ret_1y" = "1Y",
    "ret_2y" = "2Y",
    "ret_3y" = "3Y",
    "ret_5y" = "5Y",
    "vol_1y" = "Vol 1Y"
  )
  
  table_html <- "<table style='border-collapse: collapse; width: 100%; font-family: Arial, sans-serif;'>"
  
  # Header
  table_html <- paste0(
    table_html,
    "<tr style='background-color: #04103b; color: white; text-align: center;'>",
    paste0("<th style='padding: 5px;'>", col_names_map[colnames(results_no_currency)], "</th>", collapse = ""),
    "</tr>"
  )
  
  # Process currency blocks
  unique_currencies <- unique(results$currency)
  for (curr in unique_currencies) {
    # Insert a tall spacer row before currency block (skip for first currency)
    if (curr != unique_currencies[1]) {
      table_html <- paste0(
        table_html,
        "<tr style='height:15px;'><td colspan='", ncol(results_no_currency), "'></td></tr>"
      )
    }
    
    # Currency row with grey line below
    table_html <- paste0(
      table_html,
      "<tr><td colspan='", ncol(results_no_currency), "' style='font-weight:bold; text-align:left; padding:5px; border-bottom:0.1px solid #264478;'>",
      curr,
      "</td></tr>"
    )
    
    
    # Add rows for this currency
    curr_rows <- results %>% filter(currency == curr) %>% dplyr::select(-currency)
    for (i in 1:nrow(curr_rows)) {
      #table_html <- paste0(table_html, "<tr>")
      table_html <- paste0(table_html, "<tr style='height:20px;'>")  # <-- increased row height
      for (col in colnames(curr_rows)) {
        value <- curr_rows[[col]][i]
        if (grepl("%", value) & col != "vol_1y") {
          numeric_val <- as.numeric(gsub("%","",value))
          color <- ifelse(!is.na(numeric_val) & numeric_val > 0, "green", "black")
          display <- ifelse(is.na(numeric_val), "", value)
          table_html <- paste0(table_html, "<td style='padding:5px; color:", color, "; text-align:right;'>", display, "</td>")
        } else if (col %in% c("NAV","vol_1y")) {
          table_html <- paste0(table_html, "<td style='padding:5px; text-align:right;'>", value, "</td>")
        } else {
          table_html <- paste0(table_html, "<td style='padding:5px; text-align:left;'>", value, "</td>")
        }
        
      }
      table_html <- paste0(table_html, "</tr>")
    }
  }
  
  table_html <- paste0(table_html, "</table>")
  return(table_html)
}