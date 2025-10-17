generate_html_table_calendar_years <- function(df) {

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

  # Global last date
  global_last_date <- max(results_base$last_date, na.rm = TRUE)
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
      # Volatility 1Y per fund
      `Vol 1Y` = {
        fund_last_date <- max(all_dates, na.rm = TRUE)
        one_year_ago <- fund_last_date %m-% years(1)
        nav_1y <- all_nav[all_dates >= one_year_ago & all_dates <= fund_last_date]
        dates_1y <- all_dates[all_dates >= one_year_ago & all_dates <= fund_last_date]
        span_days <- as.numeric(max(dates_1y) - min(dates_1y), units = "days")
        if (length(nav_1y) < 2 || span_days < 360) "" else {
          rets <- diff(log(nav_1y))
          avg_diff_days <- mean(diff(sort(dates_1y)))
          freq <- 365 / as.numeric(avg_diff_days, units = "days")
          as.character(scales::percent(sd(rets, na.rm = TRUE) * sqrt(freq), accuracy = 0.1))
        }
      }
    ) %>%
    ungroup()

  # Calendar year returns
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
                  all_of(as.character(years_to_show)), `Vol 1Y`) %>%
    dplyr::select(-IB_ID) %>%
    dplyr::rename(
      Strategy = AccountAlias,
      Currency = currency
    ) %>%
    # <<< Fix: sort by Category, then Currency, then Strategy >>>
    arrange(Category, Currency, Strategy)

  results_no_currency <- results %>% dplyr::select(-Currency)

  # ---- Generate HTML ----
  table_html <- "<table style='border-collapse: collapse; width: 100%; font-family: Arial, sans-serif;'>"

  # Header
  table_html <- paste0(
    table_html,
    "<tr style='background-color: #04103b; color: white; text-align: center;'>",
    paste0("<th style='padding: 5px;'>", colnames(results_no_currency), "</th>", collapse = ""),
    "</tr>"
  )

  # Currency blocks
  unique_currencies <- sort(unique(results$Currency))
  for (curr in unique_currencies) {
    if (curr != unique_currencies[1]) {
      table_html <- paste0(
        table_html,
        "<tr style='height:15px;'><td colspan='", ncol(results_no_currency), "'></td></tr>"
      )
    }

    table_html <- paste0(
      table_html,
      "<tr><td colspan='", ncol(results_no_currency),
      "' style='font-weight:bold; text-align:left; padding:5px; border-bottom:0.5px solid #264478;'>",
      curr,
      "</td></tr>"
    )

    curr_rows <- results %>% filter(Currency == curr) %>% dplyr::select(-Currency)
    for (i in 1:nrow(curr_rows)) {
      table_html <- paste0(table_html, "<tr style='height:22px;'>")
      for (col in colnames(curr_rows)) {
        value <- curr_rows[[col]][i]
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



generate_html_table_calendar_years_old<-function(df)
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

      # Volatility 1Y per fund
      `Vol 1Y` = {
        # Use the last date of this fund's series
        fund_last_date <- max(all_dates, na.rm = TRUE)
        one_year_ago <- fund_last_date %m-% years(1)

        # Subset NAV and dates in the last 1 year
        nav_1y <- all_nav[all_dates >= one_year_ago & all_dates <= fund_last_date]
        dates_1y <- all_dates[all_dates >= one_year_ago & all_dates <= fund_last_date]

        # Require at least 2 observations AND coverage of ~1 year
        span_days <- as.numeric(max(dates_1y) - min(dates_1y), units = "days")
        if (length(nav_1y) < 2 || span_days < 360) {
          ""  # Not enough data
        } else {
          rets <- diff(log(nav_1y))

          # Annualization factor based on actual spacing
          avg_diff_days <- mean(diff(sort(dates_1y)))
          freq <- 365 / as.numeric(avg_diff_days, units = "days")

          # Annualized volatility
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
  unique_currencies <- sort(unique(results$Currency))

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



#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
generate_html_table_periods <- function(df) {
  library(dplyr)
  library(lubridate)
  library(scales)
  library(purrr)

  # ---- Helper: compute multi-year return safely ----
  compute_return <- function(dates, nav, years) {
    last_date <- max(dates)
    ref_date <- (last_date %m-% years(years)) - 1  # day before same day last year

    # Use last NAV **on or before** ref_date
    valid_idx <- which(dates <= ref_date)
    if (length(valid_idx) == 0) return("")
    ref_idx <- max(valid_idx)

    ref_nav <- nav[ref_idx]
    last_nav <- nav[which.max(dates)]

    if (is.na(ref_nav) || ref_nav == 0) return("")
    as.character(scales::percent(last_nav / ref_nav - 1, accuracy = 0.1))
  }

  # ---- Main calculations ----
  results <- df %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(Category, IB_ID, AccountAlias, currency) %>%
    arrange(Date) %>%
    summarise(
      last_date = max(Date),
      last_nav = last(NAV),

      # YTD return: last NAV on or before Dec 31 previous year
      ytd = {
        year_start <- as.Date(paste0(year(max(Date)) - 1, "-12-31"))
        valid_idx <- which(Date <= year_start)
        if (length(valid_idx) == 0) "" else {
          ref_nav <- NAV[max(valid_idx)]
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
      vol_1y = {
        one_year_ago <- max(Date) %m-% years(1)
        nav_1y <- NAV[Date >= one_year_ago]
        dates_1y <- Date[Date >= one_year_ago]
        span_days <- as.numeric(max(dates_1y) - min(dates_1y), units = "days")
        if (length(nav_1y) < 2 || span_days < 360) {
          ""
        } else {
          rets <- diff(log(nav_1y))
          avg_diff_days <- mean(diff(sort(dates_1y)))
          freq <- 365 / as.numeric(avg_diff_days, units = "days")
          as.character(scales::percent(sd(rets, na.rm = TRUE) * sqrt(freq), accuracy = 0.1))
        }
      },

      .groups = "drop"
    ) %>%
    arrange(Category, AccountAlias, currency) %>%
    mutate(
      `As of` = format(last_date, "%d-%b"),
      NAV = scales::comma(last_nav, accuracy = 0.01)
    ) %>%
    dplyr::select(Category, IB_ID, AccountAlias, currency, `As of`, NAV, ytd, ret_1y, ret_2y, ret_3y, ret_5y, vol_1y)

  # ---- Prepare for HTML ----
  results_chr <- results %>%
    mutate(Strategy = gsub("_", " ", AccountAlias)) %>%
    dplyr::select(currency, Category, Strategy, `As of`, NAV, ytd, ret_1y, ret_2y, ret_3y, ret_5y, vol_1y) %>%
    mutate(across(everything(), as.character))

  # Insert empty row between currencies
  results_with_blanks <- results_chr %>%
    dplyr::group_split(currency) %>%
    map_dfr(~ bind_rows(.x, tibble(
      currency = "",
      Strategy = "",
      `As of` = "",
      NAV = "",
      ytd = "", ret_1y = "", ret_2y = "", ret_3y = "", ret_5y = "", vol_1y = ""
    )))

  results <- results_with_blanks %>%
    filter(!(currency == "" & Strategy == "" & `As of` == ""))

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

  # ---- Generate HTML ----
  table_html <- "<table style='border-collapse: collapse; width: 100%; font-family: Arial, sans-serif;'>"
  # Header
  table_html <- paste0(
    table_html,
    "<tr style='background-color: #04103b; color: white; text-align: center;'>",
    paste0("<th style='padding: 5px;'>", col_names_map[colnames(results_no_currency)], "</th>", collapse = ""),
    "</tr>"
  )

  # Currency blocks
  unique_currencies <- unique(results$currency)
  for (curr in unique_currencies) {
    if (curr != unique_currencies[1]) {
      table_html <- paste0(table_html, "<tr style='height:15px;'><td colspan='", ncol(results_no_currency), "'></td></tr>")
    }
    table_html <- paste0(
      table_html,
      "<tr><td colspan='", ncol(results_no_currency),
      "' style='font-weight:bold; text-align:left; padding:5px; border-bottom:0.1px solid #264478;'>",
      curr,
      "</td></tr>"
    )

    curr_rows <- results %>% filter(currency == curr) %>% dplyr::select(-currency)
    for (i in 1:nrow(curr_rows)) {
      table_html <- paste0(table_html, "<tr style='height:20px;'>")
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



generate_html_table_periods_old<-function(df)
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
      vol_1y = {
        one_year_ago <- max(Date) %m-% years(1)

        # Subset NAV and dates in last 1 year
        nav_1y <- NAV[Date >= one_year_ago]
        dates_1y <- Date[Date >= one_year_ago]

        # Require at least 2 observations AND coverage of at least ~1 year
        span_days <- as.numeric(max(dates_1y) - min(dates_1y), units = "days")
        if (length(nav_1y) < 2 || span_days < 360) {  # 360 to allow for weekends/holidays
          ""
        } else {
          rets <- diff(log(nav_1y))

          # Annualization factor based on actual spacing
          avg_diff_days <- mean(diff(sort(dates_1y)))
          freq <- 365 / as.numeric(avg_diff_days, units = "days")

          as.character(scales::percent(sd(rets, na.rm = TRUE) * sqrt(freq), accuracy = 0.1))
        }
      }
      ,

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


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------


generate_html_table_meta_data <- function(df) {
  library(scales)
  currency_col <- if ("currency" %in% colnames(df)) "currency" else if ("Currency" %in% colnames(df)) "Currency" else stop("No currency column found.")

  # Header display names (keys must match column names exactly)
  col_names_map <- c(
    "Category"     = "Category",
    "Strategy"     = "Strategy",
    "ISIN"         = "ISIN",
    "As of"        = "As of",
    "Mgmt. Fees"   = "Mgmt Fees",
    "Perf. Fees"   = "Perf Fees",
    "Certificates" = "Certificates",
    "AUM"          = "AuM"
  )

  # Sort by currency, then by Category
  results <- df[order(as.character(df[[currency_col]]), as.character(df$Category)), , drop = FALSE]
  results_no_currency <- results[, setdiff(colnames(results), currency_col), drop = FALSE]

  # Start table
  table_html <- "<table style='border-collapse: collapse; width: 100%; font-family: Arial, sans-serif;'>"

  # Header row: left aligned, font color #04103b
  header_labels <- sapply(colnames(results_no_currency), function(nm) {
    if (nm %in% names(col_names_map)) col_names_map[[nm]] else nm
  }, USE.NAMES = FALSE)

  table_html <- paste0(
    table_html,
    "<tr style='background-color: #04103b; color: white; text-align:left;'>",
    paste0("<th style='padding:5px; text-align:left;'>", header_labels, "</th>", collapse = ""),
    "</tr>"
  )

  # Currency blocks
  unique_currencies <- sort(unique(results[[currency_col]]))
  month_abbr_custom <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec")
  first_currency <- unique_currencies[1]

  for (curr in unique_currencies) {
    # Spacer row before currency block (except first)
    if (!is.na(curr) && curr != first_currency) {
      table_html <- paste0(table_html, "<tr style='height:15px;'><td colspan='", ncol(results_no_currency), "'></td></tr>")
    }

    # Currency row
    table_html <- paste0(
      table_html,
      "<tr><td colspan='", ncol(results_no_currency),
      "' style='font-weight:bold; text-align:left; padding:5px; border-bottom:0.5px solid #264478; color:#04103b;'>",
      curr,
      "</td></tr>"
    )

    # Rows for this currency
    curr_rows <- results[as.character(results[[currency_col]]) == as.character(curr), , drop = FALSE]
    curr_rows <- curr_rows[, setdiff(colnames(curr_rows), currency_col), drop = FALSE]

    for (i in seq_len(nrow(curr_rows))) {
      table_html <- paste0(table_html, "<tr style='height:22px;'>")

      for (col in colnames(curr_rows)) {
        raw_val <- curr_rows[[col]][i]
        val_char <- if (is.na(raw_val)) "" else as.character(raw_val)

        # Certificates / AUM
        if (col %in% c("Certificates","AUM")) {
          num <- suppressWarnings(as.numeric(gsub("[,\\s]", "", val_char)))
          display <- if (!is.na(num)) formatC(num, format="f", big.mark=",", digits=0) else ""
          table_html <- paste0(table_html, "<td style='padding:5px; text-align:right; color:#04103b;'>", display, "</td>")

          # Mgmt. Fees / Perf. Fees
        } else if (col %in% c("Mgmt. Fees","Perf. Fees")) {
          if (val_char == "") { display <- ""; color <- "#04103b" } else {
            num <- if (grepl("%", val_char)) suppressWarnings(as.numeric(gsub("%","",val_char))/100) else suppressWarnings(as.numeric(val_char))
            display <- ifelse(is.na(num), "", percent(num, accuracy=0.01))
            color <- ifelse(!is.na(num) && num > 0, "green", "#04103b")
          }
          table_html <- paste0(table_html, "<td style='padding:5px; text-align:right; color:", color, ";'>", display, "</td>")

          # As of
        } else if (col == "As of") {
          if (val_char == "") { display <- "" } else {
            date_val <- suppressWarnings(as.Date(val_char))
            if (is.na(date_val)) date_val <- suppressWarnings(as.Date(val_char, "%Y-%m-%d"))
            if (is.na(date_val)) { display <- val_char } else {
              day <- format(date_val,"%d")
              mnum <- as.integer(format(date_val,"%m"))
              display <- paste0(day,"-",month_abbr_custom[mnum])
            }
          }
          table_html <- paste0(table_html, "<td style='padding:5px; text-align:center; color:#04103b;'>", display, "</td>")

          # default: left-align text
        } else {
          table_html <- paste0(table_html, "<td style='padding:5px; text-align:left; color:#04103b;'>", val_char, "</td>")
        }
      }

      table_html <- paste0(table_html,"</tr>")
    }
  }

  table_html <- paste0(table_html,"</table>")
  return(table_html)
}

# Example usage:
# names(df) <- c("Category","Strategy","ISIN","As of","Mgmt. Fees","Perf. Fees","currency","Certificates","AUM")
# meta_data_table <- generate_html_table_meta_data(df)
# cat(meta_data_table)


#---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------

generate_html_table_calendar_years_by_category <- function(df,
                                                           special_strategies = c(
                                                             "Amadeus Global Market Index Vontobel",
                                                             "Amadeus Global Market Index UBS",
                                                             "Amadeus Yield Enhancement Index GTWO",
                                                             "Amadeus Short Term Trading Index GTWO"
                                                           )
) {
  library(dplyr)
  library(lubridate)
  library(scales)
  library(htmltools)
  library(tidyr)

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

  # ---- Prep years ----
  global_last_date <- max(df$Date, na.rm = TRUE)
  years_to_show <- (year(global_last_date) - 1):(year(global_last_date) - 4)

  # ---- Expand df ----
  df_expanded <- df %>%
    mutate(Date = as.Date(Date)) %>%
    pivot_longer(
      cols = c("NAV","NAV_CHF","NAV_EUR","NAV_USD"),
      names_to = "NAV_type", values_to = "NAV_val"
    ) %>%
    filter(!( !(AccountAlias %in% special_strategies) & NAV_type != "NAV")) %>%
    filter(!( (AccountAlias %in% special_strategies) & NAV_type == "NAV")) %>%
    mutate(
      Currency = case_when(
        NAV_type == "NAV_CHF" ~ "CHF",
        NAV_type == "NAV_EUR" ~ "EUR",
        NAV_type == "NAV_USD" ~ "USD",
        NAV_type == "NAV"     ~ currency,
        TRUE ~ currency
      )
    )

  # ---- Summaries ----
  results_base <- df_expanded %>%
    group_by(Category, AccountAlias, Currency) %>%
    arrange(Date) %>%
    summarise(
      last_date = max(Date, na.rm = TRUE),
      last_nav  = last(NAV_val),
      all_dates = list(Date),
      all_nav   = list(NAV_val),
      .groups = "drop"
    )

  results <- results_base %>%
    rowwise() %>%
    mutate(
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
      `Vol 1Y` = {
        fund_last_date <- max(all_dates, na.rm = TRUE)
        one_year_ago <- fund_last_date %m-% years(1)
        nav_1y <- all_nav[all_dates >= one_year_ago & all_dates <= fund_last_date]
        dates_1y <- all_dates[all_dates >= one_year_ago & all_dates <= fund_last_date]
        span_days <- as.numeric(max(dates_1y) - min(dates_1y))
        if (length(nav_1y) < 2 || span_days < 360) "" else {
          rets <- diff(log(nav_1y))
          avg_diff_days <- mean(as.numeric(diff(sort(dates_1y)), units = "days"))
          freq <- 365 / avg_diff_days
          as.character(percent(sd(rets, na.rm = TRUE) * sqrt(freq), accuracy = 0.1))
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

  # ---- Format output ----
  results <- results %>%
    mutate(
      `As of` = format(last_date, "%d-%b"),
      NAV = comma(last_nav, accuracy = 0.01)
    ) %>%
    select(Category, Strategy = AccountAlias, Currency, `As of`, NAV, YTD,
           all_of(as.character(years_to_show)), `Vol 1Y`) %>%
    arrange(Category, Strategy, Currency)

  # ---- Adjust Category and Strategy display ----
  results <- results %>%
    group_by(Category, Strategy) %>%
    mutate(
      Category_disp = ifelse(row_number() == 1, Category, ""),
      Strategy_disp = ifelse(Strategy %in% special_strategies & row_number() > 1, "", Strategy)
    ) %>%
    ungroup()

  # ---- HTML table ----
  table_html <- "<table style='border-collapse: separate; border-spacing: 0; width: 100%; font-family: Arial, sans-serif;'>"

  # Header
  header_cols <- c("Category_disp","Strategy_disp","Currency","As of","NAV","YTD",
                   as.character(years_to_show),"Vol 1Y")
  header_names <- c("Category","Strategy","Currency","As of","NAV","YTD",
                    as.character(years_to_show),"Vol 1Y")

  table_html <- paste0(
    table_html,
    "<tr style='background-color:#04103b; color:white; text-align:center;'>",
    paste0("<th style='padding:5px;'>", header_names, "</th>", collapse=""),
    "</tr>"
  )

  # ---- Loop by Category ----
  for (cat in unique(results$Category)) {
    cat_rows <- results %>% filter(Category == cat)

    # Add vertical space above category
    table_html <- paste0(
      table_html,
      "<tr><td colspan='", length(header_cols), "' style='height:30px;'></td></tr>"
    )

    # Category header with thick dark-blue line
    table_html <- paste0(
      table_html,
      "<tr><td colspan='", length(header_cols),
      "' style='font-weight:bold; text-align:left; padding:6px; border-bottom:3px solid #04103b; color:#04103b;'>",
      cat, "</td></tr>"
    )

    # ---- Strategy rows ----
    for (i in 1:nrow(cat_rows)) {
      row <- cat_rows[i, ]

      # Detect last currency of this strategy
      is_last_for_strategy <- (
        i == nrow(cat_rows) ||
          cat_rows$Strategy[i + 1] != row$Strategy
      )
      border_style <- if (is_last_for_strategy) "border-bottom:1px solid #a7c7e7;" else ""

      table_html <- paste0(table_html, "<tr style='height:22px;'>")

      for (col in header_cols) {
        value <- row[[col]]

        # NAV special rule
        if (col == "NAV") {
          if (row$Currency != df$currency[df$AccountAlias == row$Strategy][1]) value <- ""
          table_html <- paste0(
            table_html,
            "<td style='padding:5px; text-align:right; color:#04103b;", border_style, "'>",
            value, "</td>"
          )

        } else if (grepl("%", value) & col != "Vol 1Y") {
          numeric_val <- as.numeric(gsub("%","",value))
          color <- ifelse(!is.na(numeric_val) & numeric_val > 0, "green", "black")
          display <- ifelse(is.na(numeric_val) | value == "", "", value)
          table_html <- paste0(
            table_html,
            "<td style='padding:5px; text-align:right; color:", color, ";", border_style, "'>",
            display, "</td>"
          )

        } else if (col == "Vol 1Y") {
          table_html <- paste0(
            table_html,
            "<td style='padding:5px; text-align:right; color:#04103b;", border_style, "'>",
            value, "</td>"
          )

        } else {
          table_html <- paste0(
            table_html,
            "<td style='padding:5px; text-align:left; color:#04103b;", border_style, "'>",
            value, "</td>"
          )
        }
      }
      table_html <- paste0(table_html, "</tr>")
    }
  }

  table_html <- paste0(table_html, "</table>")
  return(HTML(table_html))
}


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
generate_html_table_periods_by_category <- function(df,
                                                    special_strategies = c(
                                                      "Amadeus Global Market Index Vontobel",
                                                      "Amadeus Global Market Index UBS",
                                                      "Amadeus Yield Enhancement Index GTWO",
                                                      "Amadeus Short Term Trading Index GTWO"
                                                    )) {
  library(dplyr)
  library(lubridate)
  library(scales)
  library(htmltools)
  library(tidyr)
  library(purrr)

  # Helper to compute rolling returns
  compute_return <- function(dates, nav, years) {
    last_date <- max(dates)
    ref_date <- (last_date %m-% years(years)) - 1
    if (min(dates) > ref_date) return("")
    ref_idx <- which.min(abs(dates - ref_date))
    ref_nav <- nav[ref_idx]
    last_nav <- nav[which.max(dates)]
    if (is.na(ref_nav) || ref_nav == 0) return("")
    as.character(scales::percent(last_nav / ref_nav - 1, accuracy = 0.1))
  }

  # ---- Expand NAVs for special strategies ----
  df_expanded <- df %>%
    mutate(Date = as.Date(Date)) %>%
    pivot_longer(
      cols = c("NAV","NAV_CHF","NAV_EUR","NAV_USD"),
      names_to = "NAV_type", values_to = "NAV_val"
    ) %>%
    filter(!( !(AccountAlias %in% special_strategies) & NAV_type != "NAV")) %>%
    filter(!( (AccountAlias %in% special_strategies) & NAV_type == "NAV")) %>%
    mutate(
      Currency = case_when(
        NAV_type == "NAV_CHF" ~ "CHF",
        NAV_type == "NAV_EUR" ~ "EUR",
        NAV_type == "NAV_USD" ~ "USD",
        NAV_type == "NAV"     ~ currency,
        TRUE ~ currency
      )
    )

  # ---- Summarize ----
  results_base <- df_expanded %>%
    group_by(Category, AccountAlias, Currency) %>%
    arrange(Date) %>%
    summarise(
      last_date = max(Date),
      last_nav  = last(NAV_val),
      all_dates = list(Date),
      all_nav   = list(NAV_val),
      .groups = "drop"
    )

  global_last_date <- max(df$Date, na.rm = TRUE)

  results <- results_base %>%
    rowwise() %>%
    mutate(
      YTD = {
        start_date <- as.Date(paste0(year(global_last_date)-1, "-12-31"))
        if (min(all_dates) > start_date) "" else {
          ref_idx <- max(which(all_dates <= start_date))
          ref_nav <- all_nav[ref_idx]
          if (is.na(ref_nav) || ref_nav == 0) "" else
            as.character(scales::percent(last_nav / ref_nav - 1, accuracy = 0.1))
        }
      },
      ret_1y = compute_return(all_dates, all_nav, 1),
      ret_2y = compute_return(all_dates, all_nav, 2),
      ret_3y = compute_return(all_dates, all_nav, 3),
      ret_5y = compute_return(all_dates, all_nav, 5),
      `Vol 1Y` = {
        one_year_ago <- max(all_dates) %m-% years(1)
        nav_1y <- all_nav[all_dates >= one_year_ago & all_dates <= max(all_dates)]
        dates_1y <- all_dates[all_dates >= one_year_ago & all_dates <= max(all_dates)]
        span_days <- as.numeric(max(dates_1y) - min(dates_1y))
        if (length(nav_1y) < 2 || span_days < 360) "" else {
          rets <- diff(log(nav_1y))
          avg_diff_days <- mean(as.numeric(diff(sort(dates_1y))))
          freq <- 365 / avg_diff_days
          as.character(scales::percent(sd(rets, na.rm = TRUE) * sqrt(freq), accuracy = 0.1))
        }
      }
    ) %>%
    ungroup() %>%
    mutate(
      `As of` = format(last_date, "%d-%b"),
      NAV = scales::comma(last_nav, accuracy = 0.01),
      Strategy = AccountAlias
    ) %>%
    arrange(Category, Strategy, factor(Currency, levels = c("CHF","EUR","USD")))

  # ---- Adjust Category and Strategy display ----
  results <- results %>%
    group_by(Category, Strategy) %>%
    mutate(
      Category_disp = ifelse(row_number() == 1, Category, ""),
      Strategy_disp = ifelse(Strategy %in% special_strategies & row_number() > 1, "", Strategy)
    ) %>%
    ungroup()

  # ---- HTML table ----
  table_html <- "<table style='border-collapse: separate; border-spacing: 0; width: 100%; font-family: Arial, sans-serif;'>"
  header_cols <- c("Category_disp","Strategy_disp","Currency","As of","NAV","YTD",
                   "ret_1y","ret_2y","ret_3y","ret_5y","Vol 1Y")
  display_names <- c("Category","Strategy","Currency","As of","NAV","YTD",
                     "1Y","2Y","3Y","5Y","Vol 1Y")

  table_html <- paste0(
    table_html,
    "<tr style='background-color: #04103b; color: white; text-align: center;'>",
    paste0("<th style='padding: 5px;'>", display_names, "</th>", collapse = ""),
    "</tr>"
  )

  # ---- Loop by Category ----
  for (cat in unique(results$Category)) {
    cat_rows <- results %>% filter(Category == cat)

    # Add visible space before category (30px high)
    table_html <- paste0(
      table_html,
      "<tr><td colspan='", length(header_cols),
      "' style='height:30px;'></td></tr>"
    )

    # Category header with thick dark blue line
    table_html <- paste0(
      table_html,
      "<tr><td colspan='", length(header_cols),
      "' style='font-weight:bold; text-align:left; padding:6px; border-bottom:3px solid #04103b; color:#04103b;'>",
      cat, "</td></tr>"
    )


    # ---- Strategy rows ----
    for (i in 1:nrow(cat_rows)) {
      row <- cat_rows[i, ]

      # Add light blue line after last currency of each strategy
      is_last_for_strategy <- (
        i == nrow(cat_rows) ||
          cat_rows$Strategy[i + 1] != row$Strategy
      )
      border_style <- if (is_last_for_strategy) "border-bottom:1px solid #a7c7e7;" else ""

      table_html <- paste0(table_html, "<tr style='height:22px;'>")

      for (col in header_cols) {
        value <- row[[col]]

        if (col == "NAV") {
          if (row$Currency != df$currency[df$AccountAlias == row$Strategy][1]) value <- ""
          table_html <- paste0(
            table_html,
            "<td style='padding:5px; text-align:right; color:#04103b;", border_style, "'>",
            value, "</td>"
          )
        } else if (grepl("%", value) & col != "Vol 1Y") {
          numeric_val <- as.numeric(gsub("%","",value))
          color <- ifelse(!is.na(numeric_val) & numeric_val > 0, "green", "black")
          display <- ifelse(is.na(numeric_val) | value == "", "", value)
          table_html <- paste0(
            table_html,
            "<td style='padding:5px; text-align:right; color:", color, ";", border_style, "'>",
            display, "</td>"
          )
        } else if (col == "Vol 1Y") {
          table_html <- paste0(
            table_html,
            "<td style='padding:5px; text-align:right; color:#04103b;", border_style, "'>",
            value, "</td>"
          )
        } else {
          table_html <- paste0(
            table_html,
            "<td style='padding:5px; text-align:left; color:#04103b;", border_style, "'>",
            value, "</td>"
          )
        }
      }
      table_html <- paste0(table_html, "</tr>")
    }
  }

  table_html <- paste0(table_html, "</table>")
  return(HTML(table_html))
}



create_pdf_download_handler <- function(output_id,
                                        table_html_var,
                                        report_title = "Amadeus Products Performance by Category by Year",
                                        logo_url = "https://cdn.amadeus.ch/graphics/AMADEUS_logo_Pantone.svg") {
  downloadHandler(
    filename = function() {
      paste0(gsub(" ", "_", report_title), "_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      # Show notification
      notif <- showNotification(
        "Generating PDF... please wait",
        duration = NULL,
        type = "message"
      )
      on.exit(removeNotification(notif), add = TRUE)

      # Build the HTML
      table_html_scaled <- paste0(
        "<html><head><style>
          @page { size: A4; margin: 0.5cm; }
          body {
            margin: 0;
            padding: 0;
            font-family: Arial, sans-serif;
          }
          .logo {
            max-width: 150px;
            height: auto;
            display: block;
            margin-bottom: 1cm;
          }
          .header {
            text-align: left;
            font-size: 12pt;
            font-weight: bold;
            color: #04103b;
            margin-bottom: 10px;
          }
          .fit-page {
            transform: scale(1);
            transform-origin: top left;
            width: 100%;
            page-break-inside: avoid;
          }
          table {
            border-collapse: collapse;
            width: 100%;
            page-break-inside: avoid;
          }
          td, th {
            padding: 2px 4px;
            font-size: 8pt;
          }
        </style></head><body>",
        sprintf("<img src='%s' class='logo'>", logo_url),
        sprintf("<div class='header'>%s</div>", report_title),
        "<div class='fit-page'>",
        table_html_var,
        "</div></body></html>"
      )

      # Save temporary HTML file
      tmp_html <- tempfile(fileext = ".html")
      writeLines(table_html_scaled, tmp_html)

      # Generate PDF using Chrome
      pagedown::chrome_print(
        input = tmp_html,
        output = file,
        browser = "/usr/bin/google-chrome-stable",
        extra_args = c(
          "--no-sandbox",
          "--disable-gpu",
          "--headless",
          "--disable-dev-shm-usage",
          "--disable-software-rasterizer"
        ),
        format = "pdf"
      )
    }
  )
}
