fastwRiter <- function(conn, name, value, overwrite=FALSE, append=TRUE, row.names=FALSE) {
  start <- Sys.time()
  
  if (overwrite == TRUE | append == FALSE) {
    RMariaDB::dbWriteTable(conn, name, head(value, 100), overwrite=TRUE, row.names=row.names)
    RMariaDB::dbExecute(conn, paste0("TRUNCATE TABLE ", name))
  } else {
    cls <- dbGetQuery(conn, paste0("SELECT * FROM ", name, " LIMIT 1"))  # use conn not pool
    setDT(value)
    db_cols <- names(cls)
    missing_cols <- setdiff(db_cols, names(value))
    if (length(missing_cols) > 0) value[, (missing_cols) := NA]
    value <- as.data.frame(value)[, db_cols]
  }
  
  # Flush to avoid stale metadata cache
  DBI::dbExecute(conn, "FLUSH TABLES")
  
  write.table(value, "tmp.csv", sep="\t", row.names=row.names,
              col.names=TRUE, quote=FALSE, na="\\N")
  
  col_list <- paste(names(value), collapse=", ")
  
  DBI::dbExecute(conn, paste0("
    LOAD DATA LOCAL INFILE 'tmp.csv'
    INTO TABLE ", name, "
    FIELDS TERMINATED BY '\t'
    LINES TERMINATED BY '\n'
    IGNORE 1 LINES
    (", col_list, ")
  "))
  print(DBI::dbGetQuery(conn, "SHOW WARNINGS"))
  Sys.time() - start
}


chunkwRiter<-function(pool,name,value,overwrite=F,append=T,row.names=F)
{
  library(pool)
  start<-Sys.time()

  if(overwrite==T)
  {
    RMariaDB::dbWriteTable(pool,name,head(value,0),overwrite=T,row.names=row.names)
  } else {
    # Sync schema: add any columns present in value but missing from the table
    db_cols <- tryCatch(
      dbGetQuery(pool, paste0("SHOW COLUMNS FROM `", name, "`"))$Field,
      error = function(e) character(0)
    )
    if (length(db_cols) > 0) {
      missing_cols <- setdiff(names(value), db_cols)
      for (col in missing_cols) {
        col_type <- switch(class(value[[col]])[1],
          "numeric"   = "DOUBLE",
          "integer"   = "INT",
          "Date"      = "DATE",
          "logical"   = "TINYINT(1)",
          "TEXT"
        )
        dbExecute(pool, paste0("ALTER TABLE `", name, "` ADD COLUMN `", col, "` ", col_type))
        message("Added missing column '", col, "' (", col_type, ") to table '", name, "'")
      }
    }
  }

  conn<-pool
  try(conn <- poolCheckout(pool),silent=T)

  # Use explicit column list so positional mismatches never cause INSERT failures
  col_names <- paste0("`", names(value), "`", collapse=",")

  chunk_size <- 10000
  chunks <- split(value, (seq_len(nrow(value)) - 1) %/% chunk_size)

  dbBegin(conn)
  if(overwrite==F & append==F)
  {
    dbExecute(conn, paste0("TRUNCATE TABLE `", name, "`"))
  }

  for (chunk in chunks) {
    values_sql <- paste(
      apply(chunk, 1, function(row) {
        paste0("(", paste(DBI::dbQuoteLiteral(conn, row), collapse=","), ")")
      }),
      collapse=","
    )
    sql <- paste0("INSERT INTO `", name, "` (", col_names, ") VALUES ", values_sql)
    dbExecute(conn, sql)
  }

  dbCommit(conn)
  try(poolReturn(conn),silent=T)

  end<-Sys.time()
  end-start
}
