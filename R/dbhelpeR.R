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
  start<-Sys.time()
  
  if(overwrite==T | append==F)
  {
    dbWriteTable(pool,name,head(value,0),overwrite=T,row.names= row.names)
  }
  
  conn <- poolCheckout(pool)
  
  chunk_size <- 10000
  chunks <- split(value, (seq_len(nrow(value)) - 1) %/% chunk_size)
  
  dbBegin(conn)
  
  for (chunk in chunks) {
    values_sql <- paste(
      apply(chunk, 1, function(row) {
        paste0("(", paste(DBI::dbQuoteLiteral(conn, row), collapse = ","), ")")
      }),
      collapse = ","
    )
    
    sql <- paste0(
      paste0("INSERT INTO ",name," VALUES "),
      values_sql
    )
    
    dbExecute(conn, sql)
  }
  
  dbCommit(conn)
  poolReturn(conn)
  
  end<-Sys.time()
  end-start  
}
