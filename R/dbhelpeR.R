

fastwRiter <- function(conn, name, value, overwrite=FALSE,append=T, row.names=FALSE) {
  start <- Sys.time()

  if (overwrite==T | append==F) {
    RMariaDB::dbWriteTable(conn, name, head(value,100), overwrite=TRUE, row.names=row.names)
    RMariaDB::dbExecute(conn, paste0("TRUNCATE TABLE ", name))  
  }else{
    
    cls<-dbGetQuery(pool, paste0("select * from ",name, " limit 1"))
    setDT(value)
    db_cols <- names(cls)
    missing_cols <- setdiff(db_cols, names(value))
    
    # add missing columns as NA
    if (length(missing_cols) > 0) {
      value[, (missing_cols) := NA]
    }

    value<-as.data.frame(value)[,names(cls)]
  }
  

  # 3. write file safely (NO quotes, controlled format)
  write.table(
    value,
    "tmp.csv",
    sep = ",",
    row.names = row.names,
    col.names = TRUE,
    quote = FALSE,
    na = "\\N"
  )
  
  # 4. load into MySQL
  DBI::dbExecute(conn, paste0("
  LOAD DATA LOCAL INFILE 'tmp.csv'
  INTO TABLE ", name, "
  FIELDS TERMINATED BY ',' 
  LINES TERMINATED BY '\n'
  IGNORE 1 LINES
  "))
  
  Sys.time() - start
}


chunkwRiter<-function(pool,name,value,overwrite=F,append=T)
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
