fastwRiter<-function(conn,name,value,append=T,overwrite=F,row.names=F)
{    
  start<-Sys.time()
  if(overwrite==T | append==F)
  {
    dbWriteTable(pool,name,head(value,0),overwrite=T,row.names= row.names)
  }
  write.csv(value, "tmp.csv", row.names = row.names)
  
  DBI::dbExecute(conn, paste0("
    LOAD DATA LOCAL INFILE 'tmp.csv'
    INTO TABLE ",name,"
    FIELDS TERMINATED BY ','
    IGNORE 1 LINES
    "))
  end<-Sys.time()
  end-start 
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
