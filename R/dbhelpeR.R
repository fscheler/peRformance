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