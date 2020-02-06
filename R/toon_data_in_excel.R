library(data.table)

toon_data_in_excel <- function (df) {
  tempFilePath <- paste(tempfile(), ".csv")
  tempPath <- dirname(tempFilePath)
  preferredFile <- paste(deparse(substitute(df)), ".csv", sep = "")
  preferredFilePath <- file.path(tempPath, preferredFile)
  
  if(length(dim(df)) > 2){
    stop('Too many dimensions')
  }
  
  if(is.null(dim(df))){
    df <- as.data.frame(df)
  }
  if (is.null(rownames(df))) {
    tmp <- 1:nrow(df)
  }else {
    tmp <- rownames(df)
  }
  rownames(df) <- NULL
  df <- data.frame(RowLabels = tmp, df)
  WriteAttempt <- try(
    #    write.table(df, file=preferredFilePath, quote=TRUE, sep=";", na="",
    #                row.names=FALSE, qmethod="double"),
    #    silent = TRUE)
    data.table::fwrite(df, file=preferredFilePath, append = FALSE, quote = TRUE,
           sep = ";", eol = "\r\n",
           na = "", dec = ",", row.names = FALSE, col.names = TRUE,
           dateTimeAs = "squash",
           buffMB = 8L),
    silent = TRUE)
  
  if ("try-error" %in% class(WriteAttempt)) {
    #    write.table(df, file=tempFilePath, quote=TRUE, sep=";", na="",
    #                row.names=FALSE, qmethod="double")
    data.table::fwrite(df, file=tempFilePath, append = FALSE, quote = TRUE,
           sep = ";", eol = "\r\n",
           na = "", dec = ",", row.names = FALSE, col.names = TRUE,
           dateTimeAs = "squash",
           buffMB = 8L)
    shell.exec(tempFilePath)
  } else {
    shell.exec(preferredFilePath)
  }
}