complete <- function(directory, id = 1:332){
    data_out <- c()
    n_row <- c()
    n_col_total <- c()
    for(i in id){
        file_no_ext <- formatC(i, width = 3, format = "d", flag = "0")
        filename <- paste(file.path(directory,file_no_ext),".csv",sep="")
        file_data <- read.csv(filename,header=TRUE,sep=",")
        n_row <- c(n_row,i)
        n_col_1 <- length(file_data[!is.na(file_data[,"sulfate"]),"sulfate"])
        n_col_total <- c(n_col_total, n_col_1)
    }
    result <- data.frame(id = n_row, nobs = n_col_total)
    result
}