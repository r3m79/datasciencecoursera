corr <- function(directory, threshold = 0){
    corr_vec <- c()
    for(i in 1:332){
        file_no_ext <- formatC(i, width = 3, format = "d", flag = "0")
        filename <- paste(file.path(directory,file_no_ext),".csv",sep="")
        file_data <- read.csv(filename,header=TRUE,sep=",")
        noNA <- complete.cases(file_data)
        goodDataSul <- file_data[noNA,][,"sulfate"]
        goodDataNit <- file_data[noNA,][,"nitrate"]
        #print(goodDataSul)
        #print(goodDataNit)
        if ( length(goodDataSul) > threshold &  length(goodDataNit) > threshold){
            corr_vec <- c(corr_vec,cor(goodDataSul,goodDataNit)) 
            #print(corr_vec)
        }
    }
    corr_vec
}