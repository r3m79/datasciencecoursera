pollutantmean <- function(directory, pollutant, id = 1:332){
    mean_per_file <- c()
    goodSum <- c()
	for(i in id){
		file_no_ext <- formatC(i, width = 3, format = "d", flag = "0")
		filename <- paste(file.path(directory,file_no_ext),".csv",sep="")
		file_data <- read.csv(filename,header=TRUE,sep=",")
		goodSum <- c(goodSum,length(file_data[!is.na(file_data[,pollutant]),
		                                      pollutant]))
		print(i)
		print(goodSum)
		mean_per_file <-  c(mean_per_file,mean(file_data[[pollutant]],
		                                       na.rm = TRUE))
		print(mean_per_file)
	}
    #unweighted mean
    #result <- mean(mean_per_file)
    #weighted mean
    result <- sum((goodSum * mean_per_file)) / sum(goodSum)
    print(result)
}