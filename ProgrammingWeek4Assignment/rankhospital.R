rankhospital <- function(state, outcome, num = "best") {

    #####################################
    ## Read outcome data
    outcomeFile <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state is valid
    c <- outcomeFile["State"] == state
    
    sumState <- sum(c) ## if at least one TRUE value, then its valid
    if ( sumState > 0 ){
        outcomeFile <- outcomeFile[outcomeFile["State"]==state,]
    }
    else{
        stop("invalid state")
    }
    
    
    #####################################
    ## Check that outcome is valid
    
    ## obtain vector with valid columns for given outcome
    outcomeIn <-gsub("[[:space:]]", ".", outcome)
    c <- grepl(outcomeIn,names(outcomeFile),ignore.case = TRUE)
    
    ## obtain only column for 30 Day Mortality values
    outcomeColumn <- names(outcomeFile[,c])
    outcomeColumn <- outcomeColumn[grepl("^Hospital.30.Day.Death.",outcomeColumn)]
    
    
    sumOutcome <- sum(c) ## if at least one TRUE value, then its valid
    if ( sumOutcome == 0 ){
        stop("invalid outcome")
    }
    
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    ## get data frame with Hospital, State and 30 Day Death Rate Mortality
    outcomeFile<-outcomeFile[,cbind("Hospital.Name",outcomeColumn)]
    
    ## remove invalid values
    
    outcomeFile<-outcomeFile[outcomeFile[outcomeColumn]!="Not Available",]

    
    if (num == "best"){

        #return Hospital Name - column 1 - with minimum value of Mortality
        
        outcomeFile<-outcomeFile[order(as.numeric(outcomeFile[,outcomeColumn]),outcomeFile[,"Hospital.Name"],decreasing=FALSE),]
        outcomeFile[1,1]
        
        #This commented line outputs the same as above
        #outcomeFile[which.min(as.numeric(outcomeFile[,outcomeColumn])),1]
        
    }
    else if(num == "worst"){
        #return Hospital Name - column 1 - with maximum value of Mortality
        
        outcomeFile<-outcomeFile[order(as.numeric(outcomeFile[,outcomeColumn]),outcomeFile[,"Hospital.Name"],decreasing=TRUE),]
        outcomeFile[1,1]
        
        #This commented line outputs the same as above
        #outcomeFile[which.max(as.numeric(outcomeFile[,outcomeColumn])),1]
    }
    else{
        
        #####################################
        ## Check if number of Hospitals is bigger than specified rank
         size<-length(outcomeFile[,1])

        if (size < num){
         stop("NA")
        }

        #return Hospital Name - column 1 - for specified rank
        outcomeFile<-outcomeFile[order(as.numeric(outcomeFile[,outcomeColumn]),outcomeFile[,"Hospital.Name"],decreasing=FALSE),]
        outcomeFile[num,1]
    }
    
}