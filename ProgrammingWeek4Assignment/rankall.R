rankall <- function(outcome, num = "best") {
    #####################################
    ## Read outcome data
    outcomeFile <- read.csv("outcome-of-care-measures.csv", 
                            colClasses = "character")
    
    #####################################
    ## Check that outcome is valid
    
    ## obtain vector with valid columns for given outcome
    outcomeIn <-gsub("[[:space:]]", ".", outcome)
    c <- grepl(outcomeIn,names(outcomeFile),ignore.case = TRUE)
    
    ## obtain only column for 30 Day Mortality values
    outcomeColumn <- names(outcomeFile[,c])
    outcomeColumn <- outcomeColumn[grepl("^Hospital.30.Day.Death.",
                                         outcomeColumn)]
    
    
    sumOutcome <- sum(c) ## if at least one TRUE value, then its valid
    if ( sumOutcome == 0 ){
        stop("invalid outcome")
    }
    
    ## get data frame with Hospital, State and 30 Day Death Rate Mortality
    #outcomeFile<-outcomeFile[,cbind("Hospital.Name",outcomeColumn)]
    outcomeFile<-outcomeFile[,cbind("Hospital.Name","State",outcomeColumn)]
    
    ## remove invalid values
    
    outcomeFile<-outcomeFile[outcomeFile[outcomeColumn]!="Not Available",]
    
    outcomeFile<-outcomeFile[order(outcomeFile[,"State"],as.numeric(
        outcomeFile[,outcomeColumn]),outcomeFile[,"Hospital.Name"],
        decreasing=FALSE),]
    
   
    ## For each state, find the hospital of the given rank
    
    states<-unique(outcomeFile["State"])
    
    names(outcomeFile)<-c("hospital","state",outcomeColumn)

    outList<-vector()
    for (i in 1:length(states[,1])){
        outcomeFile2<-outcomeFile[outcomeFile["state"]==states[i,1],]
        
        if (num == "best"){

            #return Hospital Name - column 1 - with minimum value of Mortality
            # outList<-rbind(outList,outcomeFile2[which.min(
            #     as.numeric(outcomeFile2[,outcomeColumn])),])
            outcomeFile2<-outcomeFile2[order(as.numeric(outcomeFile2[,
                                    outcomeColumn]),outcomeFile2[,"hospital"],
                                             decreasing=FALSE),]
            outList<-rbind(outList,c(outcomeFile2[1,]))
        }
        else if(num == "worst"){
            #return Hospital Name - column 1 - with maximum value of Mortality
            outcomeFile2<-outcomeFile2[order(as.numeric(outcomeFile2[,
                            outcomeColumn]),outcomeFile2[,"hospital"],
                                             decreasing=TRUE),]
            outList<-rbind(outList,c(outcomeFile2[1,]))
        }
        else{
            
            #####################################
            ## Check if number of Hospitals is bigger than specified rank
            size<-length(outcomeFile2[,1])
            
            #return Hospital Name - column 1 - for specified rank
            #if num greater than hospitals in the state must return NA
            if (size < num){
                outList<-rbind(outList,c("NA",states[i,1],"00"))
            }
            else{
                outcomeFile2<-outcomeFile2[order(as.numeric(outcomeFile2[,
                                        outcomeColumn]),outcomeFile2[,"hospital"],
                                        decreasing=FALSE),]
                outList<-rbind(outList,c(outcomeFile2[num,]))
            }
        }
        

    }

    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    outList<-outList[,c("hospital","state")]
    #outList
    
    
}