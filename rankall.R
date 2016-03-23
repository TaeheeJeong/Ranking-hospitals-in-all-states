rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <-read.csv("outcome-of-care-measures.csv",colClasses="character")
        
        
        ## Check that state and outcome are valid
        if(outcome =="heart attack") {colnum<-11}
        else if(outcome =="heart failure") {colnum<-17}
        else if(outcome =="pneumonia") {colnum<-23}
        else stop("invalid outcome")
        
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        

        
        state<-sort(unique(data$State))
        hospital<-list()

        for (i in seq_along(state)){ 
                data_state<-data[data$State==state[i],]
                data_state[,colnum]<-as.numeric(data_state[,colnum])
                filtered <- complete.cases(data_state[,colnum])
                data_clean <- data_state[filtered,]
                data_sorted<-data_clean[order(data_clean[,colnum],data_clean$Hospital.Name),]
                
                if(num=="best"){
                        num<-1
                        hospital<-append(hospital, data_sorted$Hospital.Name[num])
                        }
                else if(num=="worst"){
                        num<-nrow(data_sorted)
                        hospital<-append(hospital, data_sorted$Hospital.Name[num])}
                else if(num<nrow(data_sorted)){
                        hospital<-append(hospital, data_sorted$Hospital.Name[num])}
                else if(num>nrow(data_sorted)){
                        hospital<-append(hospital, NA)}

                
        }
        ranked_hopital<-cbind(hospital,state)
        #colnames(ranked_hopital)<-c("hospital","state")
        return(ranked_hopital)
        
        
        
        
        
}