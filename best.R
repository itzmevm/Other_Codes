 
list.files()
unzip(list.files())
 
getwd()
list.files()
 
class(outcome)


library(dplyr)
library(sqldf)
library(data.table)
options(warn=-1)
oome <- fread("outcome-of-care-measures.csv") 
 
colnames(oome)

best <- function(state, otcome)
{
  #read the outcome data
  
  val_outcome <- c("HEART ATTACK" , "HEART FAILURE", "PNEUMONIA")
  
  outcome <-fread("outcome-of-care-measures.csv") 
  colnames(outcome) <-
    gsub("-", " ", colnames(outcome), fixed = TRUE)
  colnames(outcome) <-
    gsub("   ", "_", colnames(outcome), fixed = TRUE)
  colnames(outcome) <-
    gsub("  ", "_", colnames(outcome), fixed = TRUE)
  colnames(outcome) <-
    gsub(" ", "_", colnames(outcome), fixed = TRUE)
  colnames(outcome) <-
    gsub("(", "", colnames(outcome), fixed = TRUE)
  
  colnames(outcome) <-
    gsub(")", "", colnames(outcome), fixed = TRUE)
  st <- unique(outcome$State)
  
  simpleCap <- function(x) 
  {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)),
          substring(s, 2),
          sep = "",
          collapse = " ")
  }
  
  otcome_ucase <- toupper(otcome)
  otcome_case <- gsub(" ", "_", simpleCap(otcome))
  
  ## Check that state and outcome are valid
  #state_data <- subset(outcome,State=state)
  
  #outcome_data <- eval(parse(text="suppressWarnings(as.numeric(state_data[,%s]",fld)) 
  
  if (any(val_outcome ==  otcome_ucase))
  {
    if (any(st == state) &&
        grepl('^[A-Z]+$', state) && nchar(state) == 2)
    {
      fld <-
        paste("Hospital_30_Day_Death_Mortality_Rates_from_",
              otcome_case,
              sep = '')
      
      #print(sprintf("outcome$%s", fld))
      eval(parse(text=sprintf("outcome[, %s := as.numeric(as.character(%s))]",fld,fld)))
      #outcome[mapply(is.infinite,outcome)] <- NA 
      eval(parse(text=sprintf("outcome[is.infinite(%s) | is.nan (%s)] <- NA" ,fld,fld)))
       
      hosp_out <-  eval(parse(text= sprintf("outcome[State==state,list(State,Hospital_Name,%s,Min=min(%s,na.rm=TRUE)) , by = .(State)]",fld,fld) ))
      eval(parse(text=sprintf("setkey(hosp_out,Hospital_Name,%s)",fld)))
      hsp_out <- eval(parse(text=sprintf("hosp_out[%s==Min,list(Hospital_Name)]",fld,fld)))
       ## Return hospital name in that state with lowest 30-day death
      print(hsp_out)
    }  else
    {
      stop("Invalid State!! please pass valid statename")
    }
  }  
   else
  {
    st <- NULL
    stop("Invalid outcome !! please pass valid outcome")
  }
} 
best("TX", "heart attack")
best("CA", "pneumonia")

best("TX", "heart failure")
best("MD", "pneumonia")
best("MD", "heart attack")

best("BB", "heart attack")
best("NY", "hert attack")

