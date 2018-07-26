 
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

rankhospital <- function(state, otcome,rank)
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
  
  rank_cs <- c("worst", "best")
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
 
  if (any(val_outcome ==  otcome_ucase))
  {
    if (any(st == state) &&
        grepl('^[A-Z]+$', state) && nchar(state) == 2)
    {
      fld <-
        paste("Hospital_30_Day_Death_Mortality_Rates_from_",
              otcome_case,
              sep = '')
      
      eval(parse(text=sprintf("outcome[, %s := as.numeric(as.character(%s))]",fld,fld)))
      
      eval(parse(text=sprintf("outcome[is.infinite(%s) | is.nan (%s)] <- NA" ,fld,fld)))
      
      outcome<- eval(parse(text=sprintf("outcome[order(State ,%s,Hospital_Name)]",fld)))
 
      hosp_out <-  eval(parse(text= sprintf("outcome[(!is.na(%s) & State==state),.(Hospital_Name,%s,Rank=seq_len(.N),Total=.N ),by =.(State)]",fld,fld ) ))
 
      max_val <- unique(hosp_out$Total)
   
      if (is.numeric(rank))
      {
        
        
        if (rank > max_val)
        {
          hsp_out <- data.table(Hospital_Name = "NA")
        }
        else 
        {
          hsp_out <- hosp_out[(Rank==rank)]
        }
      }
      else 
      {
        if (rank =="best") 
        {
        hsp_out <- hosp_out[(Rank==1)]
         
        } 
        else if (rank=="worst")
        {
          hsp_out <- hosp_out[(.N)]
        }
      }                  
      ## Return hospital name in that state with lowest 30-day death

      out <- hsp_out$Hospital_Name
      out  
  }  
   else
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









rankhospital("TX", "heart failure",4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)

rankhospital("MD", "heart failure")
rankhospital("MD", "pneumonia",5)
rankhospital("MD", "heart attack",worst)

rankhospital("BB", "heart attack")
rankhospital("NY", "hert attack")

