 
list.files()
unzip(list.files())
 
getwd()
list.files()
 
class(outcome)

 
library(sqldf)
library(data.table)

##Subsetting Waring 

options(warn=-1)

# Function : Write a function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking
# (num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
# containing the hospital in each state that has the ranking specified in num. 
# For example the function call
# rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
# are the best in their respective states for 30-day heart attack death rates. 
# The function should return a value for every state (some may be NA). The first column in the data frame # is named hospital, which contains
# the hospital name, and the second column is named state, which contains the 2-character abbreviation for
# the state name. Hospitals that do not have data on a particular outcome should be excluded from the set 
# of hospitals when deciding the rankings.

rankall <- function(otcome,rank=1)
{
 
  
  # Predefining accetable outcomes :
  
  val_outcome <- c("HEART ATTACK" , "HEART FAILURE", "PNEUMONIA")
  
  # Reading csv file(outcome data) to data table  :
  
  outcome <-fread("outcome-of-care-measures.csv") 
  
  # Replacing spaces and brackets in the colnames of the data table with underscore :
  
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
 
  # Function to fix the upper/lower case for outcome value to be passed on to the col name 
  
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
  
  # Checkin for validity of the outcome passed through arg and proceed with the process
 
  if (any(val_outcome ==  otcome_ucase))
  {
  
  # Establishing the field name based on the outcome value passed thru arg 
    
      fld <-
        paste("Hospital_30_Day_Death_Mortality_Rates_from_",
              otcome_case,
              sep = '')
      
  # Converting the field to numeric from character : 
      
      eval(parse(text=sprintf("outcome[, %s := as.numeric(as.character(%s))]",fld,fld)))
  
  # The field has values such as "Not Available" which during conversion to numeric becomes "Inf". 
  # Inf and nan values are being cleaned up here  : 
      
      eval(parse(text=sprintf("outcome[is.infinite(%s) | is.nan (%s)] <- NA" ,fld,fld)))
      
  # Sorting the data table by State, Mortality field for the respective outcome and Hospital name 
      
      outcome<- eval(parse(text=sprintf("outcome[order(State,%s,Hospital_Name)]",fld)))
    
  # Creating Index/Ranks within each State 
      
      hosp_out <-  eval(parse(text= sprintf("outcome[,.(Hospital_Name,%s,Rank=seq_len(.N),Total=.N ,Max= max(%s,na.rm=TRUE) ),by =.(State)]",fld,fld,fld ) ))
      
  # Extracting the unique State names     
       
      states <- data.table(State = unique(outcome$State))
      states_hosp <- data.table(State = unique(outcome$State) )
   
      max_val <- max(hosp_out$Total,na.rm=TRUE)
      
   
 
     
  # Checking if the Rank passed is numeric    
      if (is.numeric(rank))
      {
        
        # If the rank is numeric and if the value is greater than maximum value in the table 
        # then "NA" is returned 
        
        if (rank > max_val)
        {
          hsp_out <- data.table(Hospital_Name = "NA", State ="NA")
        }
        
        # Else all the records with the respective rank(passed through arg) are extracted 
        
        else 
        {
          hsp_out <-   hosp_out[(Rank==rank)] 
         # print(head(states_hosp))
        }
      }
     
    # Processing non numeric rank value passed 
      
     else 
      {
        # if "best" then all records with Rank 1 are pulled 
        
        if (rank =="best") 
        {
        hsp_out <-   hosp_out[(Rank==1)] 
        } 
        
        # if "worst" then all records with maximum non NA Ranks are pulled 
        
        else if (rank=="worst")
        {
         
          hsp_out <-  eval(parse(text=sprintf("hosp_out[(%s==Max)]",fld)))
        }
      }   
     
     # Return hospital name in that state with lowest 30-day death
    
      out1 <- hsp_out[,.(State,Hospital_Name )]  
      
      # Merging with the State table to pull all the states that are not extracted into hsp_out 
      
      out <- merge(states_hosp,out1,all.x=TRUE) 
   
      #Sorting output by State and Hospital Name 
      
      out <- out[order(State,Hospital_Name)]
      
      out 
   }  
   else
  {
    st <- NULL
    stop("Invalid outcome !! please pass valid outcome")
  }
} 


head(rankall("heart attack", 20), 10)

# State                       Hospital_Name
# 1:    AK                                <NA>
#   2:    AL      D W MCMILLAN MEMORIAL HOSPITAL
# 3:    AR   ARKANSAS METHODIST MEDICAL CENTER
# 4:    AZ JOHN C LINCOLN DEER VALLEY HOSPITAL
# 5:    CA               SHERMAN OAKS HOSPITAL
# 6:    CO            SKY RIDGE MEDICAL CENTER
# 7:    CT             MIDSTATE MEDICAL CENTER
# 8:    DC                                <NA>
#   9:    DE                                <NA>
#   10:    FL      SOUTH FLORIDA BAPTIST HOSPITAL

tail(rankall("pneumonia", "worst"), 3)
# State                              Hospital_Name
# 1:    WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC
# 2:    WV                     PLATEAU MEDICAL CENTER
# 3:    WY           NORTH BIG HORN HOSPITAL DISTRICT

tail(rankall("heart failure"), 10)
# State                                                     Hospital_Name
# 1:    TN                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL
# 2:    TX                                        FORT DUNCAN MEDICAL CENTER
# 3:    UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER
# 4:    VA                                          SENTARA POTOMAC HOSPITAL
# 5:    VI                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR
# 6:    VT                                              SPRINGFIELD HOSPITAL
# 7:    WA                                         HARBORVIEW MEDICAL CENTER
# 8:    WI                                    AURORA ST LUKES MEDICAL CENTER
# 9:    WV                                         FAIRMONT GENERAL HOSPITAL
# 10:    WY                                        CHEYENNE VA MEDICAL CENTER

 