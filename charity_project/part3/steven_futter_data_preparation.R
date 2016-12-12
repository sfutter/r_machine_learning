########################################
## PREDICT 422
## Charity Project - Part 3 (The Mailing List Problem)
########################################

# Separate RFA Values (R = recency, F = frequency, A = amount) using substring function.
separateRFA = function(df,colName){
  newColName1     = paste('RECENCY_',substr(colName,5,6),sep='')
  newColName2     = paste('FREQUENCY_',substr(colName,5,6), sep='')
  newColName3     = paste('AMOUNT_', substr(colName,5,6), sep='')
  
  df[,newColName1]  = sapply(df[,colName],substring,1,1)
  df[,newColName2]  = sapply(df[,colName],substring,2,2) 
  df[,newColName3]  = sapply(df[,colName],substring,3,3)
  return(df)
}

# Function to perform Part 1 data processing steps on a data frame.
processPart1 = function(myData)
{
  ## Part A - Resolve Missing Values - no steps here. We resolve by removing HINC. 
  
  # convert HOME to factor - this may be duplicate effort but leaving as is for now.
  myData$HOME = as.factor(myData$HOME)
  
  # # GENDER - Assign A, J, and NA to category U   --> this step would help avoid unnecessary NAs in output.
  # idxMF = myData$GENDER %in% c("M","F")
  # myData$GENDER[!idxMF] = "U"
  # myData$GENDER = factor(myData$GENDER)
  # table(myData$GENDER)
  
  ## Part B - Derived or Transformed Variables
  myData$LOG_MAXRAMNT = log(myData$MAXRAMNT)			
  myData$LOG_RAMNTALL = log(myData$RAMNTALL)			

  ## Part C - Re-categorize Variables: apply separateRFA to the variables RFA_96 and RFA_97
  myData = separateRFA(myData,'RFA_96')
  myData = separateRFA(myData,'RFA_97')
  
  ## Part D - Drop Variables
  # - Remove DONR since it only has one level in the regression problem. DONR is not
  # meant to be used for the regression problem anyway.
  # - Remove RFA_96 and RFA_97 in favor or keeping the separate R, F, and A variables.
  # - Remove RECENCY_97 as this self-created variable only has one level. No information is added 
  # and it may cause problems with the code.
  # - Remove HINC as this variable suffers from a large amount of missing values.
  
  dropIdx = which(names(myData) %in% c("DONR","RFA_96","RFA_97","HINC","RECENCY_97"))
  
  # Drop the variables indicated by dropIdx.
  myData2 = myData[,-dropIdx]
  names(myData2)   # check that the result is as expected
  
  return(myData2)
}




# Function to perform Part 2 data processing steps on a data frame.

fun <- function(x){
  quantiles <- quantile( x, c(.01, .99 ) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  x
}

processPart2 = function(myData)
{
  ## Part A - Resolve Missing Values - nothing here currently. Resolve by removing HINC and GENDER. See
  #  step D below.
  
  ## Part B - Derived or Transformed Variables
  # HOME was fitted as a factor so converting it to factor here too
  myData$HOME = as.factor(myData$HOME)
  
  # create new variables with _TRIM appended at end. Cut off extreme vals for 1-99%-ile.
  myData$MEDPPH_TRIM    = fun( myData$MEDPPH )
  myData$MEDHVAL_TRIM   = fun( myData$MEDHVAL )
  myData$MEDEDUC_TRIM   = fun( myData$MEDEDUC )
  myData$MEDAGE_TRIM    = fun( myData$MEDAGE )
  myData$MEDINC_TRIM    = fun( myData$MEDINC )
  myData$NUMPROM_TRIM   = fun( myData$NUMPROM )
  myData$NUMPRM12_TRIM  = fun( myData$NUMPRM12 )
  myData$MAXRAMNT_TRIM  = fun( myData$MAXRAMNT )
  myData$RAMNTALL_TRIM  = fun( myData$RAMNTALL )
  myData$LASTGIFT_TRIM  = fun( myData$LASTGIFT )
  myData$NGIFTALL_TRIM  = fun( myData$NGIFTALL )
  myData$TDON_TRIM      = fun( myData$TDON )
  
  ## Part C - Re-categorize Variables
  # Apply separateRFA to the variables RFA_96 and RFA_97
  myData = separateRFA(myData,"RFA_96")
  myData = separateRFA(myData,"RFA_97") 
  
  ## Part D - Drop Variables
  dropIdx = which(names(myData) %in% c("ID","DAMT","RFA_96","RFA_97", "AGE", "HINC", "GENDER"))
  
  # Drop the variables indicated by dropIdx.
  myData2 = myData[,-dropIdx]
  names(myData2)   # check that the result is as expected
  
  return(myData2)
}