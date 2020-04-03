
validateParameterPositive <- function( x, x.str ){
  validate( need( !is.null(x) && is.numeric(x) && x>=0, 
                  paste0( "Need to define a positive \"", x.str, "\"!" ) ) )
}

# Not zero
validateParameterStrictPositive <- function( x, x.str ){
  validate( need( !is.null(x) && is.numeric(x) && x>0, 
                  paste0( "Need to define a non-zero positive \"", x.str, "\"!" ) ) )
}

getParamDefaultValues <- function(){
  data.frame( 
    # Trial params
    "numPatients" = 800,
    "studyDuration" = 24,
    "recDuration" = 12,
    "k" = 2,
    "r" = 1, 
    
    # Event params
    "numChangePoints" = 0,
    "HR" =  0.6,
    "ctrlMedian" = 3,
   
    "chP0" = 5, 
    "ctrlM0" = 3,
    "HR0" = 1, 
    "ctrlM1" = 5,
    "HR1" = 0.3,
    
    # Prediction params
    "eventOrTime"= "Predict events|time",
    "timePred"="10,15",
    "eventPred" = "200,400",
    
    # Start date
    "startDate" = Sys.Date()
)}
