
validateParameterPositive <- function( x, x.str ){
  validate( need( !is.null(x) && is.numeric(x) && x>=0, 
                  paste0( "Need to define a positive \"", x.str, "\"!" ) ) )
}

# Not zero
validateParameterStrictPositive <- function( x, x.str ){
  validate( need( !is.null(x) && is.numeric(x) && x>0, 
                  paste0( "Need to define a non-zero positive \"", x.str, "\"!" ) ) )
}

validateParameterBtw0and1 <- function( x, x.str ){
  validate( need( !is.null(x) && is.numeric(x) && x>0 && x<1, 
                  paste0( "Need to define \"", x.str, "\" within (0,1)!" ) ) )
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
    "shape" = 1,
   
    "chP0" = 5, 
    "ctrlM0" = 3,
    "HR0" = 0.9999, 
    "ctrlM1" = 5,
    "HR1" = 0.3,
    "shape1" = 1,
    
    # Prediction params
    "eventOrTime"= "Predict_events|time",
    "timePred"="10,15",
    "eventPred" = "200,400",
    
    # Start date
    "startDate" = Sys.Date(),
    
    # Dropouts
    "useDropouts" = "No",
    "dropoutPropCtrl" = 0.05,
    "dropoutPropExp" = 0.01,
    
    # Test
    "alpha" = 0.05,
    "power" = 0.8,
    "twoSided" = "Yes"
  )}

validateParameters <- function( input, vals ) {
  validateParameterStrictPositive( input$numPatients, "Number of patients" )
  validate( need( vals[[ "numChangePoints" ]], "Loading.." ) )
  validateParameterStrictPositive( vals[[ "HR" ]], "HR")
  validate( need(vals[[ "HR" ]] < 1, "HR needs to be less than 1"))
  validateParameterStrictPositive( vals[[ "k" ]], "Recruitment shape parameter")
  validateParameterStrictPositive( vals[[ "r" ]], "Randomization balance" ) 
  
  validateParameterStrictPositive( vals[[ "studyDuration" ]], "Study duration" )
  validateParameterStrictPositive( vals[[ "recDuration" ]], "Recruitment duration" )
  validateParameterStrictPositive( vals[[ "ctrlMedian" ]], "Control median survival" )
  validateParameterStrictPositive( vals[[ "shape" ]], "Weibull shape" )
  
  validate( need( vals[[ "studyDuration" ]] > vals[[ "recDuration" ]], 
                  "Study period needs to be longer than recruitment period" ) )
  validate( need( vals[[ "startDate" ]], "Need to provide a study start date"))
  validate( need( !is.na( anytime( vals[[ "startDate" ]] ) ), "Need to provide start date in proper format!"))
  validate( need( vals[[ "eventOrTime" ]], "Need prediction parameters" ) )
  if( vals[[ "eventOrTime" ]] == "Predict_events|time"  ){
    validate( need( !is.null( vals[[ "timePred" ]]), " Loading ... " ))  
    xvals <- getMultipleNumeric( vals[[ "timePred" ]], "Predict_time", vals[[ "studyDuration" ]] ) 
    validate( need( vals[[ "studyDuration" ]] >= max( xvals ), "Prediction time point exceeds study duration" ) )
  } else {
    validate( need( !is.null( vals[[ "eventPred" ]]) && !is.null( vals[[ "numPatients" ]] ), " Loading ... " ) )
    vals[[ "eventPred" ]] <- as.character( vals[[ "eventPred" ]] )
    xvals <- getMultipleNumeric( vals[[ "eventPred" ]], "Number of predicted events", vals[[ "numPatients" ]] ) 
    validate( need( vals[[ "numPatients" ]] >= max( xvals ), "Too few patients compared to predicted number of events" ) )
  }
  
  if( vals[[ "numChangePoints" ]] > 0 ){
    validateParameterStrictPositive( vals[[ "chP0" ]], "Change point (T)" )
    validateParameterStrictPositive( vals[[ "HR0" ]], "HR0")
    validateParameterStrictPositive( vals[[ "HR1" ]], "HR1")
    validate( need(vals[[ "HR0" ]] < 1, "HR t<T needs to be less than 1"))
    validate( need(vals[[ "HR1" ]] < 1, "HR t>T needs to be less than 1"))
    validateParameterStrictPositive( vals[[ "ctrlM0" ]], "ctrlM0" )
    validateParameterStrictPositive( vals[[ "ctrlM1" ]], "ctrlM1" )
    validateParameterStrictPositive( vals[[ "shape1" ]], "Weibull shape" )
  }
  
  validateParameterBtw0and1( vals[[ "alpha" ]], "alpha" )
  validateParameterBtw0and1( vals[[ "power" ]], "power" )
}


# This function parses the arguments given in the URL and returns
# A list with the parameter names and their values
parseParameters <- function( query ){
  vals <- list()
  
  # Numeric params 
  for( e in c( "numPatients", "studyDuration", "recDuration", "k", "r", "chP0", 
               "HR", "ctrlMedian", "shape", "ctrlM0", "HR0", "ctrlM1", "HR1", "shape1", "dropoutPropCtrl",
               "dropoutPropExp", "alpha", "power" ) ){
    if( !is.null( query[[ e ]] ) ){
        xval <- as.numeric( query[[ e ]] )
        if( !is.na(xval) ) {
          vals[[ e ]] <- xval
        }
    }
  }
  # String params
  for( e in c( "eventOrTime", "timePred", "eventPred", "useDropouts", "twoSided", "startDate"  ) ) {
    if( !is.null( query[[ e ]] ) ){
      xval <- as.character( query[[ e ]] )
      if( !is.na(xval) ) {
        vals[[ e ]] <- xval
      }
    }
  }
  
  vals
}


getMultipleNumeric <- function( val, varName, maxValue = Inf ){
  xvals <- strsplit( val, "," )[[1]]
  validate( need(length(xvals)>0, "X2: Need to provide prediction time points" ) )
  for( i in seq_along(xvals) ){
    xvals[i] <- as.numeric( xvals[i] )
    validate( need( !is.na(xvals[i]), paste0( varName, " needs to be numeric" ) ) )
    validate( need( xvals[i] > 0, paste0( varName, " needs to be greater than 0" ) ) )
    validate( need( xvals[i] <= maxValue, paste0( varName, " needs to be less than ", maxValue ) ) )
  }
  as.numeric( xvals )
}

getURL <- function( base, val ){
  base <- paste0( base, "?" )
  first <- TRUE
  for( e in names(val) ){
    if( !first ){
      base <- paste0( base, "&" ) 
    }
    base <- paste0( base, e, "=", val[[e]] )
    first <- FALSE
  }
  base
}