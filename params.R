
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
    "eventOrTime"= "Predict events|time",
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
  validate( need( vals[["numChangePoints"]], "Loading.." ) )
  validateParameterStrictPositive( input$HR, "HR")
  validate( need(input$HR < 1, "HR needs to be less than 1"))
  validateParameterStrictPositive( input$k, "Recruitment shape parameter")
  validateParameterStrictPositive( input$r, "Randomization balance" ) 
  validateParameterStrictPositive( input$numPatients, "Number of patients" )
  validateParameterStrictPositive( input$studyDuration, "Study duration" )
  validateParameterStrictPositive( input$recDuration, "Recruitment duration" )
  validateParameterStrictPositive( input$ctrlMedian, "Control median survival" )
  validateParameterStrictPositive( input$shape, "Weibull shape" )
  
  validate( need( input$studyDuration > input$recDuration, "Study period needs to be longer than recruitment period" ) )
  validate( need( input$startDate, "Need to provide a study start date"))
  validate( need( input$eventOrTime, "Need prediction parameters" ) )
  if( input$eventOrTime == "Predict events|time"  ){
    xvals <- getMultipleNumeric( input$timePred, "Predict time", vals[["studyDuration"]] ) 
    validate( need( input$studyDuration >= max( xvals ), "Prediction time point exceeds study duration" ) )
  } else {
    xvals <- getMultipleNumeric( input$eventPred, "Number of predicted events", input$numPatients ) 
    validate( need( input$numPatients >= max( xvals ), "Too few patients compared to predicted number of events" ) )
  }
  
  if( input$numChangePoints > 0 ){
    validateParameterStrictPositive( input$chP0, "Change point (T)" )
    validateParameterStrictPositive( input$HR0, "HR0")
    validateParameterStrictPositive( input$HR1, "HR1")
    validate( need(input$HR0 < 1, "HR t<T needs to be less than 1"))
    validate( need(input$HR1 < 1, "HR t>T needs to be less than 1"))
    validateParameterStrictPositive( input$ctrlM0, "ctrlM0" )
    validateParameterStrictPositive( input$ctrlM1, "ctrlM1" )
    validateParameterStrictPositive( input$shape1, "Weibull shape" )
  }
  
  validateParameterBtw0and1( input$alpha, "alpha" )
  validateParameterBtw0and1( input$power, "power" )
}


getMultipleNumeric <- function( val, varName, maxValue = Inf ){
  validate( need(is.character(val) && val >"", "Need to provide prediction time points" ) )
  xvals <- strsplit( val, "," )[[1]]
  validate( need(length(xvals)>0, "Need to provide prediction time points" ) )
  for( i in seq_along(xvals) ){
    xvals[i] <- as.numeric( xvals[i] )
    validate( need( !is.na(xvals[i]), paste0( varName, " needs to be numeric" ) ) )
    validate( need( xvals[i] > 0, paste0( varName, " needs to be greater than 0" ) ) )
    validate( need( xvals[i] <= maxValue, paste0( varName, " needs to be less than ", maxValue ) ) )
  }
  as.numeric( xvals )
}