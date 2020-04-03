# Define server logic required to draw a histogram
shinyServer( function( input, output, session ) {
  
  #' Use rective values to remember all params needed.
  vals <- reactiveValues()
  def.param <- getParamDefaultValues()
  for( iparam in names( def.param ) ){
    vals[[ iparam ]] <- def.param[[ iparam ]] 
  }
  
  
  getStudy <- reactive({
    validateParameters( input )
    
    if( vals[["numChangePoints"]] == 0 ){
       Study(
          alpha = 0.05,
          power = 0.89,
          HR = vals[["HR"]],
          r = vals[["r"]],
          N = vals[["numPatients"]],
          study.duration = vals[["studyDuration"]],
          ctrl.median = vals[["ctrlMedian"]],
          k = vals[["k"]],
          acc.period = vals[["recDuration"]],
          two.sided = TRUE)
    } else {
      validate( need( input$HR0, "Loading ..." ) )
      validate( need( input$HR1, "Loading ..." ) )
      lagged <- LagEffect(
        Lag.T = vals[["chP0"]],
        L.Ctr.median = vals[["ctrlM1"]],
        L.HazardRatio = vals[["HR1"]])
      Study(
        alpha = 0.05,
        power = 0.89,
        HR = vals[["HR0"]],
        r = vals[["r"]],
        N = vals[["numPatients"]],
        study.duration = vals[["studyDuration"]],
        ctrl.median = vals[["ctrlM0"]],
        k = vals[["k"]],
        acc.period = vals[["recDuration"]],
        two.sided = TRUE,
        lag.settings=lagged )
    }
  })

  validateParameters <- function( input ) {
    validate( need( vals[["numChangePoints"]], "Loading.." ) )
    validateParameterStrictPositive( input$HR, "HR")
    validate( need(input$HR < 1, "HR needs to be less than 1"))
    validateParameterStrictPositive( input$k, "Recruitment shape parameter")
    validateParameterStrictPositive( input$r, "Randomization balance" ) 
    validateParameterStrictPositive( input$numPatients, "Number of patients" )
    validateParameterStrictPositive( input$studyDuration, "Study duration" )
    validateParameterStrictPositive( input$recDuration, "Recruitment duration" )
    validateParameterStrictPositive( input$ctrlMedian, "Control median survival" )
    
    validate( need( input$studyDuration > input$recDuration, "Study period needs to be longer than recruitment period" ) )
    validate( need( input$startDate, "Need to provide a study start date"))
    validate( need( input$eventOrTime, "Need prediction parameters" ) )
    if( input$eventOrTime == "Predict events|time"  ){
      xvals <- getMultipleNumeric( input$timePred, "Predict time", vals[["studyDuration"]] ) 
      validate( need( input$studyDuration >= max( xvals ), "Prediction time point exceeds study duration" ) )
    } else {
      xvals <- getMultipleNumeric( input$eventPred, "Number of predicted events", vals[["numPatients"]] ) 
      validate( need( input$numPatients >= max( xvals ), "Too few patients compared to predicted number of events" ) )
    }
    
    if( input$numChangePoints > 0 ){
      validateParameterStrictPositive( input$chP0, "Change point (T)" )
      validateParameterStrictPositive( input$HR0, "HR0")
      validateParameterStrictPositive( input$HR1, "HR1")
      validate( need(input$HR0 < 1, "HR t<T needs to be less than 1"))
      validate( need(input$HR1 < 1, "HR t>T needs to be less than 1"))
      validateParameterStrictPositive( input$ctrlM0, "HR0")
      validateParameterStrictPositive( input$ctrlM1, "HR1")
    }
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
  
  getPrediction <- reactive({
    study <- getStudy()
    validate( need( input$eventOrTime, "Need prediction parameters" ) )
    if( input$eventOrTime == "Predict events|time"  ){
      xvals <- getMultipleNumeric( input$timePred, "Predict time", vals[["studyDuration"]] ) 
      prediction <- predict(study, time.pred=xvals )
    } else {
      xvals <- getMultipleNumeric( input$eventPred, "Number of predicted events", vals[["numPatients"]] ) 
      prediction <- predict( study, event.pred=xvals )
    }
    prediction
  })
  
  output$showParams <- renderUI({
    switch( input$eventParam,
            "trialParams" = { 
                list(
                  numericInput( "numPatients",
                                "Number of patients:",
                                min = 0, value = vals[["numPatients"]] ),
                  numericInput( "studyDuration",
                                "Study duration (months):",
                                min = 0, value = vals[["studyDuration"]] ),
                  numericInput( "recDuration",
                                "Recruitment duration (months):",
                                min = 0, value = vals[["recDuration"]] ),
                  numericInput( "k",
                                "Recruitment shape parameter:",
                                min = 0, value = vals[["k"]]),
                  numericInput( "r", 
                                "Randomization balance (nE/nC):", 
                                min=0, value= vals[["r"]]  )
                  )
              
            },
            "eventParams" = {
              list(
                selectInput( "numChangePoints", "Change points: ", c( "0", "1" ), vals[["numChangePoints"]] ),
                uiOutput( "eventParamsChPX" ) )
              },
              "predictParams" = {
                list(
                  selectInput( "eventOrTime", "Predict: ", c( "Predict events|time", "Predict time|events" ) ),
                  uiOutput( "eventOrTimeSel" )
                )
              }
    )
  })
    
  output$eventParamsChPX <- renderUI({
    validate( need( vals[["numChangePoints"]], "loading" ) )
    if( vals[["numChangePoints"]] == 0 ){
      list(
        numericInput( "ctrlMedian",
                      "Control median (months):",
                      min = 0, value = vals[["ctrlMedian"]] ),
        numericInput( "HR", "HR: ",
                      min=0, value=vals[["HR"]], step=0.05 ))
    } else {
      list(
        numericInput( "chP0", "Change point (T, months): ", vals[["chP0"]]),
        numericInput( "ctrlM0",
                      "Control median t<T (months):",
                      min = 0, value = vals[["ctrlMedian"]] ),
        numericInput( "HR0", "HR t<T: ",
                      min=0, value=vals[["HR"]], step=0.05 ),
         numericInput( "ctrlM1",
                    "Control median t>T (months):",
                    min = 0, value = vals[["ctrlMedian"]] ),
         numericInput( "HR1", "HR t>T: ",
                    min=0, value=vals[["HR"]], step=0.05 ))
    }
  })
  
  getStartDate <- reactive({
    if( input$useIntegers ){
      return( "0" )
    }
    format(anydate(vals[["startDate"]]), "%d/%m/%Y")
  })
  
  output$downloadEPPlot <- downloadHandler(
    filename = function() { paste0( "EPplot_", Sys.Date(), ".", input$format ) },
    content = function(file) {
        prediction <- getPrediction()
        if( input$format=="pdf" ) pdf( file )
        else png( file )
        plot( prediction, options = DisplayOptions(StartDate=getStartDate()),
              show.title=input$includeTitle )
        dev.off()
    }
  )
  
  output$eventOrTimeSel <- renderUI({
    validate( need( vals[["eventOrTime"]], "loading" ) )
    if( vals[["eventOrTime"]] == "Predict events|time" ){
      textInput( "timePred", "Time [months]: ", value=vals[["timePred"]])
    } else {
      textInput( "eventPred", "Number of events: ", value=vals[["eventPred"]])
    }
  })
    
  output$epPlot <- renderPlot({
    prediction <- getPrediction()
    plot(prediction, options = DisplayOptions(StartDate=getStartDate()),
         show.title=FALSE )
  })

  output$epText <- renderText({
    text <- eventPrediction:::getFromParameterText(getPrediction(),DisplayOptions(StartDate=getStartDate(), 
                                                                                  text.width=110, Tcrithr=FALSE ) ) 
    HTML(text)
  })
  
  output$displayOptions <- renderUI({
    list(
        dateInput( "startDate", "Study start date:"  ),
        checkboxInput( "useIntegers", "Months as integers:"  ) 
      )
  })
  
  
  #' Observe any changes and update values based on those
  #' 
  iparams <- names( getParamDefaultValues() )
  
  # Not sure why this cannot be a loop?
  observeEvent( input[[ iparams[1] ]], { vals[[ iparams[1] ]] <- input[[ iparams[1] ]] })
  observeEvent( input[[ iparams[2] ]], { vals[[ iparams[2] ]] <- input[[ iparams[2] ]] })
  observeEvent( input[[ iparams[3] ]], { vals[[ iparams[3] ]] <- input[[ iparams[3] ]] })
  observeEvent( input[[ iparams[4] ]], { vals[[ iparams[4] ]] <- input[[ iparams[4] ]] })
  observeEvent( input[[ iparams[5] ]], { vals[[ iparams[5] ]] <- input[[ iparams[5] ]] })
  observeEvent( input[[ iparams[6] ]], { vals[[ iparams[6] ]] <- input[[ iparams[6] ]] })
  observeEvent( input[[ iparams[7] ]], { vals[[ iparams[7] ]] <- input[[ iparams[7] ]] })
  observeEvent( input[[ iparams[8] ]], { vals[[ iparams[8] ]] <- input[[ iparams[8] ]] })
  observeEvent( input[[ iparams[9] ]], { vals[[ iparams[9] ]] <- input[[ iparams[9] ]] })
  observeEvent( input[[ iparams[10] ]], { vals[[ iparams[10] ]] <- input[[ iparams[10] ]] })
  observeEvent( input[[ iparams[11] ]], { vals[[ iparams[11] ]] <- input[[ iparams[11] ]] })
  observeEvent( input[[ iparams[12] ]], { vals[[ iparams[12] ]] <- input[[ iparams[12] ]] })
  observeEvent( input[[ iparams[13] ]], { vals[[ iparams[13] ]] <- input[[ iparams[13] ]] })
  observeEvent( input[[ iparams[14] ]], { vals[[ iparams[14] ]] <- input[[ iparams[14] ]] })
  observeEvent( input[[ iparams[15] ]], { vals[[ iparams[15] ]] <- input[[ iparams[15] ]] })
  observeEvent( input[[ iparams[16] ]], { vals[[ iparams[16] ]] <- input[[ iparams[16] ]] })
  observeEvent( input[[ iparams[17] ]], { vals[[ iparams[17] ]] <- input[[ iparams[17] ]] })
})
