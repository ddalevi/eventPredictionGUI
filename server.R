
shinyServer( function( input, output, session ) {
  
  #' Use rective values to remember all params needed.
  mem.vals <- reactiveValues()
  def.param <- getParamDefaultValues()
  for( iparam in names( def.param ) ){
    mem.vals[[ iparam ]] <- def.param[[ iparam ]] 
  }
  
  getTwoSided <- reactive({
    validate( need( mem.vals[["twoSided"]], "Loading ..." ) )
    if(  mem.vals[["twoSided"]] == "Yes" ) return( TRUE )
    else return( FALSE )
  })
  
  getDropouts <- reactive({
    validate( need( mem.vals[["useDropouts"]], "Loading ..." ) )
    if(  mem.vals[["useDropouts"]] == "No" ) NULL
    else{
      list( time=12, proportions=c( mem.vals[["dropoutPropCtrl"]], mem.vals[["dropoutPropExp"]]))
    }
  })
  
  getStudy <- reactive({
    validateParameters( input, mem.vals )
    validateParameterStrictPositive( input$HR, "HR")
    if( mem.vals[["numChangePoints"]] == 0 ){
       Study(
          alpha = mem.vals[["alpha"]],
          power = mem.vals[["power"]],
          HR = mem.vals[["HR"]],
          r = mem.vals[["r"]],
          N = mem.vals[["numPatients"]],
          study.duration = mem.vals[["studyDuration"]],
          ctrl.median = mem.vals[["ctrlMedian"]],
          k = mem.vals[["k"]],
          acc.period = mem.vals[["recDuration"]],
          two.sided = getTwoSided(),
          dropout=getDropouts())
    } else {
      validate( need( input$HR0, "Loading ..." ) )
      validate( need( input$HR1, "Loading ..." ) )
      lagged <- LagEffect(
        Lag.T = mem.vals[["chP0"]],
        L.Ctr.median = mem.vals[["ctrlM0"]],
        L.HazardRatio = mem.vals[["HR0"]])
      Study(
        alpha = mem.vals[["alpha"]],
        power = mem.vals[["power"]],
        HR = mem.vals[["HR1"]],
        r = mem.vals[["r"]],
        N = mem.vals[["numPatients"]],
        study.duration = mem.vals[["studyDuration"]],
        ctrl.median = mem.vals[["ctrlM1"]],
        k = mem.vals[["k"]],
        acc.period = mem.vals[["recDuration"]],
        two.sided = getTwoSided(),
        lag.settings=lagged, 
        dropout=getDropouts() )
    }
  })

  
  getPrediction <- reactive({
    study <- getStudy()
    validate( need( input$eventOrTime, "Need prediction parameters" ) )
    if( input$eventOrTime == "Predict events|time"  ){
      xvals <- getMultipleNumeric( input$timePred, "Predict time", mem.vals[["studyDuration"]] ) 
      prediction <- predict(study, time.pred=xvals )
    } else {
      xvals <- getMultipleNumeric( input$eventPred, "Number of predicted events", mem.vals[["numPatients"]] ) 
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
                                min = 0, value = mem.vals[["numPatients"]] ),
                  numericInput( "studyDuration",
                                "Study duration (months):",
                                min = 0, value = mem.vals[["studyDuration"]] ),
                  numericInput( "recDuration",
                                "Recruitment duration (months):",
                                min = 0, value = mem.vals[["recDuration"]] ),
                  numericInput( "k",
                                "Recruitment shape parameter:",
                                min = 0, value = mem.vals[["k"]]),
                  numericInput( "r", 
                                "Randomization balance (nE/nC):", 
                                min=0, value= mem.vals[["r"]]  )
                  )
              
            },
            "eventParams" = {
              list(
                selectInput( "numChangePoints", "Change points: ", c( "0", "1" ), mem.vals[["numChangePoints"]] ),
                uiOutput( "eventParamsChPX" ) )
              },
              "predictParams" = {
                list(
                  selectInput( "eventOrTime", "Predict: ", c( "Predict events|time", "Predict time|events" ) ),
                  uiOutput( "eventOrTimeSel" )
                )
              },
            "dropoutParams" = {
              list( 
                selectInput( "useDropouts", "Use dropouts:", c( "Yes", "No" ), mem.vals[["useDropouts"]] ),
                numericInput( "dropoutPropCtrl", "Proportion (ctrl): ", mem.vals[["dropoutPropCtrl"]], step=0.01, min=0, max=1 ),
                numericInput( "dropoutPropExp", "Proportion (exp): ", mem.vals[["dropoutPropExp"]], step=0.01, min=0, max=1 )
              )
            },
            "criticalEvents" = {
              list( 
                numericInput( "alpha", "Alpha: ", mem.vals[["alpha"]], step=0.01, min=0, max=1 ), 
                numericInput( "power", "Power:", mem.vals[["power"]], step=0.05, min=0, max=1 ),
                selectInput( "twoSided", "Two sided test:", c( "Yes", "No" ), mem.vals[["twoSided"]] )
                )
            }
    )
  })
    
  output$eventParamsChPX <- renderUI({
    validate( need( mem.vals[["numChangePoints"]], "loading" ) )
    if( mem.vals[["numChangePoints"]] == 0 ){
      list(
        numericInput( "ctrlMedian",
                      "Control median (months):",
                      min = 0, value = mem.vals[["ctrlMedian"]] ),
        numericInput( "HR", "HR: ",
                      min=0, value=mem.vals[["HR"]], step=0.05 ))
    } else {
      list(
        numericInput( "chP0", "Change point (T, months): ", mem.vals[["chP0"]]),
        numericInput( "ctrlM0",
                      "Control median t<T (months):",
                      min = 0, value = mem.vals[["ctrlMedian"]] ),
        numericInput( "HR0", "HR t<T: ",
                      min=0, value=mem.vals[["HR0"]], step=0.05 ),
         numericInput( "ctrlM1",
                    "Control median t>T (months):",
                    min = 0, value = mem.vals[["ctrlMedian"]] ),
         numericInput( "HR1", "HR t>T: ",
                    min=0, value=mem.vals[["HR1"]], step=0.05 ))
    }
  })
  
  getStartDate <- reactive({
    if( input$useIntegers ){
      return( "0" )
    }
    format(anydate(mem.vals[["startDate"]]), "%d/%m/%Y")
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
    validate( need( mem.vals[["eventOrTime"]], "loading" ) )
    if( mem.vals[["eventOrTime"]] == "Predict events|time" ){
      textInput( "timePred", "Time [months]: ", value=mem.vals[["timePred"]])
    } else {
      textInput( "eventPred", "Number of events: ", value=mem.vals[["eventPred"]])
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
  observeEvent( input[[ iparams[1] ]], { mem.vals[[ iparams[1] ]] <- input[[ iparams[1] ]] })
  observeEvent( input[[ iparams[2] ]], { mem.vals[[ iparams[2] ]] <- input[[ iparams[2] ]] })
  observeEvent( input[[ iparams[3] ]], { mem.vals[[ iparams[3] ]] <- input[[ iparams[3] ]] })
  observeEvent( input[[ iparams[4] ]], { mem.vals[[ iparams[4] ]] <- input[[ iparams[4] ]] })
  observeEvent( input[[ iparams[5] ]], { mem.vals[[ iparams[5] ]] <- input[[ iparams[5] ]] })
  observeEvent( input[[ iparams[6] ]], { mem.vals[[ iparams[6] ]] <- input[[ iparams[6] ]] })
  observeEvent( input[[ iparams[7] ]], { mem.vals[[ iparams[7] ]] <- input[[ iparams[7] ]] })
  observeEvent( input[[ iparams[8] ]], { mem.vals[[ iparams[8] ]] <- input[[ iparams[8] ]] })
  observeEvent( input[[ iparams[9] ]], { mem.vals[[ iparams[9] ]] <- input[[ iparams[9] ]] })
  observeEvent( input[[ iparams[10] ]], { mem.vals[[ iparams[10] ]] <- input[[ iparams[10] ]] })
  observeEvent( input[[ iparams[11] ]], { mem.vals[[ iparams[11] ]] <- input[[ iparams[11] ]] })
  observeEvent( input[[ iparams[12] ]], { mem.vals[[ iparams[12] ]] <- input[[ iparams[12] ]] })
  observeEvent( input[[ iparams[13] ]], { mem.vals[[ iparams[13] ]] <- input[[ iparams[13] ]] })
  observeEvent( input[[ iparams[14] ]], { mem.vals[[ iparams[14] ]] <- input[[ iparams[14] ]] })
  observeEvent( input[[ iparams[15] ]], { mem.vals[[ iparams[15] ]] <- input[[ iparams[15] ]] })
  observeEvent( input[[ iparams[16] ]], { mem.vals[[ iparams[16] ]] <- input[[ iparams[16] ]] })
  observeEvent( input[[ iparams[17] ]], { mem.vals[[ iparams[17] ]] <- input[[ iparams[17] ]] })
  observeEvent( input[[ iparams[18] ]], { mem.vals[[ iparams[18] ]] <- input[[ iparams[18] ]] })
  observeEvent( input[[ iparams[19] ]], { mem.vals[[ iparams[19] ]] <- input[[ iparams[19] ]] })
  observeEvent( input[[ iparams[20] ]], { mem.vals[[ iparams[20] ]] <- input[[ iparams[20] ]] })
  observeEvent( input[[ iparams[21] ]], { mem.vals[[ iparams[21] ]] <- input[[ iparams[21] ]] })
  observeEvent( input[[ iparams[22] ]], { mem.vals[[ iparams[22] ]] <- input[[ iparams[22] ]] })
  observeEvent( input[[ iparams[23] ]], { mem.vals[[ iparams[23] ]] <- input[[ iparams[23] ]] })
})
