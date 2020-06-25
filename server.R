
shinyServer( function( input, output, session ) {

  
  #' Use rective values to remember all params needed.
  mem.vals <- reactiveValues()
  def.param <- getParamDefaultValues()
  for( iparam in names( def.param ) ){
    mem.vals[[ iparam ]] <- def.param[[ iparam ]] 
  }
  
  
  # Read parameters from URL
  observe({
    res.url <- gsub( '%20', '', session$clientData$url_search )
    query <- parseQueryString( res.url )
    mylist <- parseParameters( query )
    for( e in names( mylist ) ) {
      mem.vals[[ e ]] <- mylist[[ e ]]
    }
    # Hmm, not sure why this is needed
    mem.vals[[ "timePred"]] <- as.character( mem.vals[[ "timePred"]] )
  })
  
  
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
    validate( need( !is.null(mem.vals), "Loading .." ))
    validateParameters( input, mem.vals )
    validateParameterStrictPositive( mem.vals[[ "HR" ]], "HR")
    if( mem.vals[["numChangePoints"]] == 0 ){
       Study(
          alpha = mem.vals[["alpha"]],
          power = mem.vals[["power"]],
          HR = mem.vals[["HR"]],
          r = mem.vals[["r"]],
          N = mem.vals[["numPatients"]],
          study.duration = mem.vals[["studyDuration"]],
          ctrl.median = mem.vals[["ctrlMedian"]],
          shape = mem.vals[["shape"]],
          k = mem.vals[["k"]],
          acc.period = mem.vals[["recDuration"]],
          two.sided = getTwoSided(),
          dropout=getDropouts())
    } else {
      validate( need( mem.vals[[ "HR0" ]], "Loading ..." ) )
      validate( need( mem.vals[[ "HR1" ]], "Loading ..." ) )
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
        shape = mem.vals[["shape1"]],
        k = mem.vals[["k"]],
        acc.period = mem.vals[["recDuration"]],
        two.sided = getTwoSided(),
        lag.settings=lagged, 
        dropout=getDropouts() )
    }
  })

  
  getPrediction <- reactive({
    study <- getStudy()
    validate( need( mem.vals[[ "eventOrTime" ]], "Need prediction parameters" ) )
    if( mem.vals[[ "eventOrTime" ]] == "Predict_events|time"  ){
      xvals <- getMultipleNumeric( mem.vals[[ "timePred" ]], "Predict time", mem.vals[["studyDuration"]] ) 
      prediction <- predict(study, time.pred=xvals )
    } else {
      xvals <- getMultipleNumeric( mem.vals[[ "eventPred" ]], "Number of predicted events", mem.vals[["numPatients"]] ) 
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
                  selectInput( "eventOrTime", "Predict: ", c( "Predict_events|time", "Predict_time|events" ), mem.vals[["eventOrTime"]] ),
                  uiOutput( "eventOrTimeSel" )
                )
              },
            "dropoutParams" = {
              list( 
                selectInput( "useDropouts", "Use dropouts:", c( "Yes", "No" ), mem.vals[["useDropouts"]] ),
                numericInput( "dropoutPropCtrl", "Proportion (ctrl): ", mem.vals[["dropoutPropCtrl"]], step=0.01, min=0, max=1 ),
                numericInput( "dropoutPropExp", "Proportion (exp): ", mem.vals[["dropoutPropExp"]], step=0.01, min=0, max=1 ),
                uiOutput( "paramsDescTxt" )
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
                      min=0, value=mem.vals[["HR"]], step=0.05 ),
        numericInput( "shape", "Shape: ",
                      min=0, value=mem.vals[["shape"]], step=0.05 ))
    } else {
      list(
        numericInput( "chP0", "Change point (T, months): ", mem.vals[["chP0"]]),
        numericInput( "shape1", "Shape: ",
                      min=0, value=mem.vals[["shape1"]], step=0.05 ),
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
    if( mem.vals[["eventOrTime"]] == "Predict_events|time" ){
      validate( need( mem.vals[["timePred"]], "loading" ) )
      textInput( "timePred", "Time [months]: ", value=mem.vals[["timePred"]])
    } else {
      validate( need( mem.vals[["eventPred"]], "loading" ) )
      textInput( "eventPred", "Number of events: ", value=mem.vals[["eventPred"]])
    }
  })
  
  output$paramsDescTxt <- renderUI({
    HTML( "<i>Note, annual dropout rates specified in the absence of events (competing risks may lead to a different proportion)</i>" )
  })
    
  output$epPlot <- renderPlot({
    prediction <- getPrediction()
    plot(prediction, options = DisplayOptions(StartDate=getStartDate()),
         show.title=FALSE )
  })

  output$epText <- renderText({
    text <- eventPrediction:::getFromParameterText(getPrediction(),DisplayOptions(StartDate=getStartDate(), 
                                                                                  text.width=110, Tcrithr=TRUE) ) 
    HTML(text)
  })
  
  output$displayOptions <- renderUI({
    list(
        dateInput( "startDate", "Study start date:", value=mem.vals[["startDate"]]  ),
        checkboxInput( "useIntegers", "Months as integers:"  ) 
      )
  })
  
  output$state <- renderUI({

    wrapLines <- function(x, n=100) {
      strsplit(x, paste0( "(?<=.{", n, "})" ), perl = TRUE)[[1]]
    }
    port <- session$clientData$url_port
    urlpath <- session$clientData$url_pathname
    baseurl <- session$clientData$url_hostname 
    if( !is.null(port) && port > "" ) baseurl <- paste0( baseurl, ":", port )
    if( !is.null(urlpath) && urlpath > "" ) baseurl <- paste0( baseurl, urlpath )
    
    myurl <- as.character( getURL( baseurl, mem.vals ) )    
    myurl <- paste0( wrapLines(myurl), collapse = ' <br> '  )
    mystr <- paste0( "<h2>State</h2>The current state of the application can be reproduced by saving this URL: <br><br>" )
    
    list( HTML(  mystr ),
          HTML( myurl ), 
          br(), br(), 
          HTML( paste0( "<h2>Session information</h2>", paste0( capture.output(sessionInfo()), collapse="<br>" ), "<br>") ) )
  })
  
  output$helpTxt <- renderText({
    HTML( getHelpTxt() )
  })
  
  output$aboutTxt <- renderText({
    HTML( getAboutTxt() )
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
  observeEvent( input[[ iparams[24] ]], { mem.vals[[ iparams[24] ]] <- input[[ iparams[24] ]] })
  observeEvent( input[[ iparams[25] ]], { mem.vals[[ iparams[25] ]] <- input[[ iparams[25] ]] })
})
