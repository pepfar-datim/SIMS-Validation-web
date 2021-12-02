library(shiny)
library(shinyjs)
library(openxlsx)
library(magrittr)
require(futile.logger)
require(shinyWidgets)
require(devtools)
install_github("pepfar-datim/SIMS-Validation")
install_github("pepfar-datim/datim-validation")
require(SIMS4Validation)
require(datimvalidation)


shinyServer(function(input, output, session) {
  
  ready <- reactiveValues(ok = FALSE)
  
  observeEvent(input$importdatafile, {
    shinyjs::enable("validate")
    ready$ok <- FALSE
  }) 
  
  observeEvent(input$validate, {
    shinyjs::disable("validate")
    ready$ok <- TRUE
  })  
  
  observeEvent(input$reset_input, {
    enableUI()
    ready$ok<-FALSE
    shinyjs::hide("downloadDataValidation")
    shinyjs::hide("downloadDataNormalized")
  })
  
  observeEvent(input$logout,{
    flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged out."))
    ready$ok <- FALSE
    user_input$authenticated<-FALSE
    user_input$d2_session<-NULL
    session$reload()
    gc()
    
  } )
  
  disableUI<-function(){
    shinyjs::disable("type")
    shinyjs::disable("isoPeriod")
    shinyjs::disable("ou_scheme")
    shinyjs::disable("de_scheme")
    shinyjs::disable("id_scheme")
    shinyjs::disable("importdatafile")
    shinyjs::disable("header")
  }
  
  enableUI<-function(){
    shinyjs::enable("type")
    shinyjs::enable("isoPeriod")
    shinyjs::enable("ou_scheme")
    shinyjs::enable("de_scheme")
    shinyjs::enable("id_scheme")
    shinyjs::enable("importdatafile")
    shinyjs::enable("header")
  }
  
  output$ui <- renderUI({
    
    if (user_input$authenticated == FALSE) {
      ##### UI code for login page
      fluidPage(
        fluidRow(
          column(width = 2, offset = 5,
                 br(), br(), br(), br(),
                 uiOutput("server"),
                 uiOutput("uiLogin"),
                 uiOutput("pass")
          )
        )
      )
    } else {
      fluidPage(
        tags$head(tags$style(".shiny-notification {
                             position: fixed;
                             top: 10%;
                             left: 33%;
                             right: 33%;}")),
        sidebarLayout(
          sidebarPanel(
            shinyjs::useShinyjs(),
            fileInput(
              "importdatafile",
              "Choose data file:",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                "application/json",
                "application/xml",
                ".csv",
                ".json",
                ".xml"
              )
            ),
            selectInput("type", "Type:",
                        c(
                          "CSV" = "csv",
                          "JSON" = "json",
                          "XML" = "xml"
                        )),
            selectInput("isoPeriod", "Iso Period", 
                        c(
                          "2020Q4" = "2020Q4",
                          "2021Q1" = "2021Q1",
                          "2021Q2" = "2021Q2",
                          "2021Q3" = "2021Q3",
                          "2021Q4" = "2021Q4",
                          "2022Q1" = "2022Q1",
                          "2022Q2" = "2022Q2",
                          "2022Q3" = "2022Q3",
                          "2022Q4" = "2022Q4"
                        )),
            selectInput(
              "de_scheme",
              "Data Element ID scheme:",
              c(
                "Name" = "name"
              ),
              selected = "name"
            ),
            selectInput(
              "ou_scheme",
              "Orgunit ID scheme:",
              c(
                "ID" = "id",
                "Code" = "code"
              ),
              selected = "id"
            ),
            selectInput(
              "id_scheme",
              "Mechanism ID scheme:",
              c(
                "ID" = "id",
                "Code" = "code"
              ),
              selected = "id"
            ),
            checkboxInput("header", "CSV Header", FALSE),
            tags$hr(),
            actionButton("validate","Validate"),
            downloadButton("downloadDataValidation", "Download Validation Report"),
            tags$hr(),
            downloadButton("downloadDataNormalized", "Download Normalized Data"),
            tags$hr(),
            div(style = "display: inline-block; vertical-align:top; width: 80 px;", actionButton("reset_input", "Reset inputs")),
            div(style = "display: inline-block; vertical-align:top; width: 80 px;", actionButton("logout", "Logout"))
          ),
          mainPanel(tabsetPanel(
            type = "tabs",
            tabPanel("Messages",   tags$ul(uiOutput('messages')))
          ))
        ))
  }
})
  #SIMS Datasets
  dataSets <- c("VP0uG6tzB5l", "lvfFcexh1nB")
  #Output Dir
  output_dir <- tempdir()
  user_input <- reactiveValues(authenticated = FALSE, 
                               status = "",
                               d2_session = NULL,
                               user_ous = NA)
  
  observeEvent(input$login_button, 
               {
                 
                 tryCatch(  {  d2login<-datimutils::loginToDATIM(base_url =  "http://dev-de.datim.org/",
                                                        username = input$user_name,
                                                        password = input$password) 
                 
                           },
                            #This function throws an error if the login is not successful
                            error=function(e) {
                              sendSweetAlert(
                                session,
                                title = "Login failed",
                                text = "Please check your username/password!",
                                type = "error")
                              flog.info(paste0("User ", input$user_name, " login failed."), name = "simswebvalidation")
                            } )
                 
                 if ( exists("d2_default_session"))  {
                   
                   user_input$authenticated<-TRUE
                   user_input$d2_session<-d2_default_session$clone()
                   flog.info(paste0("User ", user_input$d2_session$me$userCredentials$username, " logged in."), name = "datapack")
                 }
                 
               })
  
  # password entry UI componenets:
  #   username and password text fields, login button
  output$uiLogin <- renderUI({
    wellPanel(
        fluidRow(img(src='pepfar.png', align = "center")),
        fluidRow(h4("Welcome to the SIMS Validation tool")
      ),
      fluidRow(
      textInput("user_name", "User Name:",width = "600px"),
      passwordInput("password", "Password:",width = "600px"),
      actionButton("login_button", "Log in")
    ))
  })
  
  validate<-function() {
    
    assign("d2_default_session", user_input$d2_session, parent.frame())
    
    shinyjs::hide("downloadDataValidation")
    shinyjs::hide("downloadDataNormalized")
    if (!ready$ok) {return(NULL)}
    
    #Lock the UI and hide download button
    disableUI()
    inFile <- input$importdatafile
    
    if (is.null(inFile)) return(NULL)
    
    messages<-list()
    vr_results<-list()
    has_error<-FALSE
    
    withProgress(message = 'Validating file', value = 0,{
    incProgress(1/14, detail = ("Loading metadata"))
    incProgress(1/14, detail = ("Parsing data"))
    validation<-list()
    normalized<-list()
    
    d2_default_session <-user_input$d2_session
    
    print(input$importdatafile$datapath)
    print(input$importdatafile$name)
    print(input$type)
    print(input$id_scheme)
    print(input$de_scheme)
    print(input$ou_scheme)
    print(input$isoPeriod)
    print(input$header)
    print(d2_default_session)
    print(dataSets)
    print(output_dir)
    #sims
    #Bad values check
    incProgress(1/14, detail = ("SIMS Validator"))
    
    #Variables 
    folder<-output_dir
    myfile<- input$importdatafile
    file_type<- input$type
    idScheme<-input$id_scheme
    dataElementIdScheme<- input$de_scheme
    orgUnitIdScheme<- input$ou_scheme
    isoPeriod<-input$isoPeriod
    fileHasHeader<- input$header
    d2_default_session<-user_input$d2_session
    dataSets<-dataSets
    
    #SIMS Validator Script Decomposed
    incProgress(1/14, detail = ("Parsing the data with d2Parser"))
    path <- myfile$datapath
    filename <-myfile$name
    d2session <- d2_default_session
    
    file_summary <- c()
    file_summary["file"] <- filename
    
    options("organisationUnit"="ybg3MO3hcf4")
    # parse using regular parser, used to identify period shifts and overlapping assessments
    d <- datimvalidation::d2Parser(file = path, type = file_type, dataElementIdScheme = dataElementIdScheme, orgUnitIdScheme = orgUnitIdScheme, idScheme = idScheme, invalidData = TRUE, d2session=d2_default_session)
    
    if(any(class(d) == "data.frame")){
      # no issues
    } else {
      print(d)
    }
    
    #
    # VALIDATION
    
    # 1. parse input file
    print(path)
    print(dataElementIdScheme)
    print(orgUnitIdScheme)
    print(idScheme)
    print(fileHasHeader)
    print(isoPeriod)
    print(d2_default_session)
    incProgress(1/14, detail = ("Parsing the data with SIMS parser"))
    # parse using SIMS parser - this parser does period shifting of overlapping SIMS assessments
    d2 <- datimvalidation::sims2Parser(file=path, dataElementIdScheme = dataElementIdScheme, orgUnitIdScheme = orgUnitIdScheme, idScheme = idScheme, invalidData=TRUE, hasHeader=fileHasHeader, isoPeriod=isoPeriod)
    
    file_summary["record count"] = length(d2$comment)
    file_summary["assessment count"] = length(unique(d2$comment))
    
    messages <- append(paste(length(d2$comment), " record count"), messages)
    messages <- append(paste(length(unique(d2$comment)), " assessment count"), messages)
    
    #Count of assessments per operating unit (based on the assessment id column);
    #ou_map <- getOrganisationUnitMap()
    #assmt_per_ou = sqldf('select ou_map.ancestors.name as operatingUnit, count(comment) from d2 join ou_map on d2.orgUnit = ou_map.id group by ou_map.ancestors.name')
    
    assmt_per_ou = sqldf::sqldf('select orgUnit, count(distinct(comment)) from d2 group by orgUnit')
    file_summary["assessment count per operating unit"] = "------"
    ou_map = vector(mode = "list")
    for(col in 1:length(assmt_per_ou$orgUnit)) {
      url <- paste0(d2_default_session$base_url, "api/", api_version(),
                    "/organisationUnits/",assmt_per_ou[col,1],".json?fields=ancestors[name],name")
      r <- httr::GET(url, httr::timeout(60), handle = d2_default_session$handle)
      r <- httr::content(r, "text")
      ou <- jsonlite::fromJSON(r, flatten = TRUE)$ancestors$name[3]
      if(is.na(ou)){
        ou <- jsonlite::fromJSON(r, flatten = TRUE)$name
      }
      if(is.null(ou_map[[ou]])){
        ou_map[[ou]] <- 0
      }
      ou_map[[ou]] <- ou_map[[ou]] + as.numeric(assmt_per_ou[col,2])
    }
    for(i in 1:length(ou_map)){
      ou <- names(ou_map)[i]
      file_summary[ou] <- ou_map[[ou]]
    }
    #Count of assessments per mechanism (based on the assessment id column);
    #assmt_per_aoc = sqldf('select attributeOptionCombo, count(comment) from d2 group by attributeOptionCombo')
    #file_summary["assessment count per mechanism"] = "------"
    #for(col in 1:length(assmt_per_aoc$attributeOptionCombo)) {
    #  url <- paste0(getOption("baseurl"), "api/", api_version(),
    #                "/categoryOptionCombos/",assmt_per_aoc[col,1],".json?fields=name")
    #    r <- httr::GET(url, httr::timeout(60))
    #    r <- httr::content(r, "text")
    #    aoc <- jsonlite::fromJSON(r, flatten = TRUE)$name
    #  file_summary[aoc] = assmt_per_aoc[col,2]
    #}
    mech_map <- getMechanismsMap()
    assmt_per_aoc = sqldf::sqldf('select mech_map.code as attributeOptionCombo, count(distinct(d2.comment)) from d2 join mech_map on mech_map.id = d2.attributeOptionCombo group by d2.attributeOptionCombo')
    file_summary["assessment count per mechanism"] = "------"
    for(col in 1:length(assmt_per_aoc$attributeOptionCombo)) {
      file_summary[assmt_per_aoc[col,1]] = assmt_per_aoc[col,2]
    }
    
    #Count of unique assessment id coversheet data element values;
    de_map <- datimvalidation::getDataElementMap(d2session=d2_default_session) # used to produce post-shift duplicates with codes
    assmt_per_unique_cs_de = sqldf::sqldf("select de_map.code as dataElement, count(distinct(d2.value)) from d2 join de_map on de_map.id = d2.dataElement where de_map.code = 'SIMS.CS_ASMT_ID' group by d2.dataElement")
    file_summary["assessment count per unique cs data elements"] = "------"
    for(col in 1:length(assmt_per_unique_cs_de$dataElement)) {
      file_summary[assmt_per_unique_cs_de[col,1]] = assmt_per_unique_cs_de[col,2]
    }
    
    #Count of assessment id coversheet data element values;
    assmt_per_cs_de = sqldf::sqldf("select de_map.code as dataElement, count(d2.value) from d2 join de_map on de_map.id = d2.dataElement where de_map.code = 'SIMS.CS_ASMT_ID' group by d2.dataElement")
    file_summary["assessment count per cs data elements"] = "------"
    for(col in 1:length(assmt_per_cs_de$dataElement)) {
      file_summary[paste0((assmt_per_cs_de[col,1])," ")] = assmt_per_cs_de[col,2]
    }
    
   incProgress(1/14, detail = ("Checking Overlapping"))
    # identify overlapping assessments, and if any write out details
    overlapping_assessment <- sqldf::sqldf('select period, orgUnit, attributeOptionCombo, count(distinct(storedby)) as assessment_count from d group by period, orgUnit, attributeOptionCombo having count(distinct(storedby)) > 1')
    if(nrow(overlapping_assessment) != 0) {
      write.csv(overlapping_assessment,file=paste0(folder, filename, "_overlapping_assessment.csv"))
      overlapping_assessment_list <- sqldf::sqldf('select distinct d.period, d.orgUnit, d.attributeOptionCombo, d.storedby from d join overlapping_assessment o on d.period=o.period and d.orgUnit=o.orgUnit and d.attributeOptionCombo = o.attributeOptionCombo')
      write.csv(overlapping_assessment_list,file=paste0(folder, filename, "_overlapping_assessment_list.csv"))
      
      validation$overlappingassement <- overlapping_assessment
      validation$overlappingassementlist <- overlapping_assessment_list
      has_error<-TRUE
    }
    file_summary["overlapping PE/OU/IM count"] = length(overlapping_assessment$period)
    messages <- append(paste( length(overlapping_assessment$period), " overlapping PE/OU/IM count"), messages)
    
   incProgress(1/14, detail = ("Making Shifts"))
    # identify period shifts resulting from shifting assessments
    d_unique = sqldf::sqldf('select period, storedby from d group by period, storedby')
    d2_unique = sqldf::sqldf('select period, comment from d2 group by period, comment')
    shifts_made = sqldf::sqldf('select comment as assessment, d_unique.period as old_period, d2_unique.period as new_period from d_unique join d2_unique on d_unique.storedby = d2_unique.comment where d_unique.period != d2_unique.period order by old_period')
    if(nrow(shifts_made) != 0) {
      write.csv(shifts_made,file=paste0(folder, filename, "_shifts_made.csv"))
      validation$shifts_made <- shifts_made
      has_error<-TRUE
    }
    file_summary["shifted_assessment_count"] = nrow(shifts_made)
    messages <- append(paste(nrow(shifts_made), " shifted assessment count"), messages)
    
   incProgress(1/14, detail = ("Post Shift Duplicates"))
    # identify any exact duplicates after period shifting
    post_shift_duplicates <- getExactDuplicates(d2)
    post_shift_duplicates_w_code <- sqldf::sqldf('select de_map.code, post_shift_duplicates.* from  post_shift_duplicates left join de_map on de_map.id = post_shift_duplicates.dataElement order by dataElement, period, orgUnit, attributeOptionCombo')
    if(nrow(post_shift_duplicates_w_code) != 0) {
      write.csv(post_shift_duplicates_w_code,file=paste0(folder, filename, "_post_shift_duplicates.csv"))
      
      validation$postshiftduplicates <- post_shift_duplicates_w_code
      has_error<-TRUE
    }
    file_summary["post shift duplicate count"] = length(post_shift_duplicates_w_code$comment)
    messages <- append(paste(length(post_shift_duplicates_w_code$comment), " post shift duplicate count"), messages)
    
   incProgress(1/14, detail = ("Checking Invalid Period Mechanisms"))
    # 2. verify mechanism validity
    mechs <- checkMechanismValidity(d2)
    if(any(class(mechs) == "data.frame")){
      if(nrow(mechs) != 0){
        mech2 <- sqldf::sqldf("select mechs.*, m2.comment as assessment_id from mechs join (select distinct period, attributeOptionCombo, comment from d2) m2 on mechs.period = m2.period and mechs.attributeOptionCombo = m2.attributeOptionCombo")
        write.csv(mech2,file=paste0(folder, filename, "_mechs.csv"))
        validation$invalidperiodmechanisms <- mech2
        has_error<-TRUE
      }
      file_summary["invalid period mechanisms"] = length(mechs$attributeOptionCombo)
      messages <- append(paste(length(mechs$attributeOptionCombo), " invalid period mechanisms"), messages)
    } else {
      file_summary["invalid period mechanisms"] = 0
      messages <- append(paste(0, " invalid period mechanisms"), messages)
    }
    
   incProgress(1/14, detail = ("Checking Invalid data value types"))
    # 3. identify invalid data value types
    bad_data_values <- checkValueTypeCompliance2(d2)
    if(any(class(bad_data_values) == "data.frame")){
      if(nrow(bad_data_values) != 0){ 
        write.csv(bad_data_values,file=paste0(folder, filename, "_bad_data_values.csv"))
        validation$bad_data_values <- bad_data_values
        has_error<-TRUE
      }
      file_summary["bad data values"] = length(bad_data_values$dataElement)
      messages <- append(paste(length(bad_data_values$dataElement), " bad data values"), messages)
    } else {
      file_summary["bad data values"] = 0
      messages <- append(paste(0, " bad data values"), messages)
    }
    
   incProgress(1/14, detail = ("Checking Invalid Orgunits"))
    # 4. identify invalid orgunits
    invalid_orgunits <- checkDataElementOrgunitValidity(data=d2, datasets=dataSets, d2session = d2_default_session)
    if(any(class(invalid_orgunits) == "data.frame")){
      if(nrow(invalid_orgunits) > 0){
        #      print("Invalid data element/org unit pairs encountered. Printing out summaries.")
        #      write.csv(invalid_orgunits, paste0(folder, filename, '_invalid_de_ou.csv'), na="")
        
        invalidOUs <- sqldf::sqldf('select distinct orgUnit from invalid_orgunits')
        invalidOUAssessments <- sqldf::sqldf('select comment as assessment_id, period, orgUnit from d2 where orgunit in (select orgUnit from invalidOUs) group by comment, period, orgUnit')
        if(nrow(invalid_orgunits) != 0) {
          write.csv(invalid_orgunits,file=paste0(folder, filename, "_invalid_orgunits.csv"))
          write.csv(invalidOUAssessments,file=paste0(folder, filename, "_invalid_orgunit_list.csv"))
          
          validation$invalid_orgunits <- invalid_orgunits
          validation$invalidOUAssessments <- invalidOUAssessments
        }
        file_summary["invalid org units"] = length(invalidOUs$orgUnit)
        file_summary["invalid ou assessments"] = length(invalidOUAssessments$orgUnit)
        
        messages <- append(paste(length(invalidOUs$orgUnit), " invalid org units"), messages)
        messages <- append(paste(length(invalidOUAssessments$orgUnit) , " invalid ou assessements"), messages)
        
        has_error<-TRUE
      } else {
        file_summary["invalid org units"] = 0
        file_summary["invalid ou assessments"] = 0
        
        messages <- append(paste( 0, " invalid org units"), messages)
        messages <- append(paste(0, " invalid ou assessements"), messages)
      }
    } else {
      file_summary["invalid org units"] = 0
      file_summary["invalid ou assessments"] = 0
      
      messages <- append(paste(0, " invalid org units"), messages)
      messages <- append(paste(0, " invalid ou assessements"), messages)
    }
    
    #incomplete_assessments <- checkCoverSheetCompleteness(data_dictionary,path)
    # write out validation summary
    write.table(as.data.frame(file_summary), file = paste0(folder, filename, "_summary.txt"))
    validation$filesummary <- as.data.frame(file_summary)
    
    # write out normalized data - data has periods shifter for overlapping assessments, and has metadata in UID format. In case of any overlapping assessments in the input file, normalized file should be used for import into DATIM
    write.csv(d2[, c("dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value", "storedby", "timestamp", "comment")], paste0(folder, filename, "_normalized.csv"), row.names=FALSE, na="")
    
    normalized$normalizedfile<-d2[, c("dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value", "storedby", "timestamp", "comment")]
    bad_data_values_result <- NULL
    # to use in CEE validity check
    if(any(class(bad_data_values) == "data.frame")){
      if(nrow(bad_data_values) != 0){
          messages <- append(paste(NROW(bad_data_values), " Bad values found"), messages)
          
          #validation$baddatavalues <- bad_data_values
          bad_data_values_result <- bad_data_values
          has_error<-TRUE
      }
      else{
        bad_data_values_result <- NULL
        #messages<-append("No bad values found",messages)
      }
    } 
    
    has_error<-TRUE
    
    dcatch <-  tryCatch({
      #if dataElementIdScheme is id, construct map of data element ID and name
      de_map = vector(mode = "list")
      if(input$de_scheme %in% c("id")){
        data_elements <- read.csv(input$importdatafile$datapath, header =  input$header)
        distinct_dataElements <- data_elements[!duplicated(data_elements[,1]),]
        for(row in 1:length(distinct_dataElements[,1])) {
          url <- paste0(user_input$d2_session$base_url, "api/",
                        "dataElements/",distinct_dataElements[row,1],".json?fields=name")
          r <- httr::GET(url, httr::timeout(60), handle = user_input$d2_session$handle)
          r <- httr::content(r, "text")
          de <- jsonlite::fromJSON(r, flatten = TRUE)$name
          key <- paste0("",distinct_dataElements[row,1])
          if(is.null(de_map[[key]])){
            de_map[[key]] <- de
          }
        }
      }
      
      #CS check
     incProgress(1/14, detail = ("Checking Incomplete CS."))
      incomplete_CS <- SIMS4Validation::checkCoverSheetCompleteness(input$importdatafile$datapath,input$header,de_map,user_input$d2_session)
      if(!is.null(incomplete_CS) && nrow(incomplete_CS) != 0) {
        write.csv(incomplete_CS,file=paste0(output_dir, input$importdatafile$name, "_incomplete_CS.csv"))
        
        messages <- append(paste(NROW(incomplete_CS), " Incompete CS found"), messages)
        validation$incompletecs <- incomplete_CS
        has_error<-TRUE
      }
      else{
        messages<-append("0 Incomplete CS found",messages)
      }
      
      #Wrong Assessment check
     incProgress(1/14, detail = ("Checking Wrong Assessment Type."))
      wrongType <- SIMS4Validation::checkForWrongAssessmentType(input$importdatafile$datapath,input$header,de_map)
      if(!is.null(wrongType) && nrow(wrongType) != 0) {
        write.csv(wrongType,file=paste0(output_dir, input$importdatafile$name, "_wrongToolType.csv"))
        
        validation$wrongtypeassessments <- wrongType
        messages <- append(paste(NROW(wrongType), " Wrong Type Assessments found"), messages)
        has_error<-TRUE
      }
      else
      {
        messages<-append("0 Wrong Type Assessments found",messages)
      }
     
      #CEE validity check
     incProgress(1/14, detail = ("CEE Validity Check"))
      inValidCEE <- SIMS4Validation::checkForCEEValidity(input$importdatafile$datapath,input$header,de_map,bad_data_values_result)
      if(!is.null(inValidCEE) && nrow(inValidCEE) != 0) {
        write.csv(inValidCEE,file=paste0(output_dir, input$importdatafile$name, "_inValidCEE.csv"))
        
        validation$invalidcee <- inValidCEE
        messages <- append(paste(NROW(inValidCEE), " Invalid CEE found"), messages)
        has_error<-TRUE
      }
      else
      {
        messages<-append("0 Invalid CEE found",messages)
      }
      
      #end
    },
    error = function(e) {
      return(e)
    },
    warning = function(w) {
      list(paste("Escalated warning to error: ", conditionMessage(w)))
    })

      #Reset the button to force upload again
      shinyjs::reset("importdatafile")
      disableUI()
      
      if (inherits(dcatch, "list")) {
        messages <- append( "ERROR! : There were errors while parsing the file. Please check that you have provided the correct paramaters!", messages)
        messages <- append( d, messages)
        return(NULL)
        has_error<-TRUE
      }
      
    
    })
    if (has_error) {
      shinyjs::show("downloadDataValidation")
      shinyjs::show("downloadDataNormalized")
    }
    
    list(data=d,messages=messages,validation=validation,has_error=has_error)
  }
  
  validation_results <- reactive({ validate() })
  
  output$downloadDataValidation <- downloadHandler(
    filename = "sims_validation_results.xlsx",
    content = function(file) {
      
      vr_results <- validation_results() %>% purrr::pluck(.,"validation")
      openxlsx::write.xlsx(vr_results, file = file)
    }
  )
  
  output$downloadDataNormalized <- downloadHandler(
    filename = "sims_normalized_results.xlsx",
    content = function(file) {
      
      vr_results <- validation_results() %>% purrr::pluck(.,"normalized")
      openxlsx::write.xlsx(vr_results, file = file)
    }
  )
  
  
  output$messages <- renderUI({
    
    vr<-validation_results() 
    
    messages<-NULL
    
    if ( is.null(vr)) {
      return(NULL)
    }
    
    if ( inherits(vr,"error") ) {
      return( paste0("ERROR! ",vr$message) )
      
    } else {
      
      messages<-vr %>%   
        purrr::pluck(., "messages")
      
      if (!is.null(messages))  {
        lapply(messages, function(x)
          tags$li(x))
      } else
      {
        tags$li("No info")
      }
    }
  })
  
})

