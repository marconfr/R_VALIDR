#  .-.    __
# |   |  /\ \  ValidR
# |   |  \_\/      __        .-.
# |___|        __ /\ \      /:::\
# |:::|       / /\\_\/     /::::/
# |:::|       \/_/        / `-:/
# ':::'__   _____ _____  /    /
#     / /\ /     |:::::\ \   /
#     \/_/ \     |:::::/  `"`
#           `"""""""""`

# ================#
####  LIBRARY  ####
# ================#

library(shiny)
library(shinyvalidate)
library(readxl)
library(writexl)
library(DT)
library(tibble)
library(janitor)
library(reactable)
library(digest)

source("R/GLOBAL.R", local = TRUE)

# ===============#
####   UI    ####
# ===============#


ui <- fluidPage(

  tags$head(
    tags$style(HTML("
                    .shiny-output-error-validation {
                        color: red;
                    }
                    .shiny-titlepanel{
                      font: normal normal normal 32px / 30px 'montserrat', sans-serif;
                      color: #e41b13;
                        text-transform: none;
                      text-shadow:none                    
                    }
                    body {
                      background: #fff;
                      font-size: 18px;
                      color: #656565;
                      font-family: arial, sans-serif;
                      line-height: 28px;
                      width: 100%;
                      zoom:1
                    }
                    h1, .h1 {
                      font: normal normal normal 32px / 30px 'montserrat', sans-serif;
                      color: #e41b13;
                        text-transform: none;
                      text-shadow:none
                    }
                    h1, h2, h3, h4, h5, h6 {
                      clear: both;
                      margin: 14px 0;
                      text-rendering:optimizelegibility
                    }
                    p {
                    }
                    .tabbable > .nav > li[class=active] > a {
                    color: #e41b13;
                    }
                    .tabbable > .nav > li > a {
                    color: #656565;
                    }
    "))
  ),



  # Application title
 titlePanel(h1("ValidR – Assay")),

  ##### DISCLAIMER #####
 
  mainPanel(
    tabsetPanel(
      tabPanel("Disclaimer",
             HTML("
             <p>  [ValidR - Assay] is an application whose objective is to validate 
                  the accuracy and precision of a dosing method according to the recommendations, 
                  as described in the publication : Hubert, P.et al. 2004 <b> Harmonization of strategies 
                  for the validation of quantitative analytical procedures. 
                  A SFSTP proposal – Part I </b>. J Pharm Biomed Anal 36: 579-586.</p>
                  
              <p> All statistical calculations have been checked using the data 
                  and the results provided in the article : Hubert, P. et al.  2007. <b> Harmonization of 
                  strategies for the validation of quantitative analytical procedures. 
                  A SFSTP proposal - part III </b>. J Pharm Biomed Anal 45: 82-96.</p> 
                  
              <p> You should check for the presence or absence of a matrix effect with [ValidR - Matrix]
                  before choosing the appropriate validation method </p>"),
             ),
      
      ##### TEMPLATE ####
      
      tabPanel("Files & Instructions for use",
               HTML("<p> Data should be downloaded using a template xlsx file in which 
                 you should enter raw results obtained during your analysis</p>
                 <p>Use sliders to indicate the numbers of series, levels, replicates 
                 you have performed. You could use protocols described by Hubert, 
                 P. et al.  2007. Harmonization of strategies for the validation 
                 of quantitative analytical procedures. 
                  A SFSTP proposal - part III. J Pharm Biomed Anal 45: 82-96.</p>
                  <p> A run is the analysis of the calibration and validation standards 
                 performed on a given day by a single operator.</p><p>Concentration levels correspond to the theorical concentrations 
               obtained by weighing and diluting when preparing standards. 
                 Standards should contain the same amount of analytes in each 
                 replicate (e.g. by weighing exactly the same quantities of 
                 analytes).</p><p>Replicates correspond to the numbers of standards of the same 
                    levels analysed in a serie.</p><hr>
                    <h2> Series / days</h2>"),
               sliderInput("nSERIE", "Number of series", value = 3, min = 3, max = 10),
               HTML("<p>You should perform at least 3 series of manipulations in 
                    order to validate your analytical methods</b></br><hr>
                    <h2>Calibration standards</h2>"),
               sliderInput("nCAL_LEVEL", "Number of concentration levels", value = 2, min = 1, max = 10),
               HTML("<p>Please input the number of concentration levels used.</p>"),
               HTML("<p><ul> 
                 <li>For the determination of a single active substance at target concetration level, 
                 you could use a 1-point calibration range at target concentration</li>
                 <li>For the determination of a an active substance within a range of concentration, 
                 you could use a 2-point calibration range at 80% of the lowest and at 120% of the highest concentration</li>
                 <li>For the determination of a an active substance and unknown degradation products, 
                 you could use a 2-point calibration range at the lowest admitted 
                 limit for the degradation products* and at 120% of the target concentration of the active substance.</li>
                    </ul></p><p>* Degradation products are usually expressed as a percentage of 
                 the surface area produced by the active substance at the target 
                 concentration. It is important to note that for exact quantification 
                 of degradation products, their own assay methods should be validated</p>"),
              
               sliderInput("nCAL_REP", "Number of replicate by levels", value = 2, min = 2, max = 10),
               HTML("<p>Please input the number of replicate performed by levels within a single serie</p>
                    </br>
                    <hr>
                    <h2> Validation standards</h2>"),
               sliderInput("nVAL_LEVEL", "Number of concentration levels", value = 3, min = 1, max = 10),
               HTML("<p>Please input the number of concentration levels used</p><p/><ul>
                <li>For the determination of a single active substance at target concetration level, 
                 you could use a 3-point validation range at 80%, 100% and 120% of the target concentration</li>
                <li>For the determination of a an active substance within a range of concentration, 
                 you could use a 3-point validation range at 80% of the lowest concentration,
                 at mid range and at 120% of the highest concentration</li>
                <li>For the determination of a an active substance and unknown degradation products, 
                 you could use a 3-point validation range at the lowest admitted 
                 limit for the degradation products* at mid range and at 120% 
                 of the target concentration of the active substance.</li>
                </ul></p><p>* Degradation products are usually expressed as a percentage of 
                 the surface area produced by the active substance at the target 
                 concentration. It is important to note that for exact quantification 
                 of degradation products, their own assay methods should be validated</p>"),
               sliderInput("nVAL_REP", "Number of replicate by levels", value = 3, min = 3, max = 10),
               HTML("<p>Please input the number of replicate performed by levels</p>"),
               downloadButton("Template", label = "Generate template"),
               HTML("</br><hr>")
      ),
      
      ##### LOAD DATA ####
      
      tabPanel("Load data",
               HTML("
               <p> Please use one of the templates correctly filled with your results as follows:</p>
               <ul> 
                      <li>Column A: do not edit.</li>
                      <li>Column B: do not edit.</li>
                      <li>Column C: do not edit.</li>
                      <li>Column D: enter the numeric values of the theorical target concentration without units.</li>
                      <li>Column E: do not edit.</li>
                      <li>Column F: enter the numeric values of the real concentration tested without units 
                      (These concentrations may differ from the theoretical concentrations mentioned above depending on the actual masses weighed to make the standards) </li>
                      <li>Column G: enter the numeric raw values of the signal produced by the corresponding concentrations,without units.</li>
                    </ul>
               <p>There should be no unfilled data</p>"),
               HTML("<img src='assay_how_to.png' />"),
               fileInput(
                 inputId = "filedata",
                 label = "Upload data. Choose xlsx file",
                 accept = c(".xlsx")
               ),
               DT::DTOutput(outputId = "table_complete"),
               HTML("</br><hr>")
               ),
      
      ##### REPORT ####
      
      tabPanel("Generate report",
               HTML("<p>To generate a report, you must have uploaded the template 
                    correctly filled  with your data as described in the previous tab</p>
                    </br>
                    <p>In order to complete the report, please fill in the following fields:</p>"),
               HTML("<div style='border : 5px solid #FF0000'><b> The button to generate the report will only appear once valid data has been uploaded and required fields completed.</b></div>"),
               textInput("tiB","Beta tolerance value", placeholder = "e.g. 0.8 (= 80%)"),
               textInput("tiAL","Acceptation limits", placeholder = "e.g. 0.05 (= 5%)"),
               textInput("tiN","Name:", placeholder = "e.g. Doe"),
               textInput("tiFN", "First Name:",placeholder = "e.g. John"),
               textInput("tiAS","Active substance:", placeholder = "e.g. Hydrocortisone"),
               textInput("tiPP","Pharmaceutical preparations:", placeholder="e.g. Hydrocortisone suspension 2 mg/mL"),
               textInput("tiUUC", "Unit used for concentrations", placeholder = "e.g. mg/L"),
               uiOutput("generate_button"),
               HTML("</br><hr>")
      )

    ), style='width: 100%'
    )
)  

# ===============#
#### SERVER  ####
# ===============#

server <- function(input, output) {

  
  #####  TEMPLATE TEXT  #####

  output$textPROTOCOL <- renderUI({
  })

# CHANGER ICI LE TEMPLATE QUI DOIT MAINTENANT ETRE GENERE
  
  output$Template <- downloadHandler(
    filename = function() {
      "ASSAY_TEMPLATE.XLSX"
    },
    content = function(file) {
      write_xlsx(generate_template(input$nSERIE, input$nCAL_LEVEL, input$nCAL_REP, input$nVAL_LEVEL, input$nVAL_REP), file)
    }
  )

##### UPLOAD FILE  #####

  # The code below verifies the downloaded file and conditionally
  # displays the report generation panel

  dfUPLOAD <- reactive({
    if (is.null(input$filedata)) {
      return(NULL)
    }
    inFile <- input$filedata
    req(inFile)
    ext <- tools::file_ext(inFile$datapath)
    validate(need(ext == "XLSX" | ext == "xlsx", "Please upload a *.XLSX or *.xlsx file"))
    
    # Check the file here with check_upload in SCRIPT R
    dfTEMP <- read_excel(inFile$datapath)
    validate(need(check_upload(dfTEMP), "The file has been checked and it seems that there are some mistakes, please follow instructions"))
    return(dfTEMP)
  })

  output$generate_button <- renderUI({
    if(!is.null(dfUPLOAD()) & iv$is_valid() ){
      tagList(
        downloadButton("report", "Generate report")
      )
      
      
    } else{
      return(NULL)
    }
  })
  
  iv <- InputValidator$new()
  iv$add_rule("tiB", sv_between(0.8, 0.95, message_fmt = "A number between 0.8 and 0.95 (usually 0.8)"))
  iv$add_rule("tiAL", sv_between(left=0.01, right=0.2, message_fmt = "A number between 0.01 and 0.2 (usually 0.05 i.ex)"))
  iv$add_rule("tiN", sv_required())
  iv$add_rule("tiFN", sv_required())
  iv$add_rule("tiAS", sv_required())
  iv$add_rule("tiPP", sv_required())
  iv$add_rule("tiUUC", sv_required())
  iv$enable()
  

  
output$table_complete <- DT::renderDT(dfUPLOAD(), options = list(pageLength = 5, searching = FALSE))
# Things to upgrade -> make dfUPLOAD() editable / savable


##### REPORT GENERATOR #####

  output$report <- downloadHandler(
    filename = "REPORT_ASSAY.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "REPORT_ASSAY.Rmd")
      #tempScript <- file.path(tempdir(), "R/SCRIPT.R")
      file.copy("REPORT_ASSAY.Rmd", tempReport, overwrite = TRUE)
      #file.copy("SCRIPT.R", tempScript, overwrite = TRUE)

      # Set up parameters to pass to Rmd document dfUPLOADS needs ()
      params <- list(dfASSAY = dfUPLOAD(),
                     nBeta=input$tiB,
                     nACC_LIMIT = input$tiAL,
                     name = input$tiN,
                     firstname=input$tiFN,
                     substance=input$tiAS,
                     pharmprep=input$tiPP,
                     concunit=input$tiUUC,
                     signalunit=input$tiUUS)
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      
      rmarkdown::render(tempReport, #<- else put tempReport directory
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
