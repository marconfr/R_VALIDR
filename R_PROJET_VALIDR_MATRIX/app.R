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
library(readxl)
library(writexl)
library(DT)
library(tibble)
library(janitor)
library(reactable)
library(digest)


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
                        margin:14px 0
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
 titlePanel(h1("ValidR – Matrix effect testing")),

  ##### DISCLAIMER #####
 
  mainPanel(
    tabsetPanel(
      tabPanel("Disclaimer",
             p("[ValidR – Matrix] is an application whose objective is to test 
               whether the matrix of a drug to be assayed is likely to influence 
               the response of the analyte"),
             p("When a drug is assayed it is possible that the excipients or 
               extraction solvents (e.g. ointment assay) influence the signal 
               produced by the analyte compared to the signal produced by simply 
               dissolving the analyte in a analytic solvent (e.g. methanol)."),
             p("For instance: the signal produced by the analyte is likely to be 
               influenced by the other products around when analyzed by mass 
               spectrometry (signal suppression or enhancement) or by UV-visible 
               spetrometry."),
             p("In order to ensure that the analyte as assayed in the medicinal 
               product has a signal equivalent to that produced by a calibration 
               standard at the same concentrations, it should be ensured that 
               there are no significant differences between the two signals 
               produced."),
             p("For this purpose, two sets of standards (i.e. with exactly known 
               quantities) can be made and compared, one in the pharmaceutical
               matrix, the other in a solvent. If pre-analytical extraction are 
               needed, they should be performed as intended for the analysis of 
               the medicinal products"),
             
             p("For now, [ValidR – Matrix]  only works for assay methods where 
               there is a linear relationship between the amount of analyte and 
               the signal."),
             tags$hr()
      ),
      
      ##### TEMPLATE ####
      
      tabPanel("Files & Instructions for use",
             p("In order to enable statistical calculations you must use a template file corresponding to the protocol you want to perform."),
             selectInput(inputId = "siTEMPLATE", label = "Choice of protocol", choices = c("Active substance alone", "Active substance and its degradation products")),
             htmlOutput("textPROTOCOL"),
             downloadButton("Template", "Download Template"),
             p(""),
             tags$hr()
      ),
      
      ##### LOAD DATA ####
      
      tabPanel("Load data",
               p("Please use one of the templates correctly filled with your results as follows:"),               HTML("<ul> 
                      <li>Column A: do not edit.</li>
                      <li>Column B: do not edit.</li>
                      <li>Column C: do not edit.</li>
                      <li>Column D: do not edit.</li>
                      <li>Column E: enter the numeric values of the concentration tested without units.</li>
                      <li>Column F: enter the numeric raw values of the signal produced by the corresponding concentrations,without units.</li>
                    </ul>"),
               p("There should be no unfilled data"),
               img(src="complete.png"),
               fileInput(
                 inputId = "filedata",
                 label = "Upload data. Choose xlsx file",
                 accept = c(".xlsx")
               ),
               DT::DTOutput(outputId = "table_complete"),
               p(""),
               tags$hr()
               ),
      
      ##### REPORT ####
      
      tabPanel("Generate report",
               p("To generate a report, you must have uploaded the template correctly filled  with your data as described in the previous tab"),
               uiOutput("generate_button"),
               tags$hr()
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
    if (input$siTEMPLATE == "Active substance alone") {
      HTML("<p>This protocol allows you to test the matrix effect when you want to
            measure the active substance at a target level (the concentration in the medicine) </p>
            <p>To do this, you must carry out a 3-point calibration range at </p><ul><li>80%;</li><li>100%;</li>
            <li> 120% of the content;</li></ul>
            <p> In the absence and presence of the matrix in duplicate and on 2 different days (i.e. 24 samples)</p>
            <p> In case you want to develop a dosing method for a concentration range (e.g. 1 to 10 mg/L) you can perform a 3-point calibration with 80% of the lowest value, one point in the middle of the range, and one point at 120% of the highest value.</p>
            <img src='fig1.png' alt='missing image'>
            <p> </p><br />")
    } else {
      HTML("<p>This protocol allows you to test the matrix effect when you want to
            measure the active substance and its degradation products</p>
            <p>To do this, you must carry out a 5-point calibration range base on the analyte at </p>
            <ul>
                 <li>the limit of detection (LOD*),</li>
                 <li>limit of quantification (LOQ*),</li>
                 <li>3 x LOQ</li>
                 <li>at a mid-point of the ranre</li>
                 <li>at the high point of the range</li>
            </ul>
            <p>in the absence and presence of the matrix in
            duplicate and on 2 different days (i.e. 40 samples)</p><br />
            <img src='fig2.png' alt='missing image'>
           <p>*LOD and LOQ could be determined on preliminary studies based on ICH Q2 definitions</p><br />
           ")
    }
  })


  output$Template <- downloadHandler(
    filename = function() {
      "MATRIX_TEMPLATE.XLSX"
    },
    content = function(file) {
      write_xlsx(generate_template(input$siTEMPLATE), file)
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
    if(!is.null(dfUPLOAD())){
      tagList(
        p("In order to complete the report, please fill in the following fields:"),
        textInput("tiN","Name:", placeholder = "e.g. Doe"),
        textInput("tiSN", "First Name:",placeholder = "e.g. John"),
        textInput("tiAS","Active substance:", placeholder = "e.g. Hydrocortisone"),
        textInput("tiPP","Pharmaceutical preparations:", placeholder="e.g. Hydrocortisone suspension 2 mg/mL"),
        textInput("tiUUC", "Unit used for concentrations", placeholder = "e.g. mg/L"),
        textInput("tiUUS", "Unit used for signal", placeholder = "e.g. AU"),
        downloadButton("report", "Generate report")
      )
    } else{
      return(NULL)
    }
  })

  
output$table_complete <- DT::renderDT(dfUPLOAD(), options = list(pageLength = 5, searching = FALSE))
# Things to upgrade -> make dfUPLOAD() editable / savable


##### REPORT GENERATOR #####

  output$report <- downloadHandler(
    filename = "REPORT_MATRIX_EFFECT.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "REPORT_MATRIX.Rmd")
      file.copy("REPORT_MATRIX.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document dfUPLOADS needs ()
      params <- list(dfMATRIX = dfUPLOAD())

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)