#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("mlp_outcomes.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("MLP Report"),
   
   # Sidebar with a file input 
   sidebarLayout(
      sidebarPanel(
        fileInput(
          'fileMLPCases', 
          'Please upload a CSV of MLP Cases.  This can be downloaded from the MLP Cases report on LS.  Save as CSV.'
        ),
        
        
        fileInput(
          'fileMLPOutreaches', 
          'Please upload a CSV of MLP Outreaches.  This can be downloaded from the MLP Outreaches report on LS.  Save as CSV.'
        ),

        tags$a(href="https://clsphila.legalserver.org/report/dynamic?load=568", target="blank", "MLP Case Report"),
        tags$br(),
        tags$a(href="https://clsphila.legalserver.org/report/dynamic?load=616", target="blank", "MLP Outreaches Report")
        
      ),
      # Show something
      mainPanel(
        tags$h1("Rising Sun Health Center Medical Legal Partnership Report"),
         tags$h2("Cases"),
         textOutput(outputId="cases1"),
         textOutput(outputId="cases2"),
         textOutput(outputId="cases3"),
        
         tags$h2("Consultations"),
         textOutput(outputId="consults1"),
         textOutput(outputId="consults2"),
         textOutput(outputId="consults3"),
         
         tags$h2("Trainings"),
         textOutput(outputId="training1"),
         textOutput(outputId="training2"),
         textOutput(outputId="training3"),
         
         tags$h2("Languages"),
         textOutput(outputId="language1"),
         textOutput(outputId="language2"),
         textOutput(outputId="language3"),
         plotOutput(outputId="language4"),
         
         tags$h2("Geography"),
         textOutput(outputId="geo1"),
         plotOutput(outputId="geo2"),
         
         tags$h2("Gender"),
         textOutput(outputId="gender1"),
         textOutput(outputId="gender2"),
         
         tags$h2("Children"),
         textOutput(outputId="children1"),
         textOutput(outputId="children2"),
         
         tags$h2("Types of Cases"),
         textOutput(outputId="casetype1"),
         textOutput(outputId="casetype2"),
         textOutput(outputId="casetype3"),
         textOutput(outputId="casetype4"),
         textOutput(outputId="casetype5"),
         
         tags$h2("Types of Case Outcomes"),
         textOutput(outputId="outcomes1"),
         textOutput(outputId="outcomes2"),
         textOutput(outputId="outcomes3"),
         textOutput(outputId="outcomes4"),
         
         tags$h2("Types of Brief Consultations"),
         textOutput(outputId="consults4"),
         textOutput(outputId="consults5"),
         textOutput(outputId="consults6"),
         textOutput(outputId="consults7")
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
   
  casesFile <- reactive({
    if (is.null(input$fileMLPCases)) {
      return(NULL)
    } else {
      input$fileMLPCases
    }
  })

  
  outreachesFile <- reactive({
    if (is.null(input$fileMLPOutreaches)) {
      return(NULL)
    } else {
      input$fileMLPOutreaches
    }
  })
  
  
  caseStats <- reactive({
    if (is.null(casesFile()) || is.null(outreachesFile())) {
      return(NULL)
    } else {
      mlpCases <- readCasesFile(casesFile()$datapath)
      getCaseStats(mlpCases)
      
            # mlpOutreaches <- readOutreachesFile(mlpcases)
      # outreachStats <- getOutreachStats(mlpOutreaches)
      
    }
  })
  
  outreachStats <- reactive({
    if (is.null(casesFile()) || is.null(outreachesFile())) {
      return(NULL)
    } else {
      mlpOutreaches <- readOutreachesFile(outreachesFile()$datapath)
      getOutreachStats(mlpOutreaches)
      
    }
  })
  
  output$cases1 <- renderText({
      if (is.null(caseStats())) return(NULL)
      paste(caseStats()$totalCases, "cases opened since ___.")
   })
  
  output$cases2 <- renderText({
    if (is.null(caseStats())) return(NULL)
    paste(caseStats()$HealthInsServCases, "cases involving securing health insurance or health servives.")
  })

  output$cases3 <- renderText({ 
    if (is.null(caseStats())) return(NULL)
    paste(caseStats()$TotalTime, "hours spent working on these", caseStats()$totalCases, "cases.")
  }) 
  
  output$consults1 <- renderText({ 
    if (is.null(outreachStats())) return(NULL)
    paste(outreachStats()$totalOutreaches, "total consultations.")
  }) 
  
  
  output$consults2 <- renderText({ 
    if (is.null(outreachStats())) return(NULL)
    paste(outreachStats()$mlpOutreachesTime, "hours spent working on these", outreachStats()$totalOutreaches, "consultations.")
  }) 
  
  
  output$consults3 <- renderText({ 
    if (is.null(outreachStats())) return(NULL)
    paste(outreachStats()$totalMedicalHealthOutreaches, "consultations about Medical Assistance/Health Insurance.")
  }) 
  
  output$training1 <- renderText({ 
    if (is.null(outreachStats())) return(NULL)
    paste(outreachStats()$totalTrainings, "trainings provided to PHMC staff.")
  }) 
  
  output$training2 <- renderText({ 
    if (is.null(outreachStats())) return(NULL)
    paste(outreachStats()$totalTrainingTime, "hours spent in these trainings.")
  }) 
  
  
  output$training3 <- renderText({ 
    if (is.null(outreachStats())) return(NULL)
    paste(outreachStats()$totalTrainingAttendees, "attendees at these trainings.")
  }) 
  
  
  output$language1 <- renderText({
    if (is.null(caseStats())) return(NULL)
    paste(caseStats()$languageEnglish, "total english speaking clients served.")
  })
  
  output$language2 <- renderText({
    if (is.null(caseStats())) return(NULL)
    paste(caseStats()$languageNonEnglish, "total non-english speaking clients served.")
  })
  
  output$language3 <- renderText({
    if (is.null(caseStats())) return(NULL)
    paste(caseStats()$languageTotal, "different languages served.")
  })
  
  output$language4 <- renderPlot({
    caseStats()$languageGraph
  })
  
  output$geo1 <- renderText({
    if (is.null(caseStats())) return(NULL)
    paste(caseStats()$zipTotal, "different zip codes served in Philly.")
  })
  
  output$geo2 <- renderPlot({
    caseStats()$zipGraph
  })
  
  output$gender1 <- renderText({
    if (is.null(caseStats())) return(NULL)
    paste(caseStats()$GenderFemale, "female clients served.")
  })
  
  output$gender2 <- renderText({
    if (is.null(caseStats())) return(NULL)
    paste(caseStats()$GenderMale, "male clients served.")
  })
  
  output$children1 <- renderText({
    if (is.null(caseStats())) return(NULL)
    paste(caseStats()$ChildrenCaseTotal, "cases with children under the age of 18")
  })
  
  output$children2 <- renderText({
    if (is.null(caseStats())) return(NULL)
    paste(caseStats()$ChildrenTotalChildrenServed, "total children under 18 in our clients' households.")
  })
  
  
  output$casetype1 <- renderText({
    if (is.null(caseStats())) return(NULL)
    paste(caseStats()$typeHealthInsurance, "Health Insurance Cases")
  })
  
  output$casetype2 <- renderText({
    if (is.null(caseStats())) return(NULL)
    paste(caseStats()$typeHealthServices, "Health Services Cases")
  })
  
  output$casetype3 <- renderText({
    if (is.null(caseStats())) return(NULL)
    paste(caseStats()$typeOtherPB, "Other PUblic Benefits Cases")
  })
  
  output$casetype4 <- renderText({
    if (is.null(caseStats())) return(NULL)
    paste(caseStats()$typeUtilities, "Utilities Cases")
  })
  
  output$casetype5 <- renderText({
    if (is.null(caseStats())) return(NULL)
    paste(caseStats()$typeImmigration, "Immigration/Naturalization Cases")
  })
  
  output$outcome1 <- renderText({
    if (is.null(caseStats())) return(NULL)
    paste(caseStats()$outcomesMedical, "Outcomes: Medical Issues")
  })
  
  output$outcome2 <- renderText({
    if (is.null(caseStats())) return(NULL)
    paste(caseStats()$outcomesOtherPB, "Outcomes: Other Public Benefits")
  })
  
  output$outcome3 <- renderText({
    if (is.null(caseStats())) return(NULL)
    paste(caseStats()$outcomesUtilities, "Outcomes: Utilities")
  })
  
  output$outcome4 <- renderText({
    if (is.null(caseStats())) return(NULL)
    paste(caseStats()$outcomesOtherFinancial, "Outcomes: Other Financial Benefits")
  })
  
  output$consults4 <- renderText({ 
    if (is.null(outreachStats())) return(NULL)
    paste(outreachStats()$totalMedicalHealthOutreaches, "Consults: Medical Assistance/Health Insurance.")
  }) 

  output$consults5 <- renderText({ 
    if (is.null(outreachStats())) return(NULL)
    paste(outreachStats()$totalHousingOutreaches, "Consults: Housing.")
  }) 
  output$consults6 <- renderText({ 
    if (is.null(outreachStats())) return(NULL)
    paste(outreachStats()$totalSSIOutreaches, "Consults: Social Security/SSI")
  }) 
  output$consults7 <- renderText({ 
    if (is.null(outreachStats())) return(NULL)
    paste(outreachStats()$totalUtilitiesOutreaches, "Consults: Utilities")
  }) 

  
}

# Run the application 
shinyApp(ui = ui, server = server)

