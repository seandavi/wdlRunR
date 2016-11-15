library(shiny)
library(shinydashboard)
source('R/api.R')
options('cromwell_base'="http://104.196.210.249:8000")

pct = function(x) {sprintf('%4.2f %%',x*100)}

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),

      box(
        title = "",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    ),
    fluidRow(
        infoBoxOutput("completed"),
        infoBoxOutput('failed'),
        infoBoxOutput('running')
    ),
    fluidRow(
      box(DT::dataTableOutput("table1"))
    )
  )
)

server <- function(input, output, session) {
  stats = reactive({
    # Re-execute this reactive expression after 1000 milliseconds
    invalidateLater(10000, session)
    cromwellQuery()

    # Do something each time this is invalidated.
    # The isolate() makes this observer _not_ get invalidated and re-executed
    # when input$n changes.
    # print(paste("The value of input$n is", isolate(input$n)))
  })

  output$plot1 <- renderPlot({
      barplot(table(stats()$status))
  })
  output$table1 <- DT::renderDataTable({
    DT::datatable(stats())
  }) 
    output$completed <- renderInfoBox({
    infoBox(
      "Complete", pct(table(stats()$status)['Succeeded']/nrow(stats())), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green", fill = TRUE
    )
  })
    output$failed <- renderInfoBox({
    infoBox(
      "Failed", pct(table(stats()$status)['Failed']/nrow(stats())), icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red", fill = TRUE
    )
  })
    output$running <- renderInfoBox({
    infoBox(
      "running", pct(table(stats()$status)['Running']/nrow(stats())), icon = icon("cog"),
      color = "blue", fill = TRUE
    )
  })
}

shinyApp(ui, server)






