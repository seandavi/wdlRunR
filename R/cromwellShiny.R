library(shiny)
library(shinydashboard)
library(jsonlite)
library(htmlOutput)


pct = function(x) {sprintf('%4.2f %%',x*100)}

ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Jobs", tabName = "jobs", icon = icon("th")),
            menuItem("Settings", tabName = "jobs", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName="dashboard",
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
                )
            ),
            tabItem(
                tabName="jobs",
                fluidRow(
                    DT::dataTableOutput("table1")
                ),
                fluidRow(
                    verbatimTextOutput('rows_out')
                )
            )
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
    stats1 = cromwellQuery()

    output$plot1 <- renderPlot({
        barplot(table(stats()$status))
    })
    output$table1 <- DT::renderDataTable({
        DT::datatable(stats1,selection = "single")
    })

    output$rows_out <- renderText({
        if(length(input$table1_cell_clicked)<3) {
            return("click a row to display details")
        }
        toJSON(unclass(cromwellMetadata(as.character(stats1[input$table1_cell_clicked[[1]],'id']))),pretty=TRUE)
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








