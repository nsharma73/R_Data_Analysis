#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)


ui <- dashboardPage(
  dashboardHeader(title="PPP Loans Box Plots"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("loans", tabName = "loans", icon = icon("tree")),
      menuItem("hvloans", tabName = "hvloans", icon = icon("tree"))
    )
    
  ),
  dashboardBody(
    tabItems(
    tabItem("loans",
            box(plotOutput("box_plot"), widith = 8),
            box(
              selectInput("features","Features:",
                          c("BusinessType","Industry")), width=4
              
            )
            ),
    tabItem("hvloans",
            fluidPage(
              h2("High Value Loans > USD 2M and > 100 Jobs"),  
              dataTableOutput("high_ppp")
            )
            
    )
    )
  
    

  )
)


server <- function(input,output){
  output$box_plot <- renderPlot({
    
    p <- ggplot(ppp_loans, aes(x=ppp_loans[[input$features]], y=CurrentApprovalAmount)) +
      geom_boxplot() + 
      scale_y_continuous(trans='log10', name="Loans in USD (log10 scale)", labels = label_number(suffix = " M", scale = 1e-6)) +
      coord_flip()
    p
    
  })
  output$high_ppp <- renderDataTable(high_ppp)
}

shinyApp(ui,server)