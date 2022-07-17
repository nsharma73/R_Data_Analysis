#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# The data set high value ppp loans is required to run this app:
 # high_ppp=
 #   ppp_loans%>%
 #   filter(CurrentApprovalAmount>2000000)%>%
 #   filter(JobsReported>100)%>%
 #   select(BorrowerName,CurrentApprovalAmount,JobsReported,
 #          Industry,BusinessType,BusinessAgeDescription)


library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)


ui <- dashboardPage(
  
  dashboardHeader(title="PPP Loans Box Plots"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("loans", tabName = "loans", icon = icon("tree")),
      menuItem("hvloans", tabName = "hvloans", icon = icon("tree")),
      menuItem("hvboxplot", tabName = "hvboxplot", icon = icon("tree"))
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
            
    ),
    tabItem("hvboxplot",
            
            sliderInput("range", label = h4("High Value Loan Amount Range"), 
                        min = 2000000, 
                        max = 10000000, 
                        value = c(2000000, 8000000)
                        
            ),
            p("High Value is defined as loans greater than USD 2M"),
            p("The box plots are sorted by median loan amount"),
            box(plotOutput("box_plot2"), widith = 8),
            box(plotOutput("box_plot3"), widith = 8)
    )
    )
  
    

  )
)


server <- function(input,output){
  output$box_plot <- renderPlot({
    
    p <- ggplot(ppp_loans, aes(x = reorder(ppp_loans[[input$features]],-CurrentApprovalAmount), 
                               y = CurrentApprovalAmount)) +
      geom_boxplot() + 
      scale_y_continuous(trans='log10', name="Loans in USD (log10 scale)", labels = label_number(suffix = " M", scale = 1e-6)) +
      labs(y="Loan Amount", x="Business/Industry Type",
           subtitle="Boxplots: Descending by Loan Amount") +
      coord_flip()
    p
    
  })
  
  output$box_plot2 <- renderPlot({
    
    high_ppp%>%
      filter(CurrentApprovalAmount > input$range[1] & 
               CurrentApprovalAmount < input$range[2])%>%
      #reorder(x,y)
      ggplot(., aes(x = reorder(Industry,CurrentApprovalAmount, FUN = median), 
                    y = CurrentApprovalAmount)) +
      geom_boxplot() + 
      scale_y_continuous(trans='log10', name="Loans in USD (log10 scale)", 
                         labels = label_number(suffix = " M", scale = 1e-6)) +
      labs(y="Loan Amount", x="Industry Type",
           subtitle="Boxplots: Descending by Loan Amount") +
      coord_flip()
    
  })
  
  output$box_plot3 <- renderPlot({
    
    high_ppp%>%
      filter(CurrentApprovalAmount > input$range[1] & 
               CurrentApprovalAmount < input$range[2])%>%
      #reorder(x,y)
      ggplot(., aes(x = reorder(BusinessType,CurrentApprovalAmount, FUN = median), 
                    y = CurrentApprovalAmount)) +
      geom_boxplot() + 
      scale_y_continuous(trans='log10', name="Loans in USD (log10 scale)", 
                         labels = label_number(suffix = " M", scale = 1e-6)) +
      labs(y="Loan Amount", x="Industry Type",
           subtitle="Boxplots: Descending by Loan Amount") +
      coord_flip()
    
  })
  
  
  output$high_ppp <- renderDataTable(high_ppp)
}

shinyApp(ui,server)