#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/mortgage.R")

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   titlePanel("Mortgage vs. Property Tax"),
   plotOutput("distPlot"),
   hr(),
    fluidRow(
      column(4,
         numericInput("loan", "Mortgage Loan Amount", value = 300000, min=0, step=10000),
         numericInput("rate", "Mortgage Interest Rate", value = 4.0, min=0.0, step=0.2),
         radioButtons("mort_term", label=h4("Mortgage Term"),
                      selected = 30,
                      choices = list("15 year" = 15, "20 year" = 20, "30 year" = 30))
      ),
      column(4,
         numericInput("tax", "Annual Property Tax", value=8000, step=250),
         numericInput("tax_increase", "Property Tax Growth Rate", value=1.02, min=1.0, step=0.01),
         numericInput("extra", "Extra Plot Years", min=0, value=5, step=1)
      ),
      column(4,
         checkboxInput("do_deduct", "Apply Deduction To Interest, Tax", value = FALSE),
         numericInput("income_tax", "Income Tax Rate", value=0.3, min=0, step=0.05)
      )
   )
))

library(ggplot2)
# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
   output$distPlot <- renderPlot({
      tax_increase = as.numeric(input$tax_increase)
      mort_term = as.numeric(input$mort_term)
      mort_months = mort_term * 12
      loan = as.numeric(input$loan)
      rate = as.numeric(input$rate)
      extra = as.numeric(input$extra)
      mortgage(P=loan, I=rate, plotData=F)
      tax = as.numeric(input$tax)
      income_tax = ifelse(input$do_deduct, as.numeric(input$income_tax), 0)
      deduction = 1 - income_tax
      data <- data.frame(
        month = seq(1, (mort_term + extra) * 12),
        mort = c(aDFmonth$Monthly_Principal[1:mort_months] + deduction * aDFmonth$Monthly_Interest[1:mort_months], rep(0, extra * 12)),
        tax = deduction * rep(tax * (tax_increase ^ seq(0, mort_term + extra - 1)), each=12) / 12 
      )
      ggplot(data, aes(month)) +
        scale_y_continuous(limits = c(0, NA)) +
        geom_point(aes(y = mort, color = "Mortgage")) +
        geom_point(aes(y = tax, color = "Prop. Tax")) +
        scale_color_manual("", values = c("blue", "red")) +
        ylab("monthly payment ($)") + 
        xlab("month")
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

