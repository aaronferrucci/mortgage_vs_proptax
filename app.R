#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
# The function below takes inspiration from:
#   http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/mortgage.R
# I've simplified it a bit to my needs: made it less verbose, got rid of the evil <<-
# operator, removed the plotting option. Oh, and fixed a bug which added extra junk at the end
# of the mortgage term.
mortgage <- function(P=500000, I=6, L=30) {
  J <- I/(12 * 100)
  N <- 12 * L
  M <- P*J/(1-(1+J)^(-N))
  monthPay <- M
  # Calculate Amortization for each Month
  Pt <- P # current principal or amount of the loan
  currP <- NULL
  while(Pt >= 0.01) {
    H <- Pt * J # this is the current monthly interest
    C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
    Q <- Pt - C # this is the new balance of your principal of your loan
    if (Q < 0.01)
      Q <- 0
    Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
    currP <- c(currP, Pt)
  }
  monthP <- c(P, currP[1:(length(currP)-1)])-currP
  aDFmonth <- data.frame(
    Amortization=c(P, currP[1:(length(currP)-1)]),
    Monthly_Payment=monthP+c((monthPay-monthP)[1:(length(monthP)-1)],0),
    Monthly_Principal=monthP,
    Monthly_Interest=c((monthPay-monthP)[1:(length(monthP)-1)],0),
    Year=sort(rep(1:ceiling(N/12), 12))[1:length(monthP)]
  )
  return(aDFmonth)
}

ui <- shinyUI(fluidPage(
   fluidRow(
    column(12, h2("Mortgage vs. Property Tax", align="center"))
   ),
   plotOutput("distPlot"),
   hr(),
    fluidRow(
      column(4,
         h5("1) Enter the loan amount, interest rate and mortgage term."),
         hr(),
         numericInput("loan", "Mortgage Loan Amount", value = 300000, min=0, step=10000),
         numericInput("rate", "Mortgage Interest Rate", value = 4.25, min=0.0, step=0.25),
         radioButtons("mort_term", "Mortgage Term", selected = 30,
                      choices = list("15 year" = 15, "20 year" = 20, "30 year" = 30))
      ),
      column(4,
         h5("2) Enter the annual property tax payment, and a growth rate. In California, property tax can grow at most 2%/year."),
         h5("Property tax is plotted as a monthly payment, to put it in the same scale as the mortgage payment."),
         hr(),
         numericInput("tax", "Annual Property Tax", value=8000, step=250),
         numericInput("tax_increase", "Property Tax Growth Rate", value=1.02, min=1.0, step=0.01),
         numericInput("extra", "Extra Plot Years", min=0, value=5, step=1)
      ),
      column(4,
         h5("3) Enter your income tax rate and check the checkbox if you deduct interest and property tax payments."),
         hr(),
         checkboxInput("do_deduct", "Adjust For Deductions", value = FALSE),
         numericInput("income_tax", "Income Tax Rate", value=0.3, min=0, step=0.05)
      )
   ),
   fluidRow(
     hr(),
     column(12, 
            h5(a("Source code, github", href="https://github.com/aaronferrucci/mortgage_vs_proptax")),
            h5("mortgage function adapted from ", a("http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/mortgage.R", href="http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/mortgage.R"))
     )
   )
))

server <- shinyServer(function(input, output) {

   output$distPlot <- renderPlot({
      tax_increase <- as.numeric(input$tax_increase)
      mort_term <- as.numeric(input$mort_term)
      loan <- as.numeric(input$loan)
      rate <- as.numeric(input$rate)
      extra <- as.numeric(input$extra)

      df.month <- mortgage(P=loan, I=rate, L=mort_term)

      tax <- as.numeric(input$tax)
      income_tax <- ifelse(input$do_deduct, as.numeric(input$income_tax), 0)
      deduction <- 1 - income_tax
      data <- data.frame(
        month = seq(1, (mort_term + extra) * 12),
        mort = c(df.month$Monthly_Principal + deduction * df.month$Monthly_Interest, rep(0, extra * 12)),
        tax = deduction * rep(tax * (tax_increase ^ seq(0, mort_term + extra - 1)), each=12) / 12
      )
      ggplot(data, aes(month)) +
        scale_x_continuous(breaks=seq(0, (mort_term + extra) * 12, 60), labels=function(x) paste0(x, " (", x / 12, " years)")) +
        scale_y_continuous(limits = c(0, NA)) +
        geom_point(aes(y = mort, color = "Mortgage")) +
        geom_point(aes(y = tax, color = "Prop. Tax")) +
        scale_color_manual("", values = c("blue", "red")) +
        ylab("monthly payment ($)") +
        xlab("months")
   })
})

# Run the application
shinyApp(ui = ui, server = server)

