library(shiny)
library(shinyalert)
library(tidyverse)
library(shinydashboard)
library(plotly)
library(DT)
library(scales)

sidebar <- dashboardSidebar(
  br(),
  br(),
  
  h4("Adjust parameters"),
  br(),
  
  sliderInput(inputId = "int",
              label =  h4("Interest Rate"),
              value = 3.5, min = 2, max = 15, step =  0.25),
  sliderInput(inputId = "loan",
              label =  h4("Loan Amount"),
              value = 100000, min = 50000, max = 200000, step = 5000),
  radioButtons(inputId = "term", label = h4("Term"),
               choices = c("15 Year" = 15, "30 Year" = 30), selected = 30)
  )


# B O D Y
body <- dashboardBody(
  useShinyalert(),
  
  fluidRow(
    infoBoxOutput("pmt", width = 4),
    infoBoxOutput("ln", width = 4),
    infoBoxOutput("intrate", width = 4)
  ),
  
  br(),
  
  box(
    headerPanel("Some Definitions"),
    br(),
    br(),
    h4("Monthly Payment  =  Amout the borrower pays monthly"),
    h4("Cummulative Payments  =  The sum of payments over time"),
    h4("Interest Portion  =  Interest Portion of the monthly payments"),
    h4("Cummulative Interest Payments  =  The sum the interest portion over time"),
    h4("Principal  =  Pricipal portion of the monthly payment"),
    h4("Cummulative Principal  =  Sum of the principal; also known as equity"),
    h4("Outstanding Balance  =  How you still owe on the loan")
  ),
  br(),
  # 
  # fluidRow(
  #   DT::dataTableOutput("table")
  # ),
  
  fluidRow(
    DT::dataTableOutput("table")
  )
)


ui <- dashboardPage(
  
  # H E A D E R
  dashboardHeader(title = "Loan Amortization"),
  
  # S I D E B A R
  sidebar,
  
  # B O D Y
  body
)

server <- function(input, output) {
  i <- reactive(input$int/12/100)
  i2 <- reactive(input$int)
  v <- reactive((1+i())^-1)
  n <- reactive(as.numeric(input$term)*12)
  l <- reactive(input$loan)
  
  pmt <- reactive({
    i()*l()/(1-v()^n()) %>% round(2)
  })
  
  #observe(print(pmt()))
  
   output$pmt <- renderInfoBox({
     infoBox(
       "",
       "Payment",
       h3(pmt()),
       icon = icon("signal"),
       color = "red",
       fill = FALSE)
   })
   
   output$ln <- renderInfoBox({
     infoBox(
       "",
       "Loan Amount",
       h3(l()),
       icon = icon("signal"),
       color = "red",
       fill = FALSE)
   })
   
   output$intrate <- renderInfoBox({
     infoBox(
       "",
       "Interest Rate",
       h3(i2()),
       icon = icon("signal"),
       color = "red",
       fill = FALSE)
   })
   
   vals <- reactiveValues(data=NULL)
   observe({
     period <- 1:n()
     
     intpmt <- c()
     prcpmt <- c()
     pv_dv_new <- l()
     for(k in period){
       int_k <- pv_dv_new[k]*i()
       intpmt <- c(intpmt, int_k)
       
       prc_k <- pmt() -  int_k
       prcpmt <- c(prcpmt, prc_k)
       
       pv_dv_k <- pv_dv_new[k] - prc_k
       pv_dv_new <- c(pv_dv_new, pv_dv_k)
     }
     
     amort <- tibble(payment = pmt(),
                     Cumm_Pmt = pmt()*period,
                     interest = intpmt,
                     Cum_Interest = cumsum(intpmt),
                     principal = prcpmt,
                     Cum_Principal = cumsum(principal),
                     balance = pv_dv_new[-1])
     
     vals$amort <- amort
   })
   
   mydatColNames <- c("Monthly Payment"="payment",
                      "Cummulative Payments"="Cumm_Pmt",
                      "Interest Portion"="interest",
                      "Cummulative Interest Payments"="Cum_Interest",
                      "Principal"="principal",
                      "Cummulative Principal"="Cum_Principal",
                      "Outstanding Balance"="balance")
   amort <- reactive({vals$amort})
   
   output$table <- DT::renderDataTable(
     amort() %>%
       DT::datatable(options = list(pageLength=100, scrollX=T),
                     escape = F,
                     colnames = mydatColNames) %>%
       formatCurrency(columns = c("Monthly Payment", "Cummulative Payments",
                                  "Interest Portion", "Cummulative Interest Payments",
                                  "Principal", "Cummulative Principal", "Outstanding Balance"),
                      currency = "$", digits = 2)
   )
}


shinyApp(ui = ui, server = server)

