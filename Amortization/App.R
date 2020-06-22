
library(shiny)
library(shinyalert)
library(tidyverse)
library(shinydashboard)
library(DT)

sidebar <- dashboardSidebar(
  br(),
  br(),
  
  h4("Use the sliders to tune the interest and the loan amount to derive the payment"),
  
  br(),
  br(),
  br(),
  
  sliderInput(inputId = "int",
              label =  h5("Interest Rate"),
              value = 3.5, min = 2, max = 5, step =  0.25),
  sliderInput(inputId = "loan",
              label =  h5("Loan Amount"),
              value = 100000, min = 50000, max = 200000, step = 5000),
  numericInput(inputId = "extra", label = "Enter Extra Payment", value = 0, step = 25),
  numericInput(inputId = "escro1", label = "Enter Escrow Estimate", value = 500, step = 25),
  radioButtons(inputId = "term", label = "Term", choices = c("15 Year" = 15, "30 Year" = 30), selected = 30)
  )


# B O D Y
body <- dashboardBody(
  useShinyalert(),
  
  fluidRow(
    infoBoxOutput("pmt", width = 3),
    infoBoxOutput("expmt", width = 3),
    infoBoxOutput("escro2", width = 3),
    infoBoxOutput("tpmt", width = 3)
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
  
  fluidRow(
    infoBoxOutput("ln", width = 4),
    infoBoxOutput("intrate", width = 4)
  ),
  
  br(),
  
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
  i <- reactive(input$int/1200)
  i2 <- reactive(input$int)
  v <- reactive((1+i())^-1)
  n <- reactive(as.numeric(input$term)*12)
  l <- reactive(input$loan)
  extra_pmt <- reactive(input$extra)
  escroPmt <- reactive(input$escro1)
  
  pmt <- reactive({round(i()*l()/(1-v()^n()),2)})
  
  totpmt <- reactive(pmt()+extra_pmt() + escroPmt())
  
  #observe(print(pmt()))
  
   output$pmt <- renderInfoBox({
     infoBox(
       "",
       "Loan Dues",
       h3(pmt()),
       icon = icon("thumbs-up", lib = "glyphicon"),
       color = "red",
       fill = F)
   })
   
   output$expmt <- renderInfoBox({
     infoBox(
       "",
       "Extra Payment",
       h3(extra_pmt()),
       icon = icon("thumbs-up"),
       color = "red",
       fill = F)
   })
   
   output$escro2 <- renderInfoBox({
     infoBox(
       "",
       "Escro Estimate",
       h3(escroPmt()),
       icon = icon("signal"),
       color = "red",
       fill = F)
   })
   
   output$tpmt <- renderInfoBox({
     infoBox(
       "",
       "Grand Total Due Monthly",
       h3(totpmt()),
       icon = icon("thumbs-up", lib = "glyphicon"),
       color = "red",
       fill = F)
   })
   
   output$ln <- renderInfoBox({
     infoBox(
       "",
       "Loan Amount",
       h3(l()),
       icon = icon("signal"),
       color = "blue",
       fill = FALSE)
   })
   
   output$intrate <- renderInfoBox({
     infoBox(
       "",
       "Interest Rate",
       h3(i2()),
       icon = icon("signal"),
       color = "blue",
       fill = FALSE)
   })
   
   vals <- reactiveValues(data=NULL)
   observe({
     period <- 1:n()
     
     intpmt <- c()
     prcpmt <- c()
     pv_dv_new <- l()
     for(k in period){
       int_k <- pv_dv_new[k]*i()  # INTEREST PAYMENT
       intpmt <- c(intpmt, int_k)
       
       prc_k <- pmt() -  int_k + extra_pmt() #PMT - INTEREST PMT + EXTRA PMT. EQUITY
       prcpmt <- c(prcpmt, prc_k)
       
       pv_dv_k <- pv_dv_new[k] - prc_k  # BALANCE ON THE LOAN
       pv_dv_new <- c(pv_dv_new, pv_dv_k)
     }
     
     amort <- tibble(payment = pmt(),
                     extraPmt = extra_pmt(),
                     Escrow = escroPmt(),
                     TotalPmt = payment + extraPmt + Escrow,
                     Cumm_Pmt = TotalPmt*period,
                     interest = intpmt,
                     Cum_Interest = cumsum(intpmt),
                     principal = TotalPmt - interest,
                     Cum_Principal = cumsum(principal),
                     balance = pv_dv_new[-1])
     
     vals$amort <- amort
   })
   
   yr <- reactive({rep(1:(n()/12), 12) %>% sort()})
   
   mydatColNames <- c("Monthly Payment"="payment",
                      "Extra Payment"="extraPmt",
                      "Total Payment"="TotalPmt",
                      "Cummulative Payments"="Cumm_Pmt",
                      "Interest Portion"="interest",
                      "Cummulative Interest Payments"="Cum_Interest",
                      "Principal"="principal",
                      "Cummulative Principal"="Cum_Principal",
                      "Outstanding Balance"="balance")
   amort <- reactive({vals$amort})
   
   output$table <- DT::renderDataTable(
     amort() %>%
       mutate(Year = yr()) %>% 
       select(Year, everything()) %>% 
       filter(interest >= 0) %>% 
       DT::datatable(options = list(pageLength=100, scrollX=T),
                     escape = F,
                     colnames = mydatColNames) %>%
       formatCurrency(columns = c("Monthly Payment","Total Payment","Cummulative Payments",
                                  "Interest Portion", "Cummulative Interest Payments",
                                  "Principal", "Cummulative Principal", "Outstanding Balance"),
                      currency = "$", digits = 2)
   )
}


shinyApp(ui = ui, server = server)

