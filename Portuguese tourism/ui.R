library(dygraphs)
shinyUI(fluidPage(
  
  titlePanel("Predicted Nights stayed in Portugal"),
  helpText("The data were collected from Instituto Nacional De Estatistica."),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("region", label = h2("Regions"),
                   choices = list("Alentejo" = 1, "Algarve" = 2,"Area Metropolitana de Lisboa"=3,
                                  "Centro"=4,"Norte"=5 ,"Regiao Autonoma da Madeira"=6,"Regiao Autonoma dos Acores"=7),
                   selected = 1),
      radioButtons("radio", label = h3("Holt-Winters"),
                   choices = list("Additive" = 1, "Multiplicative" = 2),
                   selected = 1),
      numericInput("months", label = h4("Months to Predict"), 
                   value = 12, min =0, max =36, step =6),
      selectInput("interval", label = h4("Prediction Interval"),
                  choices = c("0.80", "0.90", "0.95", "0.99"),
                  selected = "0.95"),
      checkboxInput("showgrid", label = h4("Show Grid"), value = TRUE),
      hr(),
      helpText("Click and drag to zoom in (double click to zoom back out).")
    ),
    mainPanel( tabsetPanel(
      tabPanel("Display",plotOutput("plot")),
      tabPanel("Comparison Plot", dygraphOutput("dygraph1")),
      tabPanel("Holt-Winters Plot", dygraphOutput("dygraph3")),
      tabPanel("ARIMA Plot",dygraphOutput("dygraph2")),
      tabPanel("ARIMA Decomposition",plotOutput("decompose")),
      tabPanel("ARIMA Residuals",plotOutput("residuals")),
      tabPanel("Technical Analysis",h1("Model Analysis"),textOutput("text1"))
    ))
  )
))
