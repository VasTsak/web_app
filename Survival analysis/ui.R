library(shiny)
shinyUI(fluidPage(titlePanel("Survival Analysis"),
                  helpText("Input your data file below"),
                  
                  sidebarLayout(
                    sidebarPanel(
                      shinyjs::useShinyjs(),
                      fileInput("file", label = h3("File input"),accept = c(".xls")),
                      selectInput("select", label = h3("Select Dependent Variable"),
                                  choices = list( "Not specified"=0,"X1" = 1, "X2" = 2,"X3"=3,
                                                  "X4"=4,"X5"=5,"X6"=6 ,"X7"=7), 
                                  selected = 0),
                      checkboxGroupInput("region", label = h2("Select Variables"),
                                         choices = list("X1" = 1, "X2" = 2,"X3"=3,
                                                        "X4"=4,"X5"=5,"X6"=6 ,"X7"=7),
                                         selected = c(1,2,3,4,5,6,7))
                      
                    ),
                    mainPanel( tabsetPanel(
                      tabPanel("Contents",h1("Dataset"),tableOutput('contents')),
                      tabPanel("Head",h1("Head of dataset"),verbatimTextOutput("text1")),
                      tabPanel("Survival Analysis",verbatimTextOutput("text2")),
                      tabPanel("Summary",verbatimTextOutput("text3")),
                      tabPanel("Coxph Analysis",verbatimTextOutput("text4")),
                      tabPanel("Assumptions",verbatimTextOutput("text5")),
                      tabPanel("Hazard Ratio",verbatimTextOutput("text6")),
                      tabPanel("Frailty",verbatimTextOutput("text7")),
                      tabPanel("Conclusions",tableOutput("text9"))
                      
                      
                    ))
                  )))