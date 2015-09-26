library(shiny)

# Define UI for applications that plots map of voter districts
shinyUI(pageWithSidebar(
    
    # headerPanel: App title
    headerPanel("Demo: Differences between simulated, observed and expected mean/variances"),
    
    # sidebarPanel: Sidebar for 
    # a) text input: lambda
    # b) sliders: number of exponentials, number of simulations
    sidebarPanel(
        uiOutput("distControls"),
        numericInput("lambda",
                     withMathJax("$$\\lambda$$"),
                     value = 0.1,
                     step = 0.1,
                     min = 0),
        sliderInput("n",
                    "Number of Observations",
                    min = 1, 
                    max = 1000,
                    value = 40),
        sliderInput("nsim",
                    "Number of Simulations",
                    min = 1, 
                    max = 1000,
                    value = 10)
    ),
    
    # mainPanel: Show a plot of the generated map
    mainPanel(
        h3(textOutput("dist")),
        h6(tableOutput("view_sim")),
        plotOutput("simPlot"),
        h6(tableOutput("view_largeN_obs")),
        plotOutput("obsPlot")
    )
))