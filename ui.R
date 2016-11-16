library(shiny)

shinyUI(fluidPage(
  
  sidebarLayout(position = "right",
                
    sidebarPanel(
      selectInput('dataType', label="Info to look at", choices=c('Number of Votes', 'Voter Turnout', 'Voter Age', 'Voter Race')),
      selectInput('graphType', label="Type of Graph", choices=c('Bar', 'Pie')),
      selectInput('stateName', label='Filter by State', choices=stateNames)
    ),
    mainPanel(
      plotOutput("graph", dblclick='onClick')
    )
  )
  
))
