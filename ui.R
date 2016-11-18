library(shiny)

shinyUI(fluidPage(
  
  navbarPage("Voter Information from 2012", id = 'tabs',
      tabPanel("Number of Votes", plotOutput('numOfVotes')),    
      tabPanel("Turnout", plotOutput('turnout', dblclick='onClick')),
      tabPanel("Age", plotOutput('age')),
      tabPanel("Race", plotOutput('race'))
  ),
  
  sidebarLayout(position = "left",
                
    sidebarPanel(
      # selectInput('dataType', label="Info to look at", choices=c('Number of Votes', 'Voter Turnout', 'Voter Age', 'Voter Race')),
      selectInput('graphType', label="Type of Graph", choices=c('Bar', 'Pie')),
      selectInput('stateName', label='Filter by State', choices=c('Total','AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY'))
    ),
    mainPanel(
    )
  )
  
))
