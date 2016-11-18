library(shiny)

shinyServer(function(input, output, session) {
  
  options(scipen=5) # Remove scientific notation from graphs
  
  # Lables
  R <- "Romney..R."
  D <- "Obama..D."
  O <- "All.Others"
  T <- "Total.Votes"
  E <- "Eligible.Voters"
  
  # Constants
  stateNames <- c('Total','AL','AK','AZ','AR','CA','CO','CT','DE','DC','FL','GA','HI','ID','IL','IN','IA','KS','KY','LA','ME','MD','MA','MI','MN','MS','MO','MT','NE','NV','NH','NJ','NM','NY','NC','ND','OH','OK','OR','PA','RI','SC','SD','TN','TX','UT','VT','VA','WA','WV','WI','WY')
  ageNames <- c('18-29', '30-44', '45-64', '65 & Up')
  raceNames <- c('White', 'Black', 'Hispanic', 'Asian', 'Other')
  policiesNames <- c('Economics', 'Budget Deficit', 'Foreign Policy', 'Health Care')
  sexNames <- c('Male', 'Female')
  
  # File Input
  popular <- as.data.frame(read.csv(file="Popular.csv",header=TRUE,sep=",",stringsAsFactors=FALSE))
  age <- as.data.frame(read.csv(file="Age.csv",header=TRUE,sep=",",stringsAsFactors=FALSE))
  race <- as.data.frame(read.csv(file="Race.csv",header=TRUE,sep=",",stringsAsFactors=FALSE))
  policies <- as.data.frame(read.csv(file="Policies.csv",header=TRUE,sep=",",stringsAsFactors=FALSE))
  sex <- as.data.frame(read.csv(file="Sex.csv",header=TRUE,sep=",",stringsAsFactors=FALSE))
  
  # Rename Rows
  rownames(popular) <- stateNames
  rownames(age) <- ageNames
  rownames(race) <- raceNames
  rownames(policies) <- policiesNames
  rownames(sex) <- sexNames
  
  # Create new Data.Frames
  byState <- as.data.frame(t(popular))
  
  # Reset selectInputs based on Tab
  observe({
    z <- input$tabs
    
    if (z == 'Turnout' || z == 'Number of Votes') {
      updateSelectInput(session, 'stateName', label='Filter by State', choices=stateNames)
      updateSelectInput(session, 'graphType', choices=c('Bar', 'Pie'))
    } else if (z == 'Age' || z == 'Race') {
       updateSelectInput(session, 'graphType', choices='Bar')
       updateSelectInput(session, 'stateName', label='Filter by Candidate', choices=c('All','Obama','Romney'))
    }
  })
  
  # Graphs
  pieNumVotes <- reactive({
    slices <- subset(popular, States==input$stateName)
    slices <- c(as.numeric(slices[D]), as.numeric(slices[R]), as.numeric(slices[O])) 
    
    lbls <- c("B. Obama", "M. Romney", "Other")
    pct <- round(as.numeric(unlist(slices)) / sum(as.numeric(unlist(slices)))*100)
    
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # add '%' to labels 
    pie(slices,labels = lbls, col=c('blue', 'red', 'grey'),
        main="Pie Chart by State")
  })
  
  pieVoterTurnout <- reactive({
    slices <- subset(popular, States==input$stateName)
    slices <- c(as.numeric(slices[T]), as.numeric(slices[E] - slices[T])) 
    
    lbls <- c("Voters", "Non-Voters")
    pct <- round(as.numeric(unlist(slices)) / sum(as.numeric(unlist(slices)))*100)
    
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # add '%' to labels 
    pie(slices,labels = lbls, col=c('khaki1', 'grey80'),
        main="Voter Turnout")
  })
  
  # NOTE condense these into one method
  barNumVotes <- reactive({
    input$dataType
    bars <- subset(popular, States==input$stateName)
    currBars <- c(as.numeric(bars[D]), as.numeric(bars[R]), as.numeric(bars[O])) 
    
    pct <- round(as.numeric(currBars) / as.numeric(bars[T])*100)
    lbls <- c("Obama", "Romney", "Other")
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # add '%' to labels
    
    barplot(currBars, main="Number of Votes per Candidate",
            xlab="Presidential Candidate", ylab="Votes", col=c("blue","red", "grey"),
            names.arg = lbls, ylim=c(0,max(currBars) + 10000))
  })
  
  barVoterTurnout <- reactive({
    bars <- subset(popular, States==input$stateName)
    currBars <- c(as.numeric(bars[T]), as.numeric(bars[E] - bars[T]))
    
    pct <- round(as.numeric(currBars) / as.numeric(bars[E])*100)
    lbls <- c("Voters", "Non-Voters")
    lbls <- paste(lbls, pct) # add percents to labels 
    lbls <- paste(lbls,"%",sep="") # add '%' to labels
    
    barplot(currBars, main="Voter Turnout", ylab="Eligible Voters", ylim=c(0,max(currBars) + 10000),
            col=c("khaki1", "grey80"), names.arg = lbls)
  })
  
  barAge <- reactive({
    bars <- c(unlist(age[2]), unlist(age[3]), unlist(age[4]))
    
    lbls <- c("Obama", "Romney", "Voter Turnout")
    
    barplot(as.matrix(age[,-1]), main=input$dataType, ylab="Percent",
            col=c("goldenrod1", "darkorange", 'brown3','coral4'), names.arg = lbls, legend.text=ageNames,
            beside=TRUE)
  })
  
  barRace <- reactive({
    x <- input$dataType
    
    bars <- c(unlist(race[2]), unlist(race[3]), unlist(race[4]))
    
    lbls <- c("Obama", "Romney", "Voter Turnout")
    
    barplot(as.matrix(race[,-1]), main='Voters Race', ylab="Percent",
            col=c("light grey", "black", 'tan','yellow', 'brown'), names.arg = lbls, legend.text=raceNames,
            beside=TRUE)
  })
  
  barState <- reactive({
    input$graphType
    input$stateName
    
    eligible <- as.data.frame(popular[E])
    all <- as.data.frame(popular[T])
    bars <- merge(eligible, all, by=0)
    this <- bars$Row.names
    rownames(bars) <- bars$Row.names
    bars <- t(bars)

    voters <- as.numeric(bars[T,])
    nonVoters <- as.numeric(bars[E,]) - voters
    
    currBars <- matrix(c(voters, nonVoters), nrow = 2, ncol = 52, byrow = TRUE)
    colnames(currBars) <- this
    rownames(currBars) <- c("Voters", "Non-Voters")
    currBars <- currBars[,-44]
    
    barplot(currBars, main="Voter Turnout by State", ylim=c(0,max(currBars) + 10000), ylab="Eligible Voters", 
            col=c("khaki1", "grey80"), legend = c("Voters", "Non-Voters"))
  })
  
  # Graphs
  output$numOfVotes <- renderPlot({
    if (input$graphType == 'Pie') {
      pieNumVotes()
    } else {
      barNumVotes()
    }
  })
  
  output$turnout <- renderPlot({
    x <- input$onClick$x
    state <- switcher(x)
    
    if (!is.null(state)) {
      updateSelectInput(session, 'stateName', selected=state, choices=stateNames)
      barVoterTurnout()
    } else if (input$graphType == 'Bar' & input$stateName == 'Total') {
      barState()
    } else if (input$graphType == 'Pie') {
      pieVoterTurnout()
    } else {
      barVoterTurnout()
    }
  })
  
  # NOTE work on age/race with new president tab
  output$age <- renderPlot({
    updateSelectInput(session, 'graphType', choices='Bar')   
    barAge()
  })
  
  output$race <- renderPlot({
    updateSelectInput(session, 'graphType', choices='Bar') 
    barRace()
  })
  
  # Misc
  switcher <- function(input=-1) {
    if (is.null(input)) {
      return(NULL)  
    }
    
    if (input < 0) { x <- NULL
    } else if (input < 1.024535) { x <- 'AK' # 1
    } else if (input < 2.427630) { x <- 'AL'
    } else if (input < 3.596876) { x <- 'AR'
    } else if (input < 4.766122) { x <- 'AZ'
    } else if (input < 6.169218) { x <- 'CA' # 5
    } else if (input < 7.104615) { x <- 'CO'
    } else if (input < 8.507710) { x <- 'CT'
    } else if (input < 9.676956) { x <- 'DC'
    } else if (input < 10.84620) { x <- 'DE'
    } else if (input < 12.01545) { x <- 'FL' #10
    } else if (input < 13.18469) { x <- 'GA'
    } else if (input < 14.35394) { x <- 'HI'
    } else if (input < 15.75704) { x <- 'IA'
    } else if (input < 16.92628) { x <- 'ID'
    } else if (input < 18.09553) { x <- 'IL' # 15
    } else if (input < 19.03092) { x <- 'IN'
    } else if (input < 20.43402) { x <- 'KS'
    } else if (input < 21.60327) { x <- 'KY'
    } else if (input < 22.77251) { x <- 'LA'
    } else if (input < 23.94176) { x <- 'MA' # 20
    } else if (input < 25.11100) { x <- 'MD'
    } else if (input < 26.28025) { x <- 'ME'
    } else if (input < 27.68335) { x <- 'MI'
    } else if (input < 28.85259) { x <- 'MN'
    } else if (input < 30.02184) { x <- 'MO' # 25
    } else if (input < 31.19108) { x <- 'MS'
    } else if (input < 32.59418) { x <- 'MT'
    } else if (input < 33.52958) { x <- 'NC'
    } else if (input < 34.74692) { x <- 'ND'
    } else if (input < 35.99855) { x <- 'NE' #30
    } else if (input < 37.25017) { x <- 'NH'
    } else if (input < 38.36273) { x <- 'NJ'
    } else if (input < 39.61435) { x <- 'NM'
    } else if (input < 40.86598) { x <- 'NV'
    } else if (input < 41.97854) { x <- 'NY' #35
    } else if (input < 43.09109) { x <- 'OH'
    } else if (input < 44.34272) { x <- 'OK'
    } else if (input < 45.59434) { x <- 'OR'
    } else if (input < 46.84597) { x <- 'PA'
    } else if (input < 47.95852) { x <- 'RI' #40
    } else if (input < 49.21015) { x <- 'SC'
    } else if (input < 50.46177) { x <- 'SD'
    } else if (input < 51.65124) { x <- 'TN'
    } else if (input < 52.78989) { x <- 'TX'
    } else if (input < 54.05506) { x <- 'UT' #45
    } else if (input < 55.19372) { x <- 'VA'
    } else if (input < 56.45889) { x <- 'VT'
    } else if (input < 57.59754) { x <- 'WA'
    } else if (input < 58.73620) { x <- 'WI'
    } else if (input < 60.00137) { x <- 'WV' #50
    } else if (input < 61.03106) { x <- 'WY'
    } else { x <- NULL
    }
    
    return(x)
  }

})
