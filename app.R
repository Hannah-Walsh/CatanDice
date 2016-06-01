### Laura's Catan Dice UI in Shiny ###

# rsconnect::deployApp('/Users/hannahwalsh/Desktop/CatanDice/')
## Import packages I need
library(shiny)


## Run non-interactive code
seed = sample(1:100000, 1); # Random Seed
set.seed(seed)

dicerange = 2:12
dicefreq = c(1,2,3,4,5,6,5,4,3,2,1)


## Create the User Interface
ui <- fluidPage(
  # Title of the Page
  titlePanel("Settlers of Catan Dice"),
  
  sidebarLayout(
    # Controls
    sidebarPanel(
      # Number of Players
      radioButtons("players", label = h4("Number of Players"),
                   choices = list("3" = 3, "4" = 4), selected = 4),
      
      # Names of Players
      textInput("player1", label = h4("Player Names"), value = "Player 1"),
      textInput("player2", label = NULL, value = "Player 2"),
      textInput("player3", label = NULL, value = "Player 3"),
      textInput("player4", label = NULL, value = "Player 4"),
      
      # # Resource Input
      # h4("Resources"),
      # textInput("bricks", label = h6("Bricks"), value = ""),
      # textInput("sticks", label = h6("Sticks"), value = ""),
      # textInput("wheat", label = h6("Wheat"), value = ""),
      # textInput("sheep", label = h6("Sheep"), value = ""),
      # textInput("rocks", label = h6("Rocks"), value = ""),
      
      # Convergence Rate
      sliderInput("rate", label = h4("Convergence Rate"), min = 200, max = 500,
                  value = 200),
      
      # Convergence Rate
      sliderInput("random", label = h4("Randomness"), min = 0, max = 1,
                  value = 0.2)
    ),
    
    #Display
    mainPanel(
      # Roll Dice
      actionButton("roll", label = "Roll Dice"),
      
      # Undo Last Roll
      actionButton("undo", label = "Undo Last Roll"),
      
      # Display the Roll Number
      textOutput("Number"),
      tags$head(tags$style("#Number{font-size: 175px;}")),
      
      
      # plotOutput("plot"),
      
      
      # Display Player Name
      textOutput("PlayerName"),
      tags$head(tags$style("#PlayerName{font-size: 75px;}"))
      
      # # Display resource cards
      # imageOutput("brickpic"),
      # imageOutput("stickpic"),
      # imageOutput("sheeppic"),
      # imageOutput("wheatpic"),
      # imageOutput("rockpic"),
      # 
      # h1(textOutput("List")), ###
      # h1(textOutput("isrand")) ###
    )
  )
)


## Create the Server
server <- function(input, output) 
{
  Roll <- reactiveValues(data=NULL)
  RollList <- reactiveValues(data=NULL)
  RollTimes <- reactive({as.numeric(input$roll) - as.numeric(input$undo)})
  
  # Display Player Name
  playerList <- reactive({c(toString(input$player1), toString(input$player2), 
                            toString(input$player3), toString(input$player4))})
  playerNum <- reactive({as.numeric(RollTimes() - 1) %% 
      as.numeric(input$players)} + 1)
  
  
  # Roll the Dice
  observeEvent(input$roll, {
    israndom = sample(c(0,1), size = 1, prob=c(1-input$random, input$random),
                      replace = T)
    output$isrand <- renderText({israndom}) ###
    
    if (israndom | length(RollList) < 5) # If random roll, roll randomly
    {
      Roll$r <- sample(dicerange, size = 1, prob=dicefreq, replace = T)
      RollList$l = c(RollList$l, Roll$r)
      output$PlayerName <- renderText({playerList()[as.numeric(playerNum())]})
    }
    else # If the roll is trying to converge to the appropriate distribution
    {
      # Calculate new weights
      dicefreq = hist(RollList$l, breaks = c(1,dicerange) + 0.5)$counts
      
      newWeights = 2^(input$rate * 
                        (dicefreq/sum(dicefreq) - RollFreq/sum(RollFreq)))
      Roll$r <- sample(dicerange, size = 1, prob=newWeights, replace = T)
      RollList$l = c(RollList$l, Roll$r)
      output$PlayerName <- renderText({playerList()[as.numeric(playerNum())]})
    }
  })
  
  output$Number <- renderText({Roll$r})
  #output$List <- renderText({RollList$l}) ###
  # output$List <- renderText({table(RollList$l)}) ###
  # output$plot <- renderPlot({hist(RollList$l, breaks = c(1,dicerange) + 0.5)})
  # 
  
  # Undo last roll
  observeEvent(input$undo, {
    RollList$l = head(RollList$l, -1)
    Roll$r = RollList$l[length(RollList$l)]
    output$Number <- renderText({Roll$r})
  })
  
  # # Display resource images
  # bricks <- reactive(as.numeric(strsplit(input$bricks, ",")[[1]]))
  # sticks <- reactive(as.numeric(strsplit(input$sticks, ",")[[1]]))
  # sheep <- reactive(as.numeric(strsplit(input$sheep, ",")[[1]]))
  # wheat <- reactive(as.numeric(strsplit(input$wheat, ",")[[1]]))
  # rocks <- reactive(as.numeric(strsplit(input$rocks, ",")[[1]]))
  # 

  
}



## Link the Server and the UI
shinyApp(ui = ui, server = server)