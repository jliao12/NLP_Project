library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("NLP - Next Word Prediction"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("words1","Put some words here:","I want to eat ice"),
      sliderInput("NOR","Number of predicted outcomes to display",
                  min = 3, max = 10,value = 5),
      h5("Notice:"),
      p("Stemed words are used in the data")
    ),
    
    
    
    
    # Show a plot of the generated distribution
    mainPanel(
      h3('The most likely next word will be'),
      verbatimTextOutput("text1"),
      h4("Possible words from Trigrams data"),
      verbatimTextOutput("text2"),
      h4("Possible words from Bigrams data"),
      verbatimTextOutput("text3"),
      h4("Possible words from Uigram data"),
      verbatimTextOutput("text4")
    )
  )
))