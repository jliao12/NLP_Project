library(shiny)
library(dplyr)
library(quanteda)
library(datasets)
library(data.table)

triWords <- function(w1, w2, n) {
  pwords <- tri_words[word_1 == w1 & word_2 == w2][order(-Prob)]
  if (any(is.na(pwords))) {
    return(biWords(w2, n))
  }
  else if (nrow(pwords) > n) {
    return(list("tri"=pwords[1:n, word_3],
                "bi" = character(0),"uni" = character(0)))
  } else {
    count <- nrow(pwords)
    bwords <- biWords(w2, n)$bi[1:(n - count)]
    return(list("tri" = pwords[,word_3],"bi" = bwords,"uni" = character(0)))
  }
  
}

# function to return highly probable previous word given a word
biWords <- function(w1, n) {
  pwords <- bi_words[w1][order(-Prob)]
  if (any(is.na(pwords))) {
    return(uniWords(n))
  } else if (nrow(pwords) > n) {
    return(list("tri" = character(0),"bi" = pwords[1:n, word_2],"uni" = character(0)))
  } else {
    count <- nrow(pwords)
    unWords <- uniWords(n)$uni[1:(n - count)]
    return(list("tri" = character(0),"bi" = pwords[, word_2], "uni" = unWords))
  }
}

# function to return random words from unigrams
uniWords <- function(n) {  
  return(list("tri" = character(0),"bi" = character(0),
              "uni" = sample(uni_words[, word_1], size = n)))
}

# The prediction app
getWords <- function(str,n){
  require(quanteda)
  tokens <- tokens(x = char_tolower(str),remove_punct = TRUE)
  tokens <- char_wordstem(rev(rev(tokens[[1]])[1:2]), language = "english")
  
  words <- triWords(tokens[1], tokens[2], n)
  
  return(words)
}

uni_words <- readRDS(file = "unidata")
bi_words <- readRDS(file = "bidata")
tri_words <- readRDS(file = "tridata")

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  output$text1 <- renderPrint({
    if (identical(getWords(input$words1,input$NOR)$tri,character(0)) &
        identical(getWords(input$words1,input$NOR)$bi,character(0))){
      getWords(input$words1,input$NOR)$uni[1]
    } else if (!identical(getWords(input$words1,input$NOR)$bi,character(0)) &
               identical(getWords(input$words1,input$NOR)$tri,character(0))){
      getWords(input$words1,input$NOR)$bi[1]
    } else {
      getWords(input$words1,input$NOR)$tri[1]
    }
    
  })
  output$text2 <- renderPrint({
    if (identical(getWords(input$words1,input$NOR)$tri,character(0))){
      "No other possible words within this data set"
    } else {
      getWords(input$words1,input$NOR)$tri
    }
  })
  output$text3 <- renderPrint({
    if (identical(getWords(input$words1,input$NOR)$bi,character(0))){
      "No other possible words within this data set"
    } else {
      getWords(input$words1,input$NOR)$bi
    }
  })
  output$text4 <- renderPrint({
    if (identical(getWords(input$words1,input$NOR)$uni,character(0))){
      "No other possible words within this data set"
    } else {
      getWords(input$words1,input$NOR)$uni
    }
  })
  
})