library(shiny)
library(twitteR)
library(httr)
library(RCurl)
library(RJSONIO)
library(dplyr)
library(magrittr)
library(stringr)
source("getScore.R")

# cache
options(httr_oauth_cache = TRUE)

shinyServer(
    function(input, output) {
        # Use search function to search for username (and get id)
        score <- reactive({
            input$goButton
            twittername <- isolate(input$twitter)
            igname <- isolate(input$instagram)
            
            score <- getScore(twittername = twittername, igname = igname)
            
            
        })
        
        output$view <- score
    
})
