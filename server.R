library(shiny)
library(twitteR)
library(httr)
library(RCurl)
library(RJSONIO)
library(dplyr)
library(magrittr)
library(stringr)
source("hashgrep.R")
source("getUserMedia.R")
source("getScore.R")

# cache
options(httr_oauth_cache = TRUE)

# load instagram oauth
load("ig_oauth_ia")
token <- ig_oauth_ia$token

# load twitter oauth 
load("twitterOauthSurvey.Rdata")
consumer_key <- my_oauth$consumerKey
consumer_secret <- my_oauth$consumerSecret
access_token <- my_oauth$oauthKey
access_secret <- my_oauth$oauthSecret
setup_twitter_oauth(consumer_key = consumer_key, consumer_secret = consumer_secret,
                    access_token = access_token, access_secret = access_secret)
1

# read in lists of words
pos <- readRDS("data/poswords.rds")
neg <- readRDS("data/negwords.rds")

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
