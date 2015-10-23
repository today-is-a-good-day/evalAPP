getScore <-
function(twittername = NULL, igname = NULL) {
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
    
    # load utility functions
    source("hashgrep.R")
    source("getUserMedia.R")
    
    if (!is.null(twittername) & is.null(igname)) {
        # get most recent tweets
        user_tweets <- userTimeline(twittername, n = 100, includeRts = TRUE)
        # get the text\\
        dirty_tweets <- twListToDF(user_tweets) 
        dirty_tweets <- data.frame(unlist(lapply(user_tweets, function(t)t$getText())))
        names(dirty_tweets) <- "text"
        
        # cleaning pipeline
        clean_tweets <- data.frame(sapply(dirty_tweets$text, function(row) 
            iconv(row, "latin1", "ASCII", sub="")))
        names(clean_tweets) <- "text"
        clean_tweets <- clean_tweets$text %>%
            gsub("&amp;", "", .) %>% # remove &
            gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", .) %>% # remove retweet entities
            gsub("@\\w+", "", .) %>% # remove at people
            #gsub("([[:upper:]][[:lower:]])", " \\1", .) %>% # split group of words at uppercase letter and add a white space
            hashgrep %>%
            gsub("[[:punct:]]", "", .) %>% # remove punctuation
            gsub("[[:digit:]]", "", .) %>% # remove digits
            gsub("http\\w+", "", .) %>% # remove html links
            iconv(from = "latin1", to = "ASCII", sub="") %>% # remove emoji and bizarre signs
            gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
            gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces
            subset(., . != "") %>% # remove empty lines
            tolower %>%
            data.frame(., stringsAsFactors=FALSE)
        names(clean_tweets) <- "text"
        
        ################## Compute polarity ################
        
        # determine polarity of tweets
        str_tweets <- str_split(clean_tweets$text, "\\s+")
        pos.matches <- lapply(str_tweets, match, table = pos)
        neg.matches <- lapply(str_tweets, match, table = neg)
        pos.matches <- lapply(pos.matches, function(row) !is.na(row))
        neg.matches <- lapply(neg.matches, function(row) !is.na(row))
        scores.pos <- lapply(pos.matches, function(row) length(pos.matches[row == TRUE]))
        scores.neg <- lapply(neg.matches, function(row) length(neg.matches[row == TRUE]))
        ## if statement zur überprüfung von polarität (<0 ist negativ, >0 ist positiv, = 0 ist neutral)
        
        # sum scores to total score
        score <- sum(sapply(pos.matches, sum) - sapply(neg.matches, sum))
        score
        
    } else if (is.null(twittername) & !is.null(igname)) {
        # get user media
        user_media <- getUserMedia(igname, token = token)
        
        # cleaning pipeline
        clean_ig <- data.frame(sapply(user_media$df.text, function(row) 
            iconv(row, "latin1", "ASCII", sub="")))
        names(clean_ig) <- "text"
        clean_ig <- clean_ig$text %>%
            gsub("&amp;", "", .) %>% # remove &
            gsub("@\\w+", "", .) %>% # remove at people
            #gsub("([[:upper:]][[:lower:]])", " \\1", .) %>% # split group of words at uppercase letter and add a white space
            hashgrep %>%
            gsub("[[:punct:]]", "", .) %>% # remove punctuation
            gsub("[[:digit:]]", "", .) %>% # remove digits
            gsub("http\\w+", "", .) %>% # remove html links
            iconv(from = "latin1", to = "ASCII", sub="") %>% # remove emoji and bizarre signs
            gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
            gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces
            subset(., . != "") %>% # remove empty lines
            tolower %>%
            data.frame(., stringsAsFactors=FALSE)
        names(clean_ig) <- "text"
        
        # determine polarity
        str_ig <- str_split(clean_ig$text, "\\s+")
        pos.matches <- lapply(str_ig, match, table = pos)
        neg.matches <- lapply(str_ig, match, table = neg)
        pos.matches <- lapply(pos.matches, function(row) !is.na(row))
        neg.matches <- lapply(neg.matches, function(row) !is.na(row))
        scores.pos <- lapply(pos.matches, function(row) length(pos.matches[row == TRUE]))
        scores.neg <- lapply(neg.matches, function(row) length(neg.matches[row == TRUE]))
        
        # sum scores to total score
        score <- sum(sapply(pos.matches, sum) - sapply(neg.matches, sum))
        score
        
    } else if (!is.null(twittername) & !is.null(igname)) {
        
        ################################ TWITTER ###################################
        
        # get most recent tweets
        user_tweets <- userTimeline(twittername, n = 100, includeRts = TRUE)
        # get the text
        dirty_tweets <- twListToDF(user_tweets) 
        dirty_tweets <- data.frame(unlist(lapply(user_tweets, function(t)t$getText())))
        names(dirty_tweets) <- "text"
        
        # cleaning pipeline
        clean_tweets <- data.frame(sapply(dirty_tweets$text, function(row) 
            iconv(row, "latin1", "ASCII", sub="")))
        names(clean_tweets) <- "text"
        clean_tweets <- clean_tweets$text %>%
            gsub("&amp;", "", .) %>% # remove &
            gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", .) %>% # remove retweet entities
            gsub("@\\w+", "", .) %>% # remove at people
            #gsub("([[:upper:]][[:lower:]])", " \\1", .) %>% # split group of words at uppercase letter and add a white space
            hashgrep %>%
            gsub("[[:punct:]]", "", .) %>% # remove punctuation
            gsub("[[:digit:]]", "", .) %>% # remove digits
            gsub("http\\w+", "", .) %>% # remove html links
            iconv(from = "latin1", to = "ASCII", sub="") %>% # remove emoji and bizarre signs
            gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
            gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces
            subset(., . != "") %>% # remove empty lines
            tolower %>%
            data.frame(., stringsAsFactors=FALSE)
        names(clean_tweets) <- "text"
        
        
        ################## Compute polarity ################
        
        # determine polarity of tweets
        str_tweets <- str_split(clean_tweets$text, "\\s+")
        pos.matches <- lapply(str_tweets, match, table = pos)
        neg.matches <- lapply(str_tweets, match, table = neg)
        pos.matches <- lapply(pos.matches, function(row) !is.na(row))
        neg.matches <- lapply(neg.matches, function(row) !is.na(row))
        scores.pos <- lapply(pos.matches, function(row) length(pos.matches[row == TRUE]))
        scores.neg <- lapply(neg.matches, function(row) length(neg.matches[row == TRUE]))
        ## if statement zur überprüfung von polarität (<0 ist negativ, >0 ist positiv, = 0 ist neutral)
        
        # sum scores to total score
        twitterscore <- sum(sapply(pos.matches, sum) - sapply(neg.matches, sum))
        
        
        ############################ INSTAGRAM #####################################
        
        # get user media
        user_media <- getUserMedia(igname, token = token)
        
        # cleaning pipeline
        clean_ig <- data.frame(sapply(user_media$df.text, function(row) 
            iconv(row, "latin1", "ASCII", sub="")))
        names(clean_ig) <- "text"
        clean_ig <- clean_ig$text %>%
            gsub("&amp;", "", .) %>% # remove &
            gsub("@\\w+", "", .) %>% # remove at people
            #gsub("([[:upper:]][[:lower:]])", " \\1", .) %>% # split group of words at uppercase letter and add a white space
            hashgrep %>%
            gsub("[[:punct:]]", "", .) %>% # remove punctuation
            gsub("[[:digit:]]", "", .) %>% # remove digits
            gsub("http\\w+", "", .) %>% # remove html links
            iconv(from = "latin1", to = "ASCII", sub="") %>% # remove emoji and bizarre signs
            gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
            gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces
            subset(., . != "") %>% # remove empty lines
            tolower %>%
            data.frame(., stringsAsFactors=FALSE)
        names(clean_ig) <- "text"
        
        # determine polarity
        str_ig <- str_split(clean_ig$text, "\\s+")
        pos.matches <- lapply(str_ig, match, table = pos)
        neg.matches <- lapply(str_ig, match, table = neg)
        pos.matches <- lapply(pos.matches, function(row) !is.na(row))
        neg.matches <- lapply(neg.matches, function(row) !is.na(row))
        scores.pos <- lapply(pos.matches, function(row) length(pos.matches[row == TRUE]))
        scores.neg <- lapply(neg.matches, function(row) length(neg.matches[row == TRUE]))
        
        # sum scores to total score
        igscore <- sum(sapply(pos.matches, sum) - sapply(neg.matches, sum))
        
        # sum up twitter score & ig score
        score <- (igscore + twitterscore) / 2
        score
        
    } else 
        print("Sorry, we can´t provide feedback as you didn´t indicate any social media profile")
    
}
