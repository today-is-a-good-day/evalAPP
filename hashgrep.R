hashgrep <-
function(text) {
    hg <- function(text) {
        result <- ""
        while(text != result) {
            result <- text
            text <- gsub("#[[:alpha:]]+\\K([[:upper:]]+)", " \\1", 
                         text, perl = TRUE)
        }
        return(text)
    }
    unname(sapply(text, hg))
}
