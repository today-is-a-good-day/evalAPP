library(shiny)

# Define UI for application
shinyUI(fluidPage(
    titlePanel("Social Media Evaluation"),
    sidebarLayout(
        sidebarPanel(
            textInput("twitter", label = h3("Enter Twitter username:")), 
            textInput("instagram", label = h3("Enter Instagram username:")),
            actionButton("goButton", "Go!", styleclass = "primary"),
            br(),
            br()
            ),
        
        mainPanel(
            h3("How positive are your postings?"),
            p("Give in your twitter username and instagram username to get a 
              feedback about your posting's valence."), 
            
            conditionalPanel(
                condition = "input.goButton > 0",
                htmlOutput("view"),
                h3("Here you go:")
            )
        )
)))