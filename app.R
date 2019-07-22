#
# The application outputs the list of words with defined set of letters in each word.
#
# Author: Ruben R. Kazumov
# License: MIT
# 
library(shiny)

words <- read.csv("words.txt", header = FALSE, stringsAsFactors = FALSE)
wordsVector <- words[, 1]
rm(words)

lettersListChoices <- LETTERS
names(lettersListChoices) <- LETTERS


extractWithLetter <- function(letter, v){
    index <- grep(letter, v, ignore.case = TRUE)
    return(
        v[index]
    )
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Search words with the set of letters inside"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput(inputId = "lettersList", 
                               label = "Select the letters to search", 
                               choices = lettersListChoices)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            verbatimTextOutput(outputId = "details"),
            dataTableOutput(outputId = "wordList")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    results <- reactive({
        w <- wordsVector
        process <- ""
        for (l in input$lettersList) {
            w <- extractWithLetter(l, w)
            process <- paste(process, "A letter", l, "decreased set to", length(w), "words\n")
        }
        return(
            list(process = HTML(process),
                 words = data.frame(words = w)
            )
        )
    })
    
    output$details <- renderText({
        return(results()$process)
    })
    
    output$wordList <- renderDataTable({
        return(results()$words)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
