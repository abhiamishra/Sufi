#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(htmltools)

source("wordlist.R")


############### ui ##################

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bslib::bs_theme(version = 4),
    title = "Sufi",
    tags$head(
        tags$style(HTML("
            .container-fluid {
                max-width: 500px;
            }
            
            
            #result {
              display: grid;
              grid-template-columns: repeat(5, 50px);
              gap: 5px;
              padding: 15px;
              justify-content: center;
            }
            
            .guess-letter{
                width: 50px; 
                height: 50px;
                font-size: 20px;
                display: grid;
                place-content: center;
                border-radius: 5px;
                color: white;
            }
            
            
            .in-word {
                background-color: #c8b458;
            }
            
            .wrong {
                background-color: #787c7e;
            }
            
            .correct {
                background-color: #6aa964;
            }
            
            .input-wrapper {
                display: flex;
                justify-content: center;
                align-items: flex-end;
                align-content: center;
                padding-bottom: 15px;
            }
            
            .input-wrapper > div {
                margin-bottom: 0;
            }
            
            .input-wrapper > button {
                margin-left: 10px;
            }
          ")
        )
    ),
    
    
    h3("Sufi"),
    uiOutput("result"),
    
    div(
    
        textInput("guess", ""),
    
    #verbatimTextOutput("result", placeholder=TRUE),
    
    #p("[x] means the letter is in word and in correct place."),
    #p("(x) means the letter is in word and not in correct place."),
    #p("x means the letter is not in word"),
    
        actionButton("go", "Go"),
        class="input-wrapper"
    
    ),
    
    verbatimTextOutput("keyboard", placeholder=TRUE)
)

############### SERVER ####################

set.seed(as.integer(Sys.Date()))
target <- sample(words_common, 1)

# Define server logic required to draw a histogram
server <- function(input, output) {

    all_guesses <- reactiveVal(character()) #Store all tries
        
    output$result <- renderUI({
        if(!input$guess %in% words_common){
            req(FALSE, cancelOutput = TRUE) #Doesn't erase the previous content
        }
        
        all_guesses_new <- c(all_guesses(), input$guess)
        all_guesses(all_guesses_new)
        
        print_str <- lapply(all_guesses(), function(guess){
            result <- check_words(target, guess)
            format_result(result)
            #div(format_result(result), style="outline:1px solid salmon;")
        })
        
        #browser() #Use for debugging
        #paste(print_str, collapse="\n")
        print_str
        
    }) |> 
        bindEvent(input$go) #Very important as it ensures that reactivity is BINDED to hit the go button and not the reactiveValue

    #Adding keyboard
    output$keyboard <- renderText({
        keys <- paste(
            "  q  w  e  r  t  y  u  i  o  p  ",
            "   a  s  d  f  g  h  j  k  l  ",
            "    z  x  c  v  b  n  m  ",
            sep = "\n"
        )
        
        used_letters <- paste(all_guesses(), collapse="")
        used_letters <- strsplit(used_letters, "")[[1]]
        used_letters <- unique(used_letters)
        
        for (letter in used_letters){
            keys <- sub(letter, " ", keys)
        }
        
        keys
    })
}

############### BACK - END ###############

#target: "gives"
#guess: "aisle"
compare_word <- function(target_str, guess_str){
    
    #Check if words are the same length
    if(nchar(target_str) != 5){
        stop("target and guess string must be the same length")
    }
    
    #Splitting the string into an array of characters
    target <- strsplit(target_str, "")[[1]]
    guess <- strsplit(guess_str, "")[[1]]
    result <- rep("wrong", 5) #fill result array with ""s
    remaining <- character(0)
    
    for(i in seq_along(guess)){
        if(guess[i] == target[i]){
            result[i] <- "correct"
        }
        else{
            remaining <- c(remaining, target[i])
        }
    }
    
    for(i in seq_along(guess)){
        if(guess[i] != target[i] &&
           guess[i] %in% remaining){
            result[i] <- "in-word"
            
            remaining <- remaining[-match(guess[i], remaining)]
            #removing things from vector
        }
    }
    
    
    
    result
}

#remaining <- c("i","g","g")
#guess <- "g"
#match(guess, remaining)
#remaining[-match(guess, remaining)]


check_words <- function(target_str, guess_str){
    compare_result <- compare_word(target_str, guess_str)
    correct <- FALSE
    if (all(compare_result == "correct")){
        correct <- TRUE
    }
    
    list(
        word = guess_str,
        letters = strsplit(guess_str, "")[[1]],
        result = compare_result,
        correct = correct
    )
}

format_result <- function(comp_result){
    #out_str <- ""
    out_divs <- tagList()
 for(i in seq_along(comp_result$letters)){
     letterText <- if (comp_result$result[i] == "correct"){
         paste("", comp_result$letters[i], "")
         #out_str <- paste0(out_str, "[", comp_result$letters[i], "]")
     }
     else if(comp_result$result[i] == "in-word"){
         paste("", comp_result$letters[i], "")
         
         #out_str <- paste0(out_str, "(", comp_result$letters[i], ")")
     }
     else{
         paste("", comp_result$letters[i], "")
         
         #out_str <- paste0(out_str, " ", comp_result$letters[i], " ")
     }
     out_divs[[i]] <- div(letterText, class=paste("guess-letter", comp_result$result[i]))
     
 }
    out_divs
}

shinyApp(ui, server)
