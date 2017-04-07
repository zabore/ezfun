#' Interactive Simon's 2-stage Shiny app
#'
#' \code{ph2simonApp} is simply a Shiny interface for the \code{ph2simon} 
#' function from the \code{clinfun} package. No arguments need to be passed to
#' the function.
#'
#' @return The output includes 1) fields to enter the design parameters for the 
#' Simon 2-stage Phase II design, 2) R output with \code{ph2simon} results, 
#' 3) a paragraph interpreting the results, and 4) a plot of maximum versus expected 
#' number of patients indicating the optimal and minimax results.
#'
#' @export
#'

ph2simonApp <- function() {
  library(shiny)
  library(clinfun)
  
  shinyApp(
    ui = fluidPage(
      titlePanel("Simon two-stage design"),
      
      sidebarLayout(
        sidebarPanel(
          h4("Design parameters"),
          
          numericInput("num1", label ="Unacepptable rate", value = 0.1, min = 0, max = 1, step = 0.05),
          
          numericInput("num2", label ="Acceptable rate", value = 0.3, min = 0, max = 1, step = 0.05),
          
          numericInput("num3", label ="Alpha", value = 0.05, min = 0, max = 1, step = 0.05),
          
          numericInput("num4", label ="Power", value = 0.80, min = 0, max = 1, step = 0.05)
          
        ),
        mainPanel(
          h4("Results"),
          verbatimTextOutput("tab1"),
          
          h6("For the Simon optimal design:"),
          textOutput("text1"),
          
          plotOutput("plot1")
        )
      )
    ), 
    
    server = function(input, output) {
      
      output$tab1<-renderPrint({
        fun<-ph2simon(input$num1,input$num2,input$num3,(1-input$num4))
        print(fun)
      })
      
      output$text1<-renderText({
        fun<-ph2simon(input$num1,input$num2,input$num3,(1-input$num4))
        paste("If you see more than",
              fun$out[which.min(fun$out[,5]),1],
              "responses out of the first",
              fun$out[which.min(fun$out[,5]),2],
              "participants in the first stage, then accrue to a total of",
              fun$out[which.min(fun$out[,5]),4],
              "participants. Otherwise if you see",
              fun$out[which.min(fun$out[,5]),1],
              "or fewer responses out of the first",
              fun$out[which.min(fun$out[,5]),2],
              "participants in the first stage, then stop the trial. After accruing",
              fun$out[which.min(fun$out[,5]),4],
              "participants, if you see more than",
              fun$out[which.min(fun$out[,5]),3],
              "responses, then the intervention is considered worthy of further testing.")
      })
      
      output$plot1<-renderPlot({
        fun<-ph2simon(input$num1,input$num2,input$num3,(1-input$num4))
        plot(fun)
      })
    }
  )
}

# ph2simonApp()
