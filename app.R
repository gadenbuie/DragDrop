library(shiny)
library(dragulaR)
library(shinyjs)
library(DT)
library(tidyverse)

source("CSS.R")

dummy <- data.frame(age = c(20,30,50,60,70,90,40,20,40,30),
                    sex = c(rep("male", 5), rep("female", 5)),
                    treatment = c(rep("control", 5), rep("treatment", 5)),
                    dose = c(seq(1:5), seq(1:5)))

test <- c("mean/sd", "func_2", "func_3")

columnBlocks <- function(data, name)
{
  div(style = "
      border-width:2px;
      border-style:solid;
      background-color: #D7DADC;
      font-size: 20px;
      text-align: center;
      "
      ,
      drag = name,
      div(class = "active-title", name))
}


rowBlocks <- function(data, name)
{
  div(style = "
      border-width:2px;
      border-style:solid;
      background-color: #cadbed;
      font-size: 20px;",
      drag = name,
      div(class = "active-title", name))
}

aggBlocks <- function(data, name)
{
  div(style = "
      border-width:2px;
      border-style:solid;
      background-color: #d6e6a6;
      font-size: 20px;",
      drag = name,
      div(class = "active-title", name))
}

ui <- fluidPage(
  
  inlineCSS(css),
  
  fluidRow(style = "margin: 15px;",
           
           fluidRow(column(12,
                    h3("Columns:"),
                                      div(id = "Available1", 
                                          style = "min-height: 10px;",
                                          lapply(colnames(dummy[,3:4]), columnBlocks, data = dummy)))
           ),
           
           fluidRow(
           column(3,
                  h3("Rows:"),
                  div(id = "Available2", style = "min-height: 600px;",
                      lapply(colnames(dummy[,1:2]), rowBlocks, data = dummy))
           ),
           
           column(6,
                  fluidRow(
                    column(12,
                           div(id = "colOutput", 
                               style = "min-height: 30px; background-color: #F5F6F6;"),
                           dragulaOutput("dragula1")
                    )
                  ),
                  
                  fluidRow(
                    column(6,offset=0,
                           div(id = "rowOutput", 
                               style = "min-height: 300px; 
max-height: 300px;
                               background-color: #F2F6FA; 
                               margin-top:-3.5em;
                               margin-right:-1em;")
                    ),
                    column(6,
                           div(id = "aggOutput", 
                               style = "min-height: 300px; background-color: #F5F9E9; margin-top:-3.5em; margin-left:-1em;")
                    )
                  ),
                  
                  fluidRow(
                    column(12, align="center",
                           tableOutput("table")
                    ))
            ),
           
           column(3,
                  h3("Aggregate:"),
                  div(id = "Available3", style = "min-height: 600px;",
                      lapply(test, aggBlocks, data = test))
           )

           )

  ),
  

  dragulaOutput("dragula2"),
  dragulaOutput("dragula3")
  
)

server <- function(input, output) {
  
  output$dragula1 <- renderDragula({
    dragula(c("Available1", "colOutput"))
  })
  
  output$dragula2 <- renderDragula({
    dragula(c("Available2", "rowOutput"))
  })
  
  output$dragula3 <- renderDragula({
    dragula(c("Available3", "aggOutput"))
  })
  
  # This is the source of the error 
  # I think checking is.null isn't what I need to do because
  # I'm getting the error: Error in [[: subscript out of bounds
  grouping_variable <- reactive({
    gv <- NULL
    if (is.null(input$dragula1$colOutput[[1]][1])) {
      gv <- ""
    } else {
      gv <- input$dragula1$colOutput[[1]][1]
    } 
  })
  
  rows <- reactive ({
    input$dragula2
  })
  
#  agg_func <- reactive({
#    af <- NULL
#    if (d_agg$aggOutput[[1]][1] == "mean/sd") {
#      af <- paste("%>% summarize(mean = mean(", d_row$rowOutput[[1]][1], "), sd = sd(", d_row$rowOutput[[1]][1], ")")
#    } else {
#      af <- ""
#    }
#  })

  output$table <-  renderTable({
    
    string <- paste("dummy %>% group_by(",
                    grouping_variable(), ")")
    
    # testing I see that the string "dummy %>% group_by(  )" is being printed initially
    # but this original dataframe isn't displaying because
    ### Error in [[: subscript out of bounds
    print(string)
    
    t <- eval(parse(text=string))
    t
      
  }, rownames = TRUE)
  
}

shinyApp(ui = ui, server = server)