library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)
options(shiny.maxRequestSize=30*1024^2)

myfpkm <- read.table('/Users/katiecampbell/Documents/Projects/M1661/fpkm_matrix.txt', head = T, sep = '\t')

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {
  matrixData <- reactive({
    # input$fpkm_input will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'gene_name' 
    # in the first column, and the remaining columns are samples with FPKM 
    # expression values
    inFile <- input$fpkm_input
    if(is.null(inFile))
      return(NULL)
    
    d <- read.table(file = '/Users/katiecampbell/Documents/Projects/M1661/fpkm_matrix.txt', head = TRUE)
    return(d)
  })
  geneOptions <- reactive({
    if(is.null(matrixData()))
      selected <- c()
    
    selected <- as.character(matrixData()[,1])
    return(selected)
  })
  
  output$fpkm_matrix <- renderDataTable({
    matrixData()
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  output$gene_input <- renderUI({
    selectizeInput("gene_input", "Select genes", selected = NULL, multiple = TRUE, choices = geneOptions(), options = list(maxItems = 10, placeholder = "Pick your genes"))
  })
  
})
