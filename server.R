library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)
options(shiny.maxRequestSize=30*1024^2)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {
  fpkmMatrix <- reactive({
    inFile <- input$fpkm_input
    if(is.null(inFile))
      return(NULL)
    d <- read.table(file = as.character(inFile$datapath), head = TRUE, sep = input$fpkm_delim)
    return(d)
  })
  sampleMatrix <- reactive({
    inFile <- input$sample_input
    if(is.null(inFile))
      return(NULL)
    d <- read.table(file = as.character(inFile$datapath), head = TRUE, sep = input$sample_delim)
    return(d)
  })
  clinMatrix <- reactive({
    inFile <- input$clin_input
    if(is.null(inFile))
      return(NULL)
    d <- read.table(file = as.character(inFile$datapath), head = TRUE, sep = input$clin_delim)
    return(d)
  })
  # Output data matrices
  output$fpkm_matrix <- renderDataTable({ fpkmMatrix() }, options = list(pageLength = 10, scrollX = TRUE))
  output$sample_matrix <- renderDataTable({ sampleMatrix() }, options = list(pageLength = 10, scrollX = TRUE))
  output$clin_matrix <- renderDataTable({ clinMatrix() }, options = list(pageLength = 10, scrollX = TRUE))
  # Select genes
  geneOptions <- reactive({
    if(is.null(fpkmMatrix()))
      selected <- c()
    selected <- as.character(fpkmMatrix()[,1])
    return(selected)
  })
  subsetByGene <- reactive({ 
    if(is.null(fpkmMatrix()))
      return(NULL)
    subsetMatrix <- fpkmMatrix()[which(as.character(fpkmMatrix()[,1]) %in% input$gene_input),] 
    return(subsetMatrix)
    })
  #
  observe({
    updateSelectizeInput(session, "gene_input",
                         choices = geneOptions(), server = TRUE)
  })
  output$selectedGenes <- renderText({ input$gene_input })
  output$singleGeneBoxplots <- renderPlotly({
    if(is.null(subsetByGene()))
      return(NULL)
    
    geneNameColumn <- colnames(subsetByGene())[1]
    formatData <- subsetByGene() %>% 
      gather(sample, fpkm, -matches(geneNameColumn))
    if(input$logFPKM == TRUE){
      plotY = "log10(fpkm+1)"
      labelY = "Log FPKM"
    } else {
      plotY = "fpkm"
      labelY = "FPKM"
    }
    p <- ggplot(formatData, aes_string(x = geneNameColumn, y = plotY)) +
      geom_jitter(width = 0.1, alpha = 0.4) + geom_boxplot(outlier.colour = NA, fill = NA) +
      labs(x = "Gene", y = labelY) +
      theme_bw() +
      theme(text = element_text(size = 10), title = element_text(size = 12, face = 'bold'))
    return(ggplotly(p))
  })
})
