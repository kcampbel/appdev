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
  # Merge matrices for mapping
  observe({ updateSelectInput(session, "sampleIdentifier", choices = colnames(sampleMatrix())) })
  observe({ updateSelectInput(session, "individualIdentifier1", choices = colnames(sampleMatrix())) })
  observe({ updateSelectInput(session, "individualIdentifier2", choices = colnames(clinMatrix())) })
  # mapMatrix <- reactive({
  #   if(is.null(fpkmMatrix())){
  #     return(NULL)
  #   } else if(is.null(sampleMatrix())){
  #     
  #   }
  # })
  # Select genes
  geneOptions <- reactive({
    if(is.null(fpkmMatrix()))
      selected <- c()
    selected <- as.character(fpkmMatrix()[,1])
    return(selected)
  })
  observe({ updateSelectizeInput(session, "gene_input", choices = geneOptions(), server = TRUE) })
  observe({ updateSelectInput(session, "colorGroup", choices = colnames(sampleMatrix())) })
  #
  subsetByGene <- reactive({ 
    if(is.null(fpkmMatrix()))
      return(NULL)
    subsetMatrix <- fpkmMatrix()[which(as.character(fpkmMatrix()[,1]) %in% input$gene_input),] 
    colnames(subsetMatrix)[1] <- "geneName"
    return(subsetMatrix)
    })
  observe({ updateSelectInput(session, "x_corrScatter", choices = input$gene_input) })
  observe({ updateSelectInput(session, "y_corrScatter", choices = input$gene_input) })
  # output$selectedGenes <- renderText({ input$gene_input })
  output$geneBoxplots <- renderPlotly({
    if(is.null(subsetByGene()) | is.null(input$gene_input))
      return(NULL)
    
    formatData <- subsetByGene() %>% 
      gather(colnameFPKM, fpkm, -geneName)
    if(input$logFPKM == TRUE){
      plotY = "log10(fpkm+1)"
      labelY = "Log FPKM"
    } else {
      plotY = "fpkm"
      labelY = "FPKM"
    }
    q <- ggplot(formatData, aes_string(x = "geneName", y = plotY)) +
      geom_jitter(width = 0.1, alpha = 0.4, size = 2.5, colour = '#1C4EA1') + geom_boxplot(outlier.shape = 32) +
      labs(x = "Gene", y = labelY) +
      theme_bw() +
      theme(text = element_text(size = 10), title = element_text(size = 12, face = 'bold'))
    p <- plotly_build(q)
    p$x$data <- lapply(p$x$data, FUN = function(s){
      s$marker$outliercolor = "rgba(0,0,0,0)"
      s$marker$line$outliercolor = "rgba(0,0,0,0)"
      return(s)
    })
    return(p)
  })
  output$dualGeneHistograms <- renderPlotly({
    if(is.null(subsetByGene()) | is.null(input$gene_input))
      return(NULL)
    
    formatData <- subsetByGene() %>% filter(geneName %in% c("HLA-A",input$x_corrScatter, input$y_corrScatter)) %>%
      gather(colnameFPKM, fpkm, -geneName) %>% 
      mutate(logFPKM = ifelse(is.na(fpkm)==F, log10(fpkm+1), NA))
    if(input$logFPKM == TRUE){
      plotX = "log10(fpkm+1)"
      labelX = "Log FPKM"
    } else {
      plotX = "fpkm"
      labelX = "FPKM"
    }
    q <- ggplot(formatData, aes_string(x = plotX)) +
      geom_histogram(colour = 'white', alpha = 0.4) +
      scale_fill_brewer(palette = 'Set1') +
      labs(x = labelX) +
      theme_bw() +
      theme(text = element_text(size = 10), title = element_text(size = 12, face = 'bold')) +
      facet_wrap(~geneName, scales = 'free')
    p <- plotly_build(q)
    return(p)
  })
  
  output$corrScatter <- renderPlotly({
    if(is.null(subsetByGene()) | is.null(input$gene_input))
      return(NULL)
    
    formatData <- subsetByGene() %>% filter(geneName %in% c("HLA-A",input$x_corrScatter, input$y_corrScatter)) %>%
      gather(colnameFPKM, fpkm, -geneName) %>% 
      mutate(logFPKM = ifelse(is.na(fpkm)==F, log10(fpkm+1), NA))
    if(input$logFPKM == TRUE){
      reformatData <- formatData %>% dplyr::select(-fpkm) %>% spread(geneName, logFPKM)
      labelX = paste0("Log FPKM - ", input$x_corrScatter)
      labelY = paste0("Log FPKM - ", input$y_corrScatter)
    } else {
      reformatData <- formatData %>% dplyr::select(-logFPKM) %>% spread(geneName, fpkm)
      labelX = paste0("FPKM - ", input$x_corrScatter)
      labelY = paste0("FPKM - ", input$y_corrScatter)
    }
    statistics <- cor.test(reformatData[,as.character(input$x_corrScatter)], reformatData[,as.character(input$y_corrScatter)], method = input$corrMethod)
    corrLabel <- paste0(input$corrMethod," = ",signif(statistics$estimate,3),"\np = ",signif(statistics$p.value, 3))
    q <- ggplot(data = reformatData, aes_string(x = paste0("`",input$x_corrScatter,"`"), y = paste0("`",input$y_corrScatter,"`"))) +
      geom_point(alpha = 0.4, size = 2.5, color = '#1C4EA1') +
      geom_smooth(size = 0.5, alpha = 0.5, method = 'lm') +
      geom_text(label = corrLabel, size = 3,
                x = max(reformatData[,as.character(input$x_corrScatter)], na.rm=T)-(max(reformatData[,as.character(input$x_corrScatter)], na.rm=T)*0.05),
                y = max(reformatData[,as.character(input$y_corrScatter)], na.rm=T)-(max(reformatData[,as.character(input$y_corrScatter)], na.rm=T)*0.05),
                hjust = 1, vjust = 1)
      labs(x = labelX, y = labelY) +
      theme_bw() +
      theme(text = element_text(size = 10), title = element_text(size = 12, face = 'bold'))
    return(plotly_build(q))
  })
  output$geneHeatmap <- renderDataTable({
    if(is.null(subsetByGene()))
      return(NULL)
    
    geneNameColumn <- colnames(subsetByGene())[1]
    # Get hierarchical clustering of genes and samples
    m <- subsetByGene() %>% dplyr::select(-geneNameColumn) %>% as.matrix %>% apply(2, as.numeric)
    row.names(m) <- subsetByGene()[,geneNameColumn]
    return(m)
    
    # formatData <- subsetByGene() %>% 
    #   gather(sample, fpkm, -matches(geneNameColumn))
    # if(input$logFPKM == TRUE){
    #   plotY = "log10(fpkm+1)"
    #   labelY = "Log FPKM"
    # } else {
    #   plotY = "fpkm"
    #   labelY = "FPKM"
    # }
    # p <- ggplot(formatData, aes_string(x = geneNameColumn, y = plotY)) +
    #   geom_jitter(width = 0.1, alpha = 0.4) + geom_boxplot(outlier.colour = NA, fill = NA) +
    #   labs(x = "Gene", y = labelY) +
    #   theme_bw() +
    #   theme(text = element_text(size = 10), title = element_text(size = 12, face = 'bold'))
    # return(ggplotly(p))
  })
  
})
