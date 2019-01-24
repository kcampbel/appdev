library(shiny)

# Define UI for miles per gallon application
shinyUI(fluidPage(
  
  # Application title
  headerPanel("Expression Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fpkm_input", "Choose FPKM matrix input"),
      uiOutput("gene_input")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Input Matrix",
                 dataTableOutput("fpkm_matrix"))
        # tabPanel("Individual Genes",
        #          plotlyOutput("boxplots"))
      )
    )
  )
))