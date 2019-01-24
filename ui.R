library(shiny)

# Define UI for miles per gallon application
shinyUI(navbarPage("Expression Explorer",
                   # Displays input data frames
                   #####
                   tabPanel("Data",
                            sidebarLayout(
                              # Sidebar: Inputs
                              sidebarPanel(
                                fileInput("fpkm_input", "Choose FPKM matrix", accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                radioButtons("fpkm_delim", "Select delimiter:", choices = list("Tab" = "\t", "Comma" = ",", "Space" = " "), selected = "\t", inline = TRUE),
                                fileInput("sample_input", "Choose Sample metadata"),
                                radioButtons("sample_delim", "Select delimiter:", choices = list("Tab" = "\t", "Comma" = ",", "Space" = " "), selected = "\t", inline = TRUE),
                                fileInput("clin_input", "Choose Clinial metadata"),
                                radioButtons("clin_delim", "Select delimiter:", choices = list("Tab" = "\t", "Comma" = ",", "Space" = " "), selected = "\t", inline = TRUE)
                              ),
                              # Main panel: Dataframes
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Gene Expression Matrix",
                                           dataTableOutput("fpkm_matrix")),
                                  helpText("Note: Make sure that your expression matrix is formatted appropriately. The first column should contain gene names (each row representing a single gene identifier). Each column is associated with an individual sample (indicated by the column names)."),
                                  tabPanel("Sample Information",
                                           dataTableOutput("sample_matrix")),
                                  tabPanel("Clinical Information",
                                           dataTableOutput("clin_matrix"))
                                )
                              )
                            )
                            ),
                   #####
                   # Explore without performing statistical analysis
                   #####
                   tabPanel("Explore",
                            sidebarLayout(
                              # Side bar options: Select genes
                              # Color by clinical feature
                              sidebarPanel(
                                selectizeInput("gene_input", "Select up to 10 gene identifiers:", NULL, multiple = TRUE, options = list(maxOptions = 10)),
                                checkboxInput("logFPKM", "Log Y axis: ", value = TRUE)
                              ),
                              mainPanel(
                                plotlyOutput("singleGeneBoxplots")
                              )
                            )
                   )
                   ))