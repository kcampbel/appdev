library(shiny)

# Define UI for miles per gallon application
shinyUI(navbarPage("Expression Explorer",
                   # Displays input data frames
                   #####
                   tabPanel("Data",
                            sidebarLayout(
                              # Sidebar: Inputs
                              sidebarPanel(
                                h3("Gene Expression Data"),
                                fileInput("fpkm_input", "Upload FPKM matrix", accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                radioButtons("fpkm_delim", "Select delimiter:", choices = list("Tab" = "\t", "Comma" = ",", "Space" = " "), selected = "\t", inline = TRUE),
                                #
                                h3("Sample Information"),
                                fileInput("sample_input", "Upload sample metadata"),
                                radioButtons("sample_delim", "Select delimiter:", choices = list("Tab" = "\t", "Comma" = ",", "Space" = " "), selected = "\t", inline = TRUE),
                                selectInput("sampleIdentifier", "Sample ID: ", NULL, multiple = FALSE),
                                #
                                h3("Clinical Information"),
                                fileInput("clin_input", "Upload clinical metadata"),
                                radioButtons("clin_delim", "Select delimiter:", choices = list("Tab" = "\t", "Comma" = ",", "Space" = " "), selected = "\t", inline = TRUE),
                                selectInput("individualIdentifier1", "Map Individual ID (from Sample Information):", NULL, multiple = FALSE),
                                selectInput("individualIdentifier2", "to Individual ID (from Clinical Information):", NULL, multiple = FALSE)
                              ),
                              # Main panel: Dataframes
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Gene Expression Matrix",
                                           helpText("Note: Make sure that your expression matrix is formatted appropriately. The first column should contain gene names (each row representing a single gene identifier). Each column is associated with an individual sample (indicated by the column names)."),
                                           dataTableOutput("fpkm_matrix")),
                                  tabPanel("Sample Information",
                                           helpText("Note: One column must correspond to the column names (Sample Identifers) indicated in the FPKM matrix."),
                                           dataTableOutput("sample_matrix")),
                                  tabPanel("Clinical Information",
                                           helpText("Note: There must be at least one column name shared by the Sample and Clinical Metadata."),
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
                                h3("Sample Specification"),
                                selectInput("colorGroup", "Color: ", NULL, multiple = FALSE),
                                h3("Gene Selection"),
                                selectizeInput("gene_input", "Select up to 20 gene identifiers:", NULL, multiple = TRUE, options = list(maxOptions = 20)),
                                checkboxInput("logFPKM", "Log FPKM", value = TRUE),
                                selectInput("x_corrScatter", "X axis", NULL, multiple = FALSE),
                                selectInput("y_corrScatter", "Y axis", NULL, multiple = FALSE),
                                radioButtons("corrMethod", "Correlation method: ", choices = list("Pearson" = "pearson", "Kendall" = "kendall", "Spearman" = "spearman"), selected = "pearson", inline = TRUE)
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Gene Expression Distributions",
                                           plotlyOutput("geneBoxplots"),
                                           plotlyOutput("dualGeneHistograms")),
                                  tabPanel("Correlation",
                                           plotlyOutput("corrScatter"))
                                )
                              )
                            )
                   )
                   ))