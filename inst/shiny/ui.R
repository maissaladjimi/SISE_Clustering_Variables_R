library(shiny)
library(bslib)

# Define UI for data upload app ----
ui <- fluidPage(

  theme = shinythemes::shinytheme("united"),
  shinyjs::useShinyjs(),

  # App title ----
  titlePanel("ClusteringVariables"),

  # Sidebar layout ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      width = 3,
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),

      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),

      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),

      # ---- Variable selectors (always visible) ----
      selectInput("active_vars", "Choose Active Variables",
                  choices = NULL,
                  multiple = TRUE),

      selectInput("illustrative_vars", "Choose Illustrative Variables",
                  choices = NULL,
                  multiple = TRUE),


      # Input: Choose clustering algorithm ----
      radioButtons(
        inputId = "algorithm",
        label = "Choose a clustering model:",
        choices = c(
          "KMeans" = "kmeans",
          "VarClus" = "varclus",
          "MCA&CAH" = "acm_cah"
        ),
        selected = "kmeans"
      ),

      tags$div(
        tags$strong("Choose number of clusters (k):"),
        style = "margin-bottom: 2px;"
      ),

      # --- Auto K checkbox ---
      checkboxInput(
        inputId = "auto_k",
        label = "auto",
        value = TRUE
      ),

      # --- Manual number of clusters slider ---
      sliderInput(
        inputId = "num_k",
        label = NULL,
        min = 2,
        max = 15,
        value = 3,
        step = 1
      ),

      actionButton("run_clustering", "Run Clustering")

  ),

  # Main panel for displaying outputs----
  mainPanel(
    uiOutput("main_content")
   )
  )
)
