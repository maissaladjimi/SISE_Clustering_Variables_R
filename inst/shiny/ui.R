# =============================================================================
# inst/shiny/ui.R
# Interface utilisateur de l'application Shiny
# =============================================================================

library(shiny)
library(shinyjs)
library(shinythemes)
library(DT)

# Charger les modules
source("modules/kmeans_module.R")
source("modules/acm_cah_module.R")
source("modules/varclus_module.R")

# =============================================================================
# INTERFACE UTILISATEUR (navbarPage)
# =============================================================================

ui <- navbarPage(
  "Clustering Variables",

  theme = shinytheme("cosmo"),

  # ===========================================================================
  # ONGLET 1 : HOME
  # ===========================================================================
  tabPanel(
    "Home",

    fluidRow(
      column(
        width = 12,
        div(
          style = "padding: 40px 30px 40px 30px; text-align: center;",

          h1("Welcome to Clustering Variables Application!",
             style = "margin: 0 0 20px 0; font-weight: 700; font-size: 2.5em; color: #2d3748;"),

          p("Discover relationships and patterns in your data by clustering variables using our ",
            tags$code("ClusteringVariables",
                      style = "background: #edf2f7; color: #c53030; padding: 4px 12px; border-radius: 4px; font-size: 0.9em; font-weight: 700;"),
            " R Package",
            style = "text-align: center; font-size: 1.2em; color: #4a5568; margin: 0; font-weight: 400;")
        )
      )
    ),

    # Main Content Section
    fluidRow(
      column(
        width = 10,
        offset = 1,

        # Introduction Card
        div(
          style = "background: white;
                   padding: 35px;
                   border-radius: 10px;
                   box-shadow: 0 4px 15px rgba(0,0,0,0.08);
                   margin-bottom: 35px;
                   border-left: 5px solid #667eea;",

          h3("Why Cluster Variables?",
             style = "color: #2d3748; font-weight: 600; margin-top: 0; margin-bottom: 20px;"),

          p("While traditional clustering groups data points (individuals), ",
            strong("variable clustering"), " reveals the hidden structure within your features themselves.
            By analyzing correlations and grouping variables with similar behaviors, you can:",
            style = "font-size: 17px; line-height: 1.8; color: #4a5568; margin-bottom: 20px;"),

          div(
            style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(250px, 1fr)); gap: 20px; margin-top: 25px;",

            div(
              style = "background: #f7fafc; padding: 20px; border-radius: 8px; border: 1px solid #e2e8f0;",
              div(style = "font-size: 28px; margin-bottom: 10px;", "🔍"),
              h5("Understand Structure", style = "color: #2d3748; font-weight: 600; margin-bottom: 8px;"),
              p("Reveal which variables behave similarly", style = "color: #718096; font-size: 14px; margin: 0;")
            ),

            div(
              style = "background: #f7fafc; padding: 20px; border-radius: 8px; border: 1px solid #e2e8f0;",
              div(style = "font-size: 28px; margin-bottom: 10px;", "🔄"),
              h5("Identify Redundancy", style = "color: #2d3748; font-weight: 600; margin-bottom: 8px;"),
              p("Detect redundant features in your dataset", style = "color: #718096; font-size: 14px; margin: 0;")
            ),

            div(
              style = "background: #f7fafc; padding: 20px; border-radius: 8px; border: 1px solid #e2e8f0;",
              div(style = "font-size: 28px; margin-bottom: 10px;", "⚡"),
              h5("Reduce Complexity", style = "color: #2d3748; font-weight: 600; margin-bottom: 8px;"),
              p("Enable feature selection for simpler models", style = "color: #718096; font-size: 14px; margin: 0;")
            )
          )
        ),

        # Methods Section
        div(
          style = "background: white;
                   padding: 35px;
                   border-radius: 10px;
                   box-shadow: 0 4px 15px rgba(0,0,0,0.08);
                   border-left: 5px solid #764ba2;",

          h3("Three Powerful Methods",
             style = "color: #2d3748; font-weight: 600; margin-top: 0; margin-bottom: 25px;"),

          p("The ", tags$code("ClusteringVariables", style = "background: #edf2f7; padding: 3px 8px; border-radius: 4px; font-size: 15px;"),
            " R package offers three distinct approaches built with R6 classes:",
            style = "font-size: 16px; color: #4a5568; margin-bottom: 30px;"),

          div(
            style = "display: flex; flex-direction: column; gap: 20px;",

            # KMeans
            div(
              style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                       padding: 25px;
                       border-radius: 8px;
                       color: white;",
              h4("K-means for Quantitative Variables", style = "margin: 0 0 10px 0; font-weight: 600;"),
              p("Groups variables based on correlation patterns", style = "margin: 0; opacity: 0.9;")
            ),

            # VarClus
            div(
              style = "background: linear-gradient(135deg, #48bb78 0%, #38a169 100%);
                       padding: 25px;
                       border-radius: 8px;
                       color: white;",
              h4("VarClus Hierarchical Clustering", style = "margin: 0 0 10px 0; font-weight: 600;"),
              p("Hierarchical approach for quantitative variables", style = "margin: 0; opacity: 0.9;")
            ),

            # ACM-CAH
            div(
              style = "background: linear-gradient(135deg, #ed8936 0%, #dd6b20 100%);
                       padding: 25px;
                       border-radius: 8px;
                       color: white;",
              h4("MCA + HAC for Qualitative Variables", style = "margin: 0 0 10px 0; font-weight: 600;"),
              p("Combines Multiple Correspondence Analysis with hierarchical clustering", style = "margin: 0; opacity: 0.9;")
            )
          )
        )
      )
    )
  ),

  # ===========================================================================
  # ONGLET 2 : DATA IMPORT
  # ===========================================================================
  tabPanel(
    "Data Import",

    tags$head(
      tags$style(HTML("
        .stat-card {
          background: white;
          padding: 15px;
          border-radius: 8px;
          box-shadow: 0 2px 6px rgba(0,0,0,0.08);
          text-align: center;
          transition: all 0.2s;
          height: 100%;
        }
        .stat-card:hover {
          transform: translateY(-3px);
          box-shadow: 0 4px 12px rgba(0,0,0,0.15);
        }
        .stat-icon {
          font-size: 2em;
          margin-bottom: 8px;
        }
        .stat-label {
          color: #718096;
          font-size: 0.8em;
          margin-bottom: 8px;
          font-weight: 500;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }
        .stat-value {
          color: #2d3748;
          font-size: 1.6em;
          font-weight: 700;
        }
        .table-wrapper {
          overflow-x: auto;
          background: #ffffff;
          border-radius: 8px;
          border: 1px solid #e2e8f0;
        }
        .table-wrapper table {
          width: 100%;
          border-collapse: collapse;
          font-size: 0.9em;
          margin: 0;
        }
        .table-wrapper table th {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 12px 15px;
          text-align: center;
          font-weight: 600;
          border: none;
          font-size: 0.9em;
        }
        .table-wrapper table td {
          padding: 10px 15px;
          border-bottom: 1px solid #e2e8f0;
          background: white;
          text-align: center;
          color: #2d3748;
        }
        .table-wrapper table tbody tr:hover td {
          background: #f7fafc;
        }
      "))
    ),

    div(
      class = "row",

      # ===== SIDEBAR : Upload et paramètres =====
      div(
        class = "col-sm-3 sidebar-fixed",

        # Header
        div(
          style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                   padding: 15px;
                   margin: -15px -15px 20px -15px;",
          h4(
            style = "color: white; margin: 0; font-weight: 600; font-size: 1.2em; text-align: center;",
            "Import Your Dataset"
          )
        ),

        # File Upload
        fileInput(
          "file1",
          "Choose File",
          multiple = FALSE,
          accept = c(
            "text/csv",
            "text/comma-separated-values",
            "text/plain",
            ".csv",
            ".txt",
            ".xlsx",
            ".xls"
          )
        ),

        # CSV/TXT Options (toujours affichées, pas de conditionalPanel)
        # Header checkbox
        checkboxInput("header", "Header", TRUE),

        # Separator (toujours visible)
        radioButtons(
          "sep",
          "Separator",
          choices = c(
            Comma = ",",
            Semicolon = ";",
            Tab = "\t"
          ),
          selected = ";"
        ),

        # Quote (toujours visible)
        selectInput(
          "quote",
          "Quote",
          choices = c(
            "None" = "",
            "Double Quote" = '"',
            "Single Quote" = "'"
          ),
          selected = '"'
        ),

        tags$div(
          style = "font-size: 0.85em; color: #666; font-style: italic; margin-top: -10px;",
          "Note: Excel files (.xlsx, .xls) ignore these settings"
        ),

        # Supprimer le conditionalPanel qui ne marchait pas
        # conditionalPanel ci-dessous supprimé
        tags$span(style = "display:none;",
                  checkboxInput("header_hidden", "Header", TRUE),
                  radioButtons("sep_hidden", "Separator", choices = c(Comma = ","), selected = ","),
                  selectInput("quote_hidden", "Quote", choices = c("None" = ""), selected = "")
        ),

        hr(),

        # Save Button
        actionButton(
          "save_dataset",
          "💾 Save Dataset",
          class = "btn-success btn-block",
          style = "font-weight: 600;"
        ),

        uiOutput("save_msg")
      ),

      # ===== MAIN PANEL : Preview =====
      div(
        class = "col-sm-9 main-panel-adjusted",

        tags$hr(style = "border: 0; border-top: 1px solid #ccc; margin: 20px 0;"),

        fluidRow(
          column(
            width = 12,
            h4("👁️ Preview of uploaded data"),
            uiOutput("data_preview")
          )
        )
      )
    )
  ),

  # ===========================================================================
  # ONGLET 3 : CLUSTERING
  # ===========================================================================
  tabPanel(
    "Clustering",

    tags$head(
      tags$style(HTML("
        /* Fix the navbar */
        .navbar {
          position: fixed !important;
          top: 0 !important;
          left: 0 !important;
          right: 0 !important;
          z-index: 1000 !important;
        }

        /* Add padding to body so content isn't hidden under navbar */
        body {
          padding-top: 51px !important;
        }

        .sidebar-fixed {
          position: fixed;
          top: 51px;
          bottom: 0;
          left: 0;
          width: 20%;
          overflow-y: auto;
          padding: 15px;
          background: #f8f9fa;
          z-index: 100;
        }

        .main-panel-adjusted {
          margin-left: 20%;
          width: 80%;
        }

        /* Scrollbar styling for sidebar */
        .sidebar-fixed::-webkit-scrollbar {
          width: 8px;
        }

        .sidebar-fixed::-webkit-scrollbar-track {
          background: #f1f1f1;
        }

        .sidebar-fixed::-webkit-scrollbar-thumb {
          background: #888;
          border-radius: 4px;
        }

        .sidebar-fixed::-webkit-scrollbar-thumb:hover {
          background: #555;
        }
      "))
    ),

    div(
      class = "row",

      # ===== SIDEBAR : Configuration =====
      div(
        class = "col-sm-3 sidebar-fixed",

        useShinyjs(),

        # Header
        div(
          style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                   padding: 15px;
                   margin: -15px -15px 10px -15px;",
          h4(
            style = "color: white; margin: 0; font-weight: 600; font-size: 1.2em; text-align: center;",
            "Choose Your Model"
          )
        ),

        # Dataset Selection
        div(
          style = "margin-bottom: 10px;",
          tags$label(
            style = "font-weight: 600; color: #2d3748; margin-bottom: 6px; display: block; font-size: 1em;",
            "📂 Dataset"
          ),
          selectInput(
            "dataset_choice",
            label = NULL,
            choices = NULL
          )
        ),

        # Variable Selectors
        div(
          style = "margin-bottom: 10px;",

          tags$label(
            style = "font-weight: 600; color: #2d3748; margin-bottom: 8px; display: block; font-size: 1em;",
            "🎯 Variables Selection"
          ),

          selectInput(
            "active_vars",
            label = tags$span("Active Variables", style = "font-size: 0.85em;"),
            choices = NULL,
            multiple = TRUE
          ),

          selectInput(
            "illustrative_vars",
            label = tags$span("Illustrative Variables", style = "font-size: 0.85em;"),
            choices = NULL,
            multiple = TRUE
          )
        ),

        # Clustering Algorithm
        div(
          style = "margin-bottom: 15px;",
          tags$label(
            style = "font-weight: 600; color: #2d3748; margin-bottom: 8px; display: block; font-size: 1em;",
            "📊 Clustering Method"
          ),

          tags$head(
            tags$style(HTML("
              #algorithm .radio label {
                font-size: 0.85em;
              }
            "))
          ),

          radioButtons(
            "algorithm",
            "Select Clustering Algorithm:",
            choices = c(
              "K-Means (quantitative variables)" = "kmeans",
              "ACM-CAH (qualitative variables)" = "acm_cah",
              "VarClus (quantitative variables)" = "varclus"
            ),
            selected = "kmeans"
          ),

          conditionalPanel(
            condition = "input.algorithm == 'acm_cah'",

            hr(),

            h5("ACM-CAH Parameters", style = "font-weight: 600; color: #2d3748;"),

            radioButtons(
              "acm_cah_method",
              "Method:",
              choices = c(
                "Dice Distance" = "dice",
                "MCA + CAH" = "acm"
              ),
              selected = "dice"
            ),

            conditionalPanel(
              condition = "input.acm_cah_method == 'acm'",
              numericInput(
                "acm_cah_n_axes",
                "Number of MCA axes:",
                value = 2,
                min = 2,
                max = 5
              )
            )
          )

        ),

        # Number of Clusters
        div(
          style = "margin-bottom: 18px;",

          tags$label(
            style = "font-weight: 600; color: #2d3748; margin-bottom: 8px; display: block; font-size: 1em;",
            "#️⃣ Number of Clusters K"
          ),

          checkboxInput(
            inputId = "auto_k",
            label = tags$span("Auto-detect optimal k", style = "font-size: 0.85em;"),
            value = FALSE
          ),

          sliderInput(
            inputId = "num_k",
            label = NULL,
            min = 2,
            max = 10,
            value = 3,
            step = 1
          )
        ),

        hr(),

        # Run Button
        actionButton(
          "run_clustering",
          "🚀 Run Clustering",
          class = "btn-primary btn-block",
          style = "font-weight: 600; padding: 12px;"
        )
      ),

      # ===== MAIN PANEL : Résultats =====
      div(
        class = "col-sm-9 main-panel-adjusted",
        uiOutput("clustering_output")  # Affichage conditionnel
      )
    )
  )
)
