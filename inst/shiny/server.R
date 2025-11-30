# ==============================================================================
# inst/shiny/server.R
# Server logic - Clustering Variables Shiny App
# ==============================================================================

library(shiny)
library(DT)
library(shinyjs)

# Load R6 classes
source("../../R/n_clusters.R")
source("../../R/kmeans.R")
source("../../R/acm_cah.R")
source("../../R/varclus.R")

# Load Shiny modules
source("modules/kmeans_module.R")
source("modules/acm_cah_module.R")
source("modules/varclus_module.R")

server <- function(input, output, session) {

  datasets <- reactiveValues(data = list())

  # ============================================================================
  # REACTIVE: Preview uploaded data
  # ============================================================================

  preview_data <- reactive({
    req(input$file1)

    tryCatch({
      file_ext <- tools::file_ext(input$file1$name)

      if (file_ext %in% c("xlsx", "xls")) {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          showNotification("Package 'readxl' required. Installing...", type = "warning", duration = 5)
          install.packages("readxl")
        }
        df <- as.data.frame(readxl::read_excel(input$file1$datapath))
      } else {
        df <- read.csv(input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote, dec = ",")
      }

      # Auto-convert character columns to numeric if possible
      for (col in names(df)) {
        if (is.character(df[[col]])) {
          test_numeric <- gsub(",", ".", df[[col]])
          if (all(grepl("^-?[0-9]+(\\.[0-9]+)?$", test_numeric, perl = TRUE) | is.na(test_numeric))) {
            df[[col]] <- as.numeric(gsub(",", ".", df[[col]]))
          }
        }
      }
      df
    }, error = function(e) {
      showNotification(paste("Read error:", e$message), type = "error", duration = 10)
      NULL
    })
  })

  # ============================================================================
  # OUTPUT: Display data preview
  # ============================================================================

  output$data_preview <- renderUI({
    if (is.null(input$file1)) {
      div(
        style = "text-align: center; padding: 60px 40px;",
        div(
          style = "display: inline-block; background: linear-gradient(135deg, #667eea08 0%, #764ba208 100%);
                   padding: 40px 60px; border-radius: 20px; border: 3px dashed #667eea40;",
          div(style = "font-size: 70px; margin-bottom: 20px;", "📊"),
          h2("Welcome to Data Import", style = "color: #2d3748; font-weight: 700; margin-bottom: 10px; font-size: 1.8em;"),
          p("Upload a CSV file from the sidebar to begin", style = "color: #718096; font-size: 1em; margin: 5px 0;"),
          p("Preview updates automatically as you adjust settings", style = "color: #a0aec0; font-size: 0.9em;")
        )
      )
    } else {
      df <- preview_data()

      if (is.null(df)) {
        return(
          div(
            style = "text-align: center; padding: 40px;",
            div(
              style = "display: inline-block; background: #fff5f5; padding: 30px 50px;
                       border-radius: 12px; border: 2px solid #fc8181;",
              div(style = "font-size: 60px; margin-bottom: 15px;", "⚠️"),
              h4("Unable to Read File", style = "color: #c53030; margin-bottom: 10px; font-weight: 700;"),
              p("Please adjust the separator and quote settings", style = "color: #742a2a; font-size: 0.95em; margin: 0;")
            )
          )
        )
      }

      n_quanti <- sum(sapply(df, is.numeric))
      n_quali  <- sum(sapply(df, function(x) !is.numeric(x)))
      file_size <- file.info(input$file1$datapath)$size
      size_text <- if (file_size < 1024) {
        paste(file_size, "B")
      } else if (file_size < 1024^2) {
        paste(round(file_size / 1024, 2), "KB")
      } else {
        paste(round(file_size / 1024^2, 2), "MB")
      }

      tagList(
        # Statistics Cards
        fluidRow(
          column(3,
                 div(class = "stat-card",
                     div(class = "stat-icon", "📁"),
                     div(class = "stat-label", "FILENAME"),
                     div(class = "stat-value", style = "font-size: 1.2em;", input$file1$name)
                 )
          ),
          column(3,
                 div(class = "stat-card",
                     div(class = "stat-icon", "📏"),
                     div(class = "stat-label", "DIMENSIONS"),
                     div(class = "stat-value", paste0(nrow(df), " × ", ncol(df)))
                 )
          ),
          column(3,
                 div(class = "stat-card",
                     div(class = "stat-icon", "📊"),
                     div(class = "stat-label", "QUANTITATIVE"),
                     div(class = "stat-value", n_quanti)
                 )
          ),
          column(3,
                 div(class = "stat-card",
                     div(class = "stat-icon", "🏷️"),
                     div(class = "stat-label", "QUALITATIVE"),
                     div(class = "stat-value", n_quali)
                 )
          )
        ),

        br(),

        # Data Table
        fluidRow(
          column(12,
                 div(
                   style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08);",
                   h4("📋 Data Preview (First 10 Rows)", style = "color: #2d3748; font-weight: 600; margin-top: 0;"),
                   div(class = "table-wrapper", tableOutput("contents"))
                 )
          )
        )
      )
    }
  })

  output$contents <- renderTable({
    req(preview_data())
    head(preview_data(), 10)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # ============================================================================
  # OBSERVER: Save dataset
  # ============================================================================

  save_msg <- reactiveVal(NULL)

  observeEvent(input$file1, { save_msg(NULL) })

  observeEvent(input$save_dataset, {
    req(preview_data())
    datasets$data[[input$file1$name]] <- preview_data()
    updateSelectInput(session, "dataset_choice", choices = names(datasets$data), selected = input$file1$name)
    save_msg(
      div(
        style = "color: #28a745; font-weight: bold; margin-top: 10px; padding: 10px;
                 background-color: #d4edda; border-radius: 5px; border: 1px solid #c3e6cb;",
        icon("check-circle"), HTML(" Dataset saved successfully!")
      )
    )
  })

  output$save_msg <- renderUI({ save_msg() })

  # ============================================================================
  # OBSERVER: Update variable selectors
  # ============================================================================

  observeEvent(input$dataset_choice, {
    req(datasets$data)
    df <- datasets$data[[input$dataset_choice]]
    req(df)

    cols <- names(df)
    num_vars <- names(df)[sapply(df, is.numeric)]

    updateSelectInput(session, "active_vars", choices = cols, selected = num_vars)
    updateSelectInput(session, "illustrative_vars", choices = cols)
  })

  observeEvent(input$active_vars, {
    req(input$dataset_choice)
    df <- datasets$data[[input$dataset_choice]]
    updateSelectInput(session, "illustrative_vars",
                      choices = setdiff(names(df), input$active_vars),
                      selected = intersect(input$illustrative_vars, setdiff(names(df), input$active_vars)))
  })

  observeEvent(input$illustrative_vars, {
    req(input$dataset_choice)
    df <- datasets$data[[input$dataset_choice]]
    updateSelectInput(session, "active_vars",
                      choices = setdiff(names(df), input$illustrative_vars),
                      selected = intersect(input$active_vars, setdiff(names(df), input$illustrative_vars)))
  })

  observeEvent(input$auto_k, {
    if (input$auto_k) {
      shinyjs::disable("num_k")  # Figer le slider
    } else {
      shinyjs::enable("num_k")   # Réactiver le slider
    }
  })

  # ============================================================================
  # REACTIVE: Main clustering engine
  # ============================================================================

  clustering_engine <- eventReactive(input$run_clustering, {

    req(input$dataset_choice, input$active_vars, input$algorithm)

    df <- datasets$data[[input$dataset_choice]]
    X <- df[, input$active_vars, drop = FALSE]

    # ===========================================================================
    # K-MEANS
    # ===========================================================================

    if (!is.null(input$algorithm) && input$algorithm == "kmeans") {

      numeric_cols <- sapply(X, is.numeric)
      quant_data <- X[, numeric_cols, drop = FALSE]

      if (ncol(quant_data) == 0) {
        showNotification("K-means requires at least one quantitative variable", type = "error", duration = 5)
        return(NULL)
      }

      if (ncol(quant_data) < 2) {
        showNotification("K-means requires at least 2 quantitative variables", type = "error", duration = 5)
        return(NULL)
      }

      withProgress(message = "K-means in progress...", {

        if (input$auto_k) {
          setProgress(0.2, detail = "Automatic k detection...")
          elbow_res <- kmeans_elbow(X_num = quant_data, k_range = 2:10, seed = NULL)
          k_opt <- elbow_res$optimal_k
          showNotification(paste("✅ K-means: optimal k detected =", k_opt), type = "message", duration = 5)
        } else {
          k_opt <- input$num_k
        }

        setProgress(0.5, detail = "Building model...")

        km <- KMeansVariablesQuant$new(k = k_opt, seed = 42)
        km$fit(quant_data)

        setProgress(1, detail = "Done!")
      })

      result <- list(model = km, type = "kmeans")

      if (!is.null(input$illustrative_vars) && length(input$illustrative_vars) > 0) {
        X_illust <- df[, input$illustrative_vars, drop = FALSE]
        X_illust_num <- X_illust[, sapply(X_illust, is.numeric), drop = FALSE]

        if (ncol(X_illust_num) > 0) {
          result$illustrative <- X_illust_num
        }
      }

      result

      # ===========================================================================
      # ACM-CAH
      # ===========================================================================

    } else if (!is.null(input$algorithm) && input$algorithm == "acm_cah") {

      quali_cols <- sapply(X, function(x) is.factor(x) || is.character(x))
      quali_data <- X[, quali_cols, drop = FALSE]

      if (ncol(quali_data) == 0) {
        showNotification("ACM-CAH requires at least one qualitative variable", type = "error", duration = 5)
        return(NULL)
      }

      quali_data[] <- lapply(quali_data, factor)

      withProgress(message = "ACM-CAH in progress...", {

        if (input$auto_k) {
          setProgress(0.2, detail = "Automatic k detection...")
          elbow_res <- acm_cah_elbow(
            X_quali = quali_data,
            method = input$acm_cah_method,
            k_max = 10
          )
          k_opt <- elbow_res$optimal_k
          showNotification(
            paste("✅ ACM-CAH: optimal k detected =", k_opt),
            type = "message",
            duration = 5
          )
        } else {
          k_opt <- input$num_k
        }

        setProgress(0.5, detail = "Building model...")

        cm <- ClustModalities$new(
          method = input$acm_cah_method,
          n_axes = if (input$acm_cah_method == "acm") input$acm_cah_n_axes else NULL
        )

        cm$fit(quali_data, k = k_opt)

        setProgress(1, detail = "Done!")
      })

      result <- list(model = cm, type = "acm_cah")

      # Illustrative QUALITATIVE variables
      if (!is.null(input$illustrative_vars) && length(input$illustrative_vars) > 0) {
        X_illust <- df[, input$illustrative_vars, drop = FALSE]
        X_illust_quali <- X_illust[, sapply(X_illust, function(x) is.factor(x) | is.character(x)), drop = FALSE]

        if (ncol(X_illust_quali) > 0) {
          X_illust_quali[] <- lapply(X_illust_quali, factor)
          result$illustrative <- X_illust_quali
        }

        # Also add QUANTITATIVE illustrative variables
        X_illust_quant <- X_illust[, sapply(X_illust, is.numeric), drop = FALSE]
        if (ncol(X_illust_quant) > 0) {
          result$illustrative_numeric <- X_illust_quant
        }
      }

      result

      # ===========================================================================
      # VARCLUS
      # ===========================================================================

    } else if (!is.null(input$algorithm) && input$algorithm == "varclus") {

      numeric_cols <- sapply(X, is.numeric)
      quant_data <- X[, numeric_cols, drop = FALSE]

      if (ncol(quant_data) == 0) {
        showNotification("VarClus requires at least one quantitative variable", type = "error", duration = 5)
        return(NULL)
      }

      if (ncol(quant_data) < 2) {
        showNotification("VarClus requires at least 2 quantitative variables", type = "error", duration = 5)
        return(NULL)
      }

      quant_data <- as.matrix(quant_data)
      mode(quant_data) <- "numeric"

      withProgress(message = "VarClus in progress...", {

        if (input$auto_k) {
          setProgress(0.2, detail = "Automatic k detection...")
          elbow_res <- varclus_elbow(X_num = quant_data, similarity = "pearson")
          k_opt <- elbow_res$optimal_k
          showNotification(paste("✅ VarClus: optimal k detected =", k_opt), type = "message", duration = 5)
        } else {
          k_opt <- input$num_k
        }

        setProgress(0.5, detail = "Building model...")

        vc <- VarClus$new(similarity = "pearson", n_clusters = k_opt)
        vc$fit(quant_data)

        setProgress(1, detail = "Done!")
      })

      result <- list(model = vc, type = "varclus")

      if (!is.null(input$illustrative_vars) && length(input$illustrative_vars) > 0) {
        X_illust <- df[, input$illustrative_vars, drop = FALSE]
        X_illust_num <- X_illust[, sapply(X_illust, is.numeric), drop = FALSE]

        if (ncol(X_illust_num) > 0) {
          result$illustrative <- as.matrix(X_illust_num)
          mode(result$illustrative) <- "numeric"
        }
      }

      result

    } else {
      showNotification("Algorithm not implemented", type = "warning")
      NULL
    }
  })

  # ============================================================================
  # MODULE SERVER: Call modules
  # ============================================================================

  kmeansServer("kmeans_tab", engine_reactive = clustering_engine)
  acm_cah_server(engine_reactive = clustering_engine)
  varclus_server(engine_reactive = clustering_engine)

  # ============================================================================
  # OUTPUT: Conditional clustering display
  # ============================================================================

  output$clustering_output <- renderUI({

    # If no clustering launched, display welcome message
    if (is.null(input$run_clustering) || input$run_clustering == 0) {
      return(
        div(
          style = "padding: 40px 30px;",

          # Welcome Card
          div(
            style = "background: white; padding: 35px; border-radius: 10px;
                     box-shadow: 0 4px 15px rgba(0,0,0,0.08); margin-bottom: 25px;
                     border-left: 5px solid #667eea; text-align: center;",

            div(style = "font-size: 4em; margin-bottom: 15px;", "🎯"),

            h2("Ready to Cluster Your Variables?",
               style = "color: #2d3748; font-weight: 700; margin: 0 0 15px 0;"),

            p("Configure your clustering parameters in the sidebar and click",
              tags$strong("'Run Clustering'"),
              "to begin the analysis.",
              style = "color: #4a5568; font-size: 1.1em; line-height: 1.6; margin: 0;")
          ),

          # Quick Guide
          div(
            style = "background: white; padding: 30px; border-radius: 10px;
                     box-shadow: 0 4px 15px rgba(0,0,0,0.08);",

            h3("📋 Quick Guide", style = "color: #2d3748; font-weight: 600; margin: 0 0 20px 0;"),

            div(
              style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px;",

              div(
                style = "background: linear-gradient(135deg, #667eea15 0%, #667eea05 100%);
                         padding: 20px; border-radius: 8px; border: 2px solid #667eea30;",
                div(style = "font-size: 2.5em; margin-bottom: 10px; text-align: center;", "1️⃣"),
                h5("Select Dataset", style = "color: #667eea; font-weight: 600; margin: 0 0 8px 0; text-align: center;"),
                p("Choose your dataset", style = "color: #4a5568; font-size: 0.9em; margin: 0; text-align: center;")
              ),

              div(
                style = "background: linear-gradient(135deg, #48bb7815 0%, #48bb7805 100%);
                         padding: 20px; border-radius: 8px; border: 2px solid #48bb7830;",
                div(style = "font-size: 2.5em; margin-bottom: 10px; text-align: center;", "2️⃣"),
                h5("Choose Variables", style = "color: #48bb78; font-weight: 600; margin: 0 0 8px 0; text-align: center;"),
                p("Select active/illustrative", style = "color: #4a5568; font-size: 0.9em; margin: 0; text-align: center;")
              ),

              div(
                style = "background: linear-gradient(135deg, #ed893615 0%, #ed893605 100%);
                         padding: 20px; border-radius: 8px; border: 2px solid #ed893630;",
                div(style = "font-size: 2.5em; margin-bottom: 10px; text-align: center;", "3️⃣"),
                h5("Select Method", style = "color: #ed8936; font-weight: 600; margin: 0 0 8px 0; text-align: center;"),
                p("KMeans, ACM-CAH, or VarClus", style = "color: #4a5568; font-size: 0.9em; margin: 0; text-align: center;")
              ),

              div(
                style = "background: linear-gradient(135deg, #9f7aea15 0%, #9f7aea05 100%);
                         padding: 20px; border-radius: 8px; border: 2px solid #9f7aea30;",
                div(style = "font-size: 2.5em; margin-bottom: 10px; text-align: center;", "4️⃣"),
                h5("Set Clusters", style = "color: #9f7aea; font-weight: 600; margin: 0 0 8px 0; text-align: center;"),
                p("Auto-detect or set K", style = "color: #4a5568; font-size: 0.9em; margin: 0; text-align: center;")
              )
            )
          )
        )
      )
    }

    # If clustering launched, display only selected tab
    req(input$run_clustering > 0)

    if (input$algorithm == "kmeans") {
      kmeansUI("kmeans_tab")
    } else if (input$algorithm == "acm_cah") {
      acm_cah_ui()
    } else if (input$algorithm == "varclus") {
      varclus_ui()
    }
  })

  observeEvent(input$algorithm, {
    if (!is.null(input$run_clustering) && input$run_clustering > 0) {
      # Force re-render when algo changes
      output$clustering_output <- renderUI({
        if (input$algorithm == "kmeans") {
          kmeansUI("kmeans_tab")
        } else if (input$algorithm == "acm_cah") {
          acm_cah_ui()
        } else if (input$algorithm == "varclus") {
          varclus_ui()
        }
      })
    }
  })
}
