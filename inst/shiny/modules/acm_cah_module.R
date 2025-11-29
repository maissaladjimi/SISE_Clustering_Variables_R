# ==============================================================================
# ACM-CAH MODULE - UI & SERVER
# Module Shiny pour ACM-CAH avec illustrative
# Compatible avec clustering_engine
# ==============================================================================

library(shiny)
library(DT)

# ==============================================================================
# UI MODULE
# ==============================================================================

acm_cah_ui <- function() {
  tagList(
    tabsetPanel(
      id = "acm_cah_tabs",
      type = "pills",

      # ========== TAB 1: SUMMARY ==========
      tabPanel(
        "📊 Summary",
        value = "summary",
        br(),

        fluidRow(
          column(
            width = 12,
            div(
              style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              h4("📈 Clustering Summary", style = "color: #2d3748; font-weight: 600; margin-bottom: 20px;"),
              verbatimTextOutput("acm_cah_summary"),
              hr(),
              h5("📋 Cluster Assignments (Modalities)"),
              DTOutput("acm_cah_clusters_table")
            )
          )
        )
      ),

      # ========== TAB 2: VISUALIZATIONS ==========
      tabPanel(
        "📈 Visualizations",
        value = "viz",
        br(),

        fluidRow(
          column(
            width = 6,
            div(
              style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              h5("🌳 Dendrogram", style = "color: #2d3748; font-weight: 600;"),
              plotOutput("acm_cah_dendro", height = "400px")
            )
          ),
          column(
            width = 6,
            div(
              style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              h5("📉 Elbow Plot", style = "color: #2d3748; font-weight: 600;"),
              plotOutput("acm_cah_elbow", height = "400px")
            )
          )
        ),

        br(),

        # Factorial map (ACM only)
        conditionalPanel(
          condition = "input.acm_cah_method == 'acm'",
          fluidRow(
            column(
              width = 12,
              div(
                style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                h5("🗺️ Factorial Map (MCA)", style = "color: #2d3748; font-weight: 600;"),
                plotOutput("acm_cah_factmap", height = "500px")
              )
            )
          )
        )
      ),

      # ========== TAB 3: MCA DETAILS (ACM only) ==========
      tabPanel(
        "🔬 MCA Details",
        value = "mca",
        br(),

        conditionalPanel(
          condition = "input.acm_cah_method == 'acm'",

          fluidRow(
            column(
              width = 6,
              div(
                style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                h5("📊 Scree Plot", style = "color: #2d3748; font-weight: 600;"),
                plotOutput("acm_cah_scree", height = "350px")
              )
            ),
            column(
              width = 6,
              div(
                style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                h5("📈 Contributions (Dimension 1)", style = "color: #2d3748; font-weight: 600;"),
                plotOutput("acm_cah_contrib", height = "350px")
              )
            )
          )
        ),

        conditionalPanel(
          condition = "input.acm_cah_method != 'acm'",
          div(
            style = "text-align: center; padding: 60px; color: #718096;",
            icon("info-circle", class = "fa-3x", style = "color: #cbd5e0;"),
            br(), br(),
            h5("MCA details are only available for ACM method", style = "color: #4a5568;"),
            p("Switch to 'MCA + CAH' method in the sidebar to see MCA-specific visualizations.",
              style = "font-size: 14px;")
          )
        )
      ),

      # ========== TAB 4: ILLUSTRATIVE VARIABLES (NOUVEAU!) ==========
      tabPanel(
        "🔍 Illustrative Variables",
        value = "illustrative",
        br(),

        fluidRow(
          column(
            width = 12,
            div(
              style = "background: #f7fafc; padding: 20px; border-radius: 8px; border-left: 4px solid #ed8936;",
              h4("📌 Illustrative Variables Analysis", style = "color: #2d3748; font-weight: 600; margin-top: 0;"),
              p("Project qualitative illustrative variables onto the existing clusters.
                 For DICE: average Dice² distance to cluster members.
                 For ACM: euclidean distance to cluster barycenters in factorial space.",
                style = "color: #4a5568; font-size: 15px;")
            )
          )
        ),

        br(),

        uiOutput("acm_cah_illustrative_content")
      ),

      # ========== TAB 5: ILLUSTRATIVE NUMERIC (NOUVEAU!) ==========
      tabPanel(
        "🔢 Illustrative Numeric",
        value = "illustrative_numeric",
        br(),

        fluidRow(
          column(
            width = 12,
            div(
              style = "background: #f7fafc; padding: 20px; border-radius: 8px; border-left: 4px solid #4299e1;",
              h4("📊 Quantitative Illustrative Variables", style = "color: #2d3748; font-weight: 600; margin-top: 0;"),
              p("Project quantitative variables on MCA factorial space.",
                style = "color: #4a5568; font-size: 15px;")
            )
          )
        ),

        br(),

        uiOutput("acm_cah_illustrative_numeric_content")
      ),
    )
  )
}

# ==============================================================================
# SERVER MODULE (ADAPTÉ POUR engine_reactive)
# ==============================================================================

acm_cah_server <- function(engine_reactive, input = NULL, output = NULL, session = NULL) {

  # Si input/output/session ne sont pas fournis, utiliser le parent scope
  if (is.null(input)) input <- shiny::getDefaultReactiveDomain()$input
  if (is.null(output)) output <- shiny::getDefaultReactiveDomain()$output
  if (is.null(session)) session <- shiny::getDefaultReactiveDomain()

  # Extraire le modèle ACM-CAH depuis clustering_engine
  acm_cah_model <- reactive({
    engine <- engine_reactive()
    if (!is.null(engine) && engine$type == "acm_cah") {
      engine$model
    } else {
      NULL
    }
  })

  # Extraire les variables illustratives depuis clustering_engine
  illustrative_data <- reactive({
    engine <- engine_reactive()
    if (!is.null(engine) && engine$type == "acm_cah") {
      engine$illustrative
    } else {
      NULL
    }
  })

  # Extraire les variables illustratives QUANTITATIVES
  illustrative_numeric_data <- reactive({
    engine <- engine_reactive()
    if (!is.null(engine) && engine$type == "acm_cah") {
      engine$illustrative_numeric
    } else {
      NULL
    }
  })

  # Récupérer k depuis le modèle
  k_value <- reactive({
    req(acm_cah_model())
    acm_cah_model()$k
  })

  # Récupérer méthode depuis le modèle
  method_value <- reactive({
    req(acm_cah_model())
    acm_cah_model()$method
  })

  # Reactive: Summary complet
  summary_results <- reactive({
    req(acm_cah_model())
    acm_cah_model()$summary(print_output = FALSE)
  })

  # ========== OUTPUT: SUMMARY ==========
  output$acm_cah_summary <- renderPrint({
    req(acm_cah_model())
    acm_cah_model()$print()

    cat("\n")
    summ <- summary_results()

    cat("========================================\n")
    cat("  BASIC STATISTICS\n")
    cat("========================================\n")
    print(summ$basic_stats, row.names = FALSE)

    if (!is.null(summ$acm_stats)) {
      cat("\n========================================\n")
      cat("  MCA EIGENVALUES (top 5)\n")
      cat("========================================\n")
      print(head(summ$acm_stats, 5), row.names = FALSE)
    }

    if (!is.null(summ$cluster_stats)) {
      cat("\n========================================\n")
      cat("  CLUSTER STATISTICS\n")
      cat("========================================\n")
      print(summ$cluster_stats, row.names = FALSE)
    }
  })

  # ========== OUTPUT: CLUSTER TABLE ==========
  output$acm_cah_clusters_table <- renderDT({
    req(acm_cah_model())

    clusters_df <- acm_cah_model()$get_clusters_table()

    datatable(
      clusters_df,
      options = list(
        pageLength = 20,
        dom = 'Bfrtip',
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatStyle(
        'cluster',
        backgroundColor = styleEqual(
          unique(clusters_df$cluster),
          rainbow(length(unique(clusters_df$cluster)))
        )
      )
  })

  # ========== OUTPUT: DENDROGRAM ==========
  output$acm_cah_dendro <- renderPlot({
    req(acm_cah_model())
    req(k_value())
    tryCatch({
      acm_cah_model()$plot_dendrogram(k = k_value())
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2, col = "red")
    })
  })

  # ========== OUTPUT: ELBOW ==========
  output$acm_cah_elbow <- renderPlot({
    req(acm_cah_model())
    tryCatch({
      acm_cah_model()$plot_elbow(k_max = 10)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2, col = "red")
    })
  })

  # ========== OUTPUT: FACTORIAL MAP (ACM only) ==========
  output$acm_cah_factmap <- renderPlot({
    req(acm_cah_model())
    req(method_value() == "acm")
    req(k_value())

    tryCatch({
      acm_cah_model()$plot_factorial_map(dims = c(1, 2), k = k_value())
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2, col = "red")
    })
  })

  # ========== OUTPUT: SCREE PLOT (ACM only) ==========
  output$acm_cah_scree <- renderPlot({
    req(acm_cah_model())
    req(method_value() == "acm")

    tryCatch({
      acm_cah_model()$plot_scree(cumulative = FALSE)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2, col = "red")
    })
  })

  # ========== OUTPUT: CONTRIBUTIONS (ACM only) ==========
  output$acm_cah_contrib <- renderPlot({
    req(acm_cah_model())
    req(method_value() == "acm")

    tryCatch({
      acm_cah_model()$plot_contrib(dim = 1, top = 15)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2, col = "red")
    })
  })

  # ========== ILLUSTRATIVE VARIABLES (NOUVEAU!) ==========

  # Reactive: Calculer illustrative si variables présentes
  illustrative_results <- reactive({
    req(acm_cah_model())

    illust_data <- illustrative_data()

    if (!is.null(illust_data) && ncol(illust_data) > 0) {
      tryCatch({
        acm_cah_model()$illustrative(illust_data, plot = FALSE)
      }, error = function(e) {
        showNotification(
          paste("Error in illustrative:", e$message),
          type = "error",
          duration = 10
        )
        NULL
      })
    } else {
      NULL
    }
  })

  # UI dynamique pour illustrative
  output$acm_cah_illustrative_content <- renderUI({
    if (is.null(illustrative_results())) {
      div(
        style = "text-align: center; padding: 40px; color: #718096;",
        icon("info-circle", class = "fa-3x", style = "color: #cbd5e0;"),
        br(), br(),
        h5("No illustrative variables selected", style = "color: #4a5568;"),
        p("Select qualitative illustrative variables in the sidebar to see their projection onto the clusters.",
          style = "font-size: 14px;")
      )
    } else {
      tagList(
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              h5("📊 Illustrative Modalities - Distance to Clusters", style = "color: #2d3748; font-weight: 600;"),
              p(if (method_value() == "dice") {
                "Average Dice² distance to cluster members (lower = closer)"
              } else {
                "Euclidean distance to cluster barycenters in MCA space (lower = closer)"
              }, style = "color: #718096; font-size: 13px; margin-bottom: 15px;"),
              DTOutput("acm_cah_illustrative_table")
            )
          )
        ),

        br(),

        fluidRow(
          column(
            width = 12,
            div(
              style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              h5("📈 Distance Visualization", style = "color: #2d3748; font-weight: 600;"),
              plotOutput("acm_cah_illustrative_plot", height = "500px")
            )
          )
        )
      )
    }
  })

  # Table illustrative
  output$acm_cah_illustrative_table <- renderDT({
    req(illustrative_results())

    datatable(
      illustrative_results()$table,
      options = list(
        pageLength = 15,
        dom = 'Bfrtip',
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatStyle(
        'cluster_assigned',
        backgroundColor = styleEqual(
          unique(illustrative_results()$table$cluster_assigned),
          rainbow(length(unique(illustrative_results()$table$cluster_assigned)))
        )
      )
  })

  # Plot illustrative
  output$acm_cah_illustrative_plot <- renderPlot({
    req(illustrative_results())
    illustrative_results()$plot()
  })

  # ========== ILLUSTRATIVE NUMERIC (NOUVEAU!) ==========

  illustrative_numeric_results <- reactive({
    req(acm_cah_model())
    req(method_value() == "acm")

    illust_num <- illustrative_numeric_data()

    if (!is.null(illust_num) && ncol(illust_num) > 0) {
      tryCatch({
        acm_cah_model()$illustrative_numeric(illust_num)
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
        NULL
      })
    } else {
      NULL
    }
  })

  output$acm_cah_illustrative_numeric_content <- renderUI({
    if (method_value() != "acm") {
      div(
        style = "text-align: center; padding: 40px; color: #718096;",
        icon("info-circle", class = "fa-3x", style = "color: #cbd5e0;"),
        br(), br(),
        h5("Only available for ACM method", style = "color: #4a5568;")
      )
    } else if (is.null(illustrative_numeric_results())) {
      div(
        style = "text-align: center; padding: 40px; color: #718096;",
        icon("info-circle", class = "fa-3x", style = "color: #cbd5e0;"),
        br(), br(),
        h5("No quantitative illustrative variables", style = "color: #4a5568;")
      )
    } else {
      fluidRow(
        column(
          width = 12,
          div(
            style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
            h5("📈 Correlation Circle", style = "color: #2d3748; font-weight: 600;"),
            plotOutput("acm_cah_illustrative_numeric_plot", height = "500px")
          )
        )
      )
    }
  })

  output$acm_cah_illustrative_numeric_plot <- renderPlot({
    req(illustrative_numeric_results())
    illustrative_numeric_results()
  })

  # Return model for external use
  return(acm_cah_model)
}
