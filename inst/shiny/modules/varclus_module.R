# ==============================================================================
# VARCLUS MODULE - UI & SERVER
# Module Shiny pour VarClus avec illustrative
# Compatible avec clustering_engine
# ==============================================================================

library(shiny)
library(DT)

# ==============================================================================
# UI MODULE
# ==============================================================================

varclus_ui <- function() {
  tagList(
    tabsetPanel(
      id = "varclus_tabs",
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
              h4("📈 VarClus Summary", style = "color: #2d3748; font-weight: 600; margin-bottom: 20px;"),
              verbatimTextOutput("varclus_summary"),
              hr(),
              h5("📋 Cluster Assignments"),
              DTOutput("varclus_clusters_table")
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
              plotOutput("varclus_dendro", height = "400px")
            )
          ),
          column(
            width = 6,
            div(
              style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              h5("🔥 Correlation Heatmap", style = "color: #2d3748; font-weight: 600;"),
              plotOutput("varclus_heatmap", height = "400px")
            )
          )
        ),

        br(),

        fluidRow(
          column(
            width = 12,
            div(
              style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              h5("📉 Elbow Method", style = "color: #2d3748; font-weight: 600;"),
              plotOutput("varclus_elbow", height = "400px")
            )
          )
        )
      ),

      # ========== TAB 3: DETAILED STATS ==========
      tabPanel(
        "📋 Detailed Stats",
        value = "stats",
        br(),

        fluidRow(
          column(
            width = 6,
            div(
              style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              h5("📊 Cluster Summary", style = "color: #2d3748; font-weight: 600;"),
              DTOutput("varclus_cluster_summary")
            )
          ),
          column(
            width = 6,
            div(
              style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              h5("🎯 Cluster Quality", style = "color: #2d3748; font-weight: 600;"),
              DTOutput("varclus_cluster_quality")
            )
          )
        ),

        br(),

        fluidRow(
          column(
            width = 12,
            div(
              style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              h5("📈 R² Details by Variable", style = "color: #2d3748; font-weight: 600;"),
              DTOutput("varclus_r2_details")
            )
          )
        )
      ),

      # ========== TAB 4: ILLUSTRATIVE VARIABLES ==========
      tabPanel(
        "🔍 Illustrative Variables",
        value = "illustrative",
        br(),

        fluidRow(
          column(
            width = 12,
            div(
              style = "background: #f7fafc; padding: 20px; border-radius: 8px; border-left: 4px solid #48bb78;",
              h4("📌 Illustrative Variables Analysis", style = "color: #2d3748; font-weight: 600; margin-top: 0;"),
              p("Illustrative variables are regressed on cluster latent components (PC1).
                 This shows how they relate to the cluster structure and allows visual interpretation via PCA.",
                style = "color: #4a5568; font-size: 15px;")
            )
          )
        ),

        br(),

        uiOutput("varclus_illustrative_content")
      )
    )
  )
}

# ==============================================================================
# SERVER MODULE (ADAPTÉ POUR engine_reactive)
# ==============================================================================

varclus_server <- function(engine_reactive, input = NULL, output = NULL, session = NULL) {

  # Si input/output/session ne sont pas fournis, utiliser le parent scope
  if (is.null(input)) input <- shiny::getDefaultReactiveDomain()$input
  if (is.null(output)) output <- shiny::getDefaultReactiveDomain()$output
  if (is.null(session)) session <- shiny::getDefaultReactiveDomain()

  # Extraire le modèle VarClus depuis clustering_engine
  varclus_model <- reactive({
    engine <- engine_reactive()
    if (!is.null(engine) && engine$type == "varclus") {
      engine$model
    } else {
      NULL
    }
  })

  # Extraire les variables illustratives depuis clustering_engine
  illustrative_data <- reactive({
    engine <- engine_reactive()
    if (!is.null(engine) && engine$type == "varclus") {
      engine$illustrative
    } else {
      NULL
    }
  })

  # Reactive: Summary complet
  summary_results <- reactive({
    req(varclus_model())
    varclus_model()$summary(print_output = FALSE)
  })

  # ========== OUTPUT: SUMMARY ==========
  output$varclus_summary <- renderPrint({
    req(varclus_model())
    varclus_model()$print()

    cat("\n")
    summ <- summary_results()

    cat("========================================\n")
    cat("  GLOBAL STATISTICS\n")
    cat("========================================\n")
    print(summ$global_stats, row.names = FALSE)

    cat("\n========================================\n")
    cat("  CLUSTER SUMMARY\n")
    cat("========================================\n")
    print(summ$cluster_summary, row.names = FALSE)

    cat("\n========================================\n")
    cat("  CLUSTER QUALITY (mean R² own)\n")
    cat("========================================\n")
    print(summ$cluster_quality, row.names = FALSE)
  })

  # ========== OUTPUT: CLUSTER TABLE ==========
  output$varclus_clusters_table <- renderDT({
    req(varclus_model())

    clusters_df <- varclus_model()$get_clusters_table()

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
  output$varclus_dendro <- renderPlot({
    req(varclus_model())
    tryCatch({
      dend_func <- varclus_model()$get_dendrogram()
      dend_func()
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2, col = "red")
    })
  })

  # ========== OUTPUT: HEATMAP ==========
  output$varclus_heatmap <- renderPlot({
    req(varclus_model())
    tryCatch({
      cor_matrix <- cor(varclus_model()$X)
      heatmap(cor_matrix,
              col = colorRampPalette(c("blue", "white", "red"))(100),
              scale = "none",
              main = "Correlation Heatmap",
              margins = c(10, 10))
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2, col = "red")
    })
  })

  # ========== OUTPUT: ELBOW ==========
  output$varclus_elbow <- renderPlot({
    req(varclus_model())
    tryCatch({
      if (!is.null(varclus_model()$plot_elbow)) {
        varclus_model()$plot_elbow()
      } else {
        plot.new()
        text(0.5, 0.5, "Elbow plot not available", cex = 1.2, col = "gray")
      }
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), cex = 1.2, col = "red")
    })
  })

  # ========== OUTPUT: CLUSTER SUMMARY ==========
  output$varclus_cluster_summary <- renderDT({
    req(summary_results())

    datatable(
      summary_results()$cluster_summary,
      options = list(
        pageLength = 10,
        dom = 't',
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })

  # ========== OUTPUT: CLUSTER QUALITY ==========
  output$varclus_cluster_quality <- renderDT({
    req(summary_results())

    datatable(
      summary_results()$cluster_quality,
      options = list(
        pageLength = 10,
        dom = 't',
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatStyle(
        'mean_R2_own',
        background = styleColorBar(c(0, 1), 'lightgreen'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })

  # ========== OUTPUT: R² DETAILS ==========
  output$varclus_r2_details <- renderDT({
    req(summary_results())

    datatable(
      summary_results()$R2_details,
      options = list(
        pageLength = 20,
        dom = 'Bfrtip',
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    )
  })

  # ========== ILLUSTRATIVE VARIABLES (NOUVEAU!) ==========

  # Reactive: Calculer illustrative si variables présentes
  illustrative_results <- reactive({
    req(varclus_model())

    illust_data <- illustrative_data()

    if (!is.null(illust_data) && ncol(illust_data) > 0) {
      tryCatch({
        varclus_model()$illustrative(illust_data)
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
  output$varclus_illustrative_content <- renderUI({
    if (is.null(illustrative_results())) {
      div(
        style = "text-align: center; padding: 40px; color: #718096;",
        icon("info-circle", class = "fa-3x", style = "color: #cbd5e0;"),
        br(), br(),
        h5("No illustrative variables selected", style = "color: #4a5568;"),
        p("Select quantitative illustrative variables in the sidebar to see their analysis.",
          style = "font-size: 14px;")
      )
    } else {
      tagList(
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              h5("📊 Regression on Cluster Components", style = "color: #2d3748; font-weight: 600;"),
              p("Each illustrative variable is regressed on the latent components (PC1) of all clusters.",
                style = "color: #718096; font-size: 13px; margin-bottom: 15px;"),
              DTOutput("varclus_illustrative_table")
            )
          )
        ),

        br(),

        fluidRow(
          column(
            width = 12,
            div(
              style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              h5("📈 PCA Correlation Circle", style = "color: #2d3748; font-weight: 600;"),
              p("Active variables (blue) and illustrative variables (red) projected on PC1-PC2 plane.",
                style = "color: #718096; font-size: 13px; margin-bottom: 15px;"),
              plotOutput("varclus_illustrative_plot", height = "500px")
            )
          )
        )
      )
    }
  })

  # Table illustrative
  output$varclus_illustrative_table <- renderDT({
    req(illustrative_results())

    datatable(
      illustrative_results()$table,
      options = list(
        pageLength = 20,
        dom = 'Bfrtip',
        scrollX = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe'
    ) %>%
      formatStyle(
        'R2',
        background = styleColorBar(c(0, 1), 'lightblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })

  # Plot illustrative
  output$varclus_illustrative_plot <- renderPlot({
    req(illustrative_results())
    illustrative_results()$plot()
  })

  # Return model for external use
  return(varclus_model)
}
