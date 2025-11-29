# ==============================================================================
# KMEANS MODULE - UI & SERVER
# Module Shiny pour K-means avec illustrative
# Compatible avec clustering_engine
# ==============================================================================

library(shiny)
library(DT)

# ==============================================================================
# UI MODULE
# ==============================================================================

kmeansUI <- function(id) {
  ns <- NS(id)

  tagList(
    # Tabs pour organiser les résultats
    tabsetPanel(
      id = ns("kmeans_tabs"),
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
              verbatimTextOutput(ns("kmeans_summary")),
              hr(),
              h5("📋 Cluster Assignments"),
              DTOutput(ns("kmeans_clusters_table"))
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
              h5("🎯 Correlation Circle", style = "color: #2d3748; font-weight: 600;"),
              plotOutput(ns("kmeans_corr_circle"), height = "400px"),
              hr(),
              h6("With Illustrative Variables", style = "color: #718096;"),
              plotOutput(ns("kmeans_corr_circle_illust"), height = "400px")
            )
          ),
          column(
            width = 6,
            div(
              style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              h5("📊 Biplot", style = "color: #2d3748; font-weight: 600;"),
              plotOutput(ns("kmeans_biplot"), height = "400px")
            )
          )
        ),

        br(),

        fluidRow(
          column(
            width = 12,
            div(
              style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              h5("📉 Elbow Method (optimal k)", style = "color: #2d3748; font-weight: 600;"),
              plotOutput(ns("kmeans_elbow"), height = "400px")
            )
          )
        )
      ),

      # ========== TAB 3: ILLUSTRATIVE VARIABLES (NOUVEAU!) ==========
      tabPanel(
        "🔍 Illustrative Variables",
        value = "illustrative",
        br(),

        fluidRow(
          column(
            width = 12,
            div(
              style = "background: #f7fafc; padding: 20px; border-radius: 8px; border-left: 4px solid #667eea;",
              h4("📌 Illustrative Variables Analysis", style = "color: #2d3748; font-weight: 600; margin-top: 0;"),
              p("Illustrative variables are projected onto the existing clusters without modifying them.
                 This allows you to understand how additional variables relate to the cluster structure.",
                style = "color: #4a5568; font-size: 15px;")
            )
          )
        ),

        br(),

        # Affichage conditionnel
        uiOutput(ns("illustrative_content"))
      ),

      # ========== TAB 4: DETAILED STATISTICS ==========
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
              DTOutput(ns("cluster_summary_table"))
            )
          ),
          column(
            width = 6,
            div(
              style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              h5("🔗 Correlations Between Latent Components", style = "color: #2d3748; font-weight: 600;"),
              verbatimTextOutput(ns("cor_latent"))
            )
          )
        ),

        br(),

        fluidRow(
          column(
            width = 12,
            div(
              style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              h5("📈 R² Matrix (Variables × Clusters)", style = "color: #2d3748; font-weight: 600;"),
              DTOutput(ns("r2_matrix_table"))
            )
          )
        )
      )
    )
  )
}

# ==============================================================================
# SERVER MODULE (ADAPTÉ POUR engine_reactive)
# ==============================================================================

kmeansServer <- function(id, engine_reactive) {
  moduleServer(id, function(input, output, session) {

    # Extraire le modèle K-means depuis clustering_engine
    kmeans_model <- reactive({
      engine <- engine_reactive()
      if (!is.null(engine) && engine$type == "kmeans") {
        engine$model
      } else {
        NULL
      }
    })

    # Extraire les variables illustratives depuis clustering_engine
    illustrative_data <- reactive({
      engine <- engine_reactive()
      if (!is.null(engine) && engine$type == "kmeans") {
        engine$illustrative
      } else {
        NULL
      }
    })

    # Reactive: Summary complet
    summary_results <- reactive({
      req(kmeans_model())
      kmeans_model()$summary(print_output = FALSE)
    })

    # ========== OUTPUT: SUMMARY ==========
    output$kmeans_summary <- renderPrint({
      req(kmeans_model())
      kmeans_model()$print()

      cat("\n")
      cat("========================================\n")
      cat("  QUALITY METRICS\n")
      cat("========================================\n")

      summ <- summary_results()
      print(summ$global_quality, row.names = FALSE)
    })

    # ========== OUTPUT: CLUSTER TABLE ==========
    output$kmeans_clusters_table <- renderDT({
      req(kmeans_model())

      clusters_df <- kmeans_model()$get_clusters_table()

      datatable(
        clusters_df,
        options = list(
          pageLength = 15,
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

    # ========== OUTPUT: CORRELATION CIRCLE ==========
    output$kmeans_corr_circle <- renderPlot({
      req(kmeans_model())
      tryCatch({
        kmeans_model()$plot_correlation_circle(dims = c(1, 2))
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), cex = 1.2, col = "red")
      })
    })

    # Projection des variables illustratives si présentes
    output$kmeans_corr_circle_illust <- renderPlot({
      req(kmeans_model())
      illust_data <- illustrative_data()

      if (!is.null(illust_data) && ncol(illust_data) > 0) {
        tryCatch({
          # Obtenir les scores des individus sur PC1 et PC2
          scores <- kmeans_model()$X_std %*% kmeans_model()$pca$rotation[, 1:2]

          # Calculer corrélations des illustratives avec PC1 et PC2
          cor_illust <- cor(illust_data, scores)

          # Plot
          plot(cor_illust, xlim = c(-1, 1), ylim = c(-1, 1),
               xlab = "PC1", ylab = "PC2",
               main = "Illustrative Variables on Correlation Circle",
               pch = 17, col = "red", cex = 1.5)

          # Cercle
          theta <- seq(0, 2*pi, length = 100)
          lines(cos(theta), sin(theta), col = "gray50")

          # Labels
          text(cor_illust[,1], cor_illust[,2],
               labels = rownames(cor_illust),
               col = "red", pos = 3, cex = 0.8)

          # Axes
          abline(h = 0, v = 0, col = "gray70", lty = 2)

        }, error = function(e) {
          plot.new()
          text(0.5, 0.5, paste("Error:", e$message), cex = 1.2, col = "red")
        })
      } else {
        plot.new()
        text(0.5, 0.5, "No illustrative variables", cex = 1.2, col = "gray")
      }
    })

    # ========== OUTPUT: BIPLOT ==========
    output$kmeans_biplot <- renderPlot({
      req(kmeans_model())
      tryCatch({
        kmeans_model()$plot_biplot(dims = c(1, 2))
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), cex = 1.2, col = "red")
      })
    })

    # ========== OUTPUT: ELBOW ==========
    output$kmeans_elbow <- renderPlot({
      req(kmeans_model())
      tryCatch({
        kmeans_model()$plot_elbow(k_range = 2:10)
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), cex = 1.2, col = "red")
      })
    })

    # ========== OUTPUT: DETAILED STATS ==========
    output$cluster_summary_table <- renderDT({
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

    output$cor_latent <- renderPrint({
      req(summary_results())
      print(summary_results()$cor_latent)
    })

    output$r2_matrix_table <- renderDT({
      req(summary_results())

      r2_df <- summary_results()$r2_matrix
      r2_df <- cbind(variable = rownames(r2_df), r2_df)
      rownames(r2_df) <- NULL

      datatable(
        r2_df,
        options = list(
          pageLength = 15,
          dom = 'Bfrtip',
          scrollX = TRUE
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      ) %>%
        formatStyle(
          columns = 2:ncol(r2_df),
          background = styleColorBar(c(0, 1), 'lightblue'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    })

    # ========== ILLUSTRATIVE VARIABLES (NOUVEAU!) ==========

    # Reactive: Calculer illustrative si variables présentes
    illustrative_results <- reactive({
      req(kmeans_model())

      illust_data <- illustrative_data()

      if (!is.null(illust_data) && ncol(illust_data) > 0) {
        tryCatch({
          kmeans_model()$illustrative(illust_data, plot = FALSE)
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
    output$illustrative_content <- renderUI({
      ns <- session$ns

      if (is.null(illustrative_results())) {
        div(
          style = "text-align: center; padding: 40px; color: #718096;",
          icon("info-circle", class = "fa-3x", style = "color: #cbd5e0;"),
          br(), br(),
          h5("No illustrative variables selected", style = "color: #4a5568;"),
          p("Select illustrative variables in the sidebar to see their projection onto the clusters.",
            style = "font-size: 14px;")
        )
      } else {
        tagList(
          fluidRow(
            column(
              width = 12,
              div(
                style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                h5("📊 Illustrative Variables - R² with Clusters", style = "color: #2d3748; font-weight: 600;"),
                DTOutput(ns("illustrative_table"))
              )
            )
          ),

          br(),

          fluidRow(
            column(
              width = 12,
              div(
                style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                h5("📈 R² Visualization (Barplot)", style = "color: #2d3748; font-weight: 600;"),
                plotOutput(ns("illustrative_plot"), height = "400px")
              )
            )
          ),

          br(),

          fluidRow(
            column(
              width = 12,
              div(
                style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
                h5("🔢 Complete R² Matrix (All Clusters)", style = "color: #2d3748; font-weight: 600;"),
                DTOutput(ns("illustrative_r2_all"))
              )
            )
          )
        )
      }
    })

    # Table illustrative
    output$illustrative_table <- renderDT({
      req(illustrative_results())

      datatable(
        illustrative_results()$table,
        options = list(
          pageLength = 10,
          dom = 'Bfrtip',
          scrollX = TRUE
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      ) %>%
        formatStyle(
          'cluster',
          backgroundColor = styleEqual(
            unique(illustrative_results()$table$cluster),
            rainbow(length(unique(illustrative_results()$table$cluster)))
          )
        )
    })

    # Plot illustrative
    output$illustrative_plot <- renderPlot({
      req(illustrative_results())
      illustrative_results()$plot()
    })

    # R² matrix complète
    output$illustrative_r2_all <- renderDT({
      req(illustrative_results())

      r2_all <- illustrative_results()$r2_all_clusters
      r2_all <- cbind(variable = rownames(r2_all), r2_all)
      rownames(r2_all) <- NULL

      datatable(
        r2_all,
        options = list(
          pageLength = 10,
          dom = 'Bfrtip',
          scrollX = TRUE
        ),
        rownames = FALSE,
        class = 'cell-border stripe'
      ) %>%
        formatStyle(
          columns = 2:ncol(r2_all),
          background = styleColorBar(c(0, 100), 'lightgreen'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    })

    # Return model for external use
    return(kmeans_model)
  })
}
