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

      # ========== TAB 4: ILLUSTRATIVE VARIABLES (MERGED) ==========
      tabPanel(
        "🔍 Illustrative Variables",
        value = "illustrative",
        br(),

        ##### SECTION 1 — QUALITATIVE #####
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: #f7fafc; padding: 20px; border-radius: 8px; border-left: 4px solid #ed8936;",
              h4("📌 Qualitative Illustrative Variables",
                 style = "color: #2d3748; font-weight: 600; margin-top: 0;"),
              p("Projection of qualitative variables onto clusters:",
                style = "color: #4a5568; font-size: 15px;"),
              p("• DICE → mean Dice² distance to cluster modalities",
                style = "color: #4a5568; font-size: 14px;"),
              p("• ACM → Euclidean distance to cluster barycenters",
                style = "color: #4a5568; font-size: 14px;")
            )
          )
        ),

        br(),
        uiOutput("acm_cah_illustrative_content"),


        br(), br(),

        hr(style = "border-top: 2px solid #CBD5E0; margin: 40px 0;"),
        ##### SECTION 2 — NUMERIQUE #####
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: #f7fafc; padding: 20px; border-radius: 8px; border-left: 4px solid #4299e1;",
              h4("📊 Quantitative Illustrative Variables",
                 style = "color: #2d3748; font-weight: 600; margin-top: 0;"),
              p("Correlation of quantitative variables with MCA dimensions.",
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
          grDevices::colorRampPalette(c(
            "#1f77b4", "#ff7f0e", "#2ca02c",
            "#d62728", "#9467bd", "#8c564b",
            "#e377c2", "#7f7f7f", "#bcbd22",
            "#17becf"
          ))(length(unique(clusters_df$cluster)))
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
        p(
          "Select qualitative illustrative variables in the sidebar to see their projection onto the clusters.",
          style = "font-size: 14px;"
        )
      )
    } else {
      tagList(
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: white; padding: 15px; border-radius: 8px;
                     box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              h5("📊 Illustrative Modalities - Distance to Clusters",
                 style = "color: #2d3748; font-weight: 600;"),
              p(
                if (method_value() == "dice") {
                  "Average Dice² distance to cluster members (lower = closer)"
                } else {
                  "Euclidean distance to cluster barycenters in MCA space (lower = closer)"
                },
                style = "color: #718096; font-size: 13px; margin-bottom: 15px;"
              ),
              DTOutput("acm_cah_illustrative_table")
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 12,
            div(
              style = "background: white; padding: 15px; border-radius: 8px;
                     box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
              h5("📈 Distance Visualization",
                 style = "color: #2d3748; font-weight: 600;"),
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
          grDevices::colorRampPalette(c(
            "#1f77b4", "#ff7f0e", "#2ca02c",
            "#d62728", "#9467bd", "#8c564b",
            "#e377c2", "#7f7f7f", "#bcbd22",
            "#17becf"
          ))(length(unique(illustrative_results()$table$cluster_assigned)))
        )
      )
  })

  # Plot illustrative
  output$acm_cah_illustrative_plot <- renderPlot({
    req(illustrative_results())
    illustrative_results()$plot()
  })

  # ========== ILLUSTRATIVE NUMERIC (retour à la version simple & sûre) ==========

  illustrative_numeric_results <- reactive({
    req(acm_cah_model())
    req(method_value() == "acm")

    illust_num <- illustrative_numeric_data()

    # Aucun jeu de données quantitatif illustratif
    if (is.null(illust_num) || ncol(illust_num) == 0) {
      return(NULL)
    }

    tryCatch({
      res <- acm_cah_model()$illustrative_numeric(illust_num, plot = FALSE)

      # Sécurité : si pas de rownames → on met les noms des variables
      if (is.null(rownames(res))) {
        rownames(res) <- colnames(illust_num)
      }

      res
    }, error = function(e) {
      showNotification(
        paste("Error in illustrative_numeric:", e$message),
        type = "error",
        duration = 10
      )
      NULL
    })
  })

  # --------- UI dynamique pour les variables illustratives quantitatives ---------
  output$acm_cah_illustrative_numeric_content <- renderUI({
    # Si on n'est pas en ACM : on affiche juste un message
    if (method_value() != "acm") {
      return(
        div(
          style = "text-align: center; padding: 40px; color = #718096;",
          icon("info-circle", class = "fa-3x", style = "color: #cbd5e0;"),
          br(), br(),
          h5("Quantitative illustrative variables are only available for MCA method",
             style = "color: #4a5568;"),
          p("Switch to 'MCA + CAH' in the sidebar to see this section.",
            style = "font-size: 14px;")
        )
      )
    }

    cors <- illustrative_numeric_results()

    # Aucun résultat (pas de variables illustratives quantitatives)
    if (is.null(cors)) {
      return(
        div(
          style = "text-align: center; padding: 40px; color: #718096;",
          icon("info-circle", class = "fa-3x", style = "color: #cbd5e0;"),
          br(), br(),
          h5("No quantitative illustrative variables", style = "color: #4a5568;"),
          p("Select at least one numeric illustrative variable in the sidebar.",
            style = "font-size: 14px;")
        )
      )
    }

    # Sinon : on affiche le cercle + la table
    tagList(
      fluidRow(
        column(
          width = 6,
          div(
            style = "background: white; padding: 15px; border-radius: 8px;
                     box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
            h5("📈 Correlation Circle",
               style = "color: #2d3748; font-weight: 600;"),
            plotOutput("acm_cah_illustrative_numeric_plot", height = "450px")
          )
        ),
        column(
          width = 6,
          div(
            style = "background: white; padding: 15px; border-radius: 8px;
                     box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
            h5("📋 Correlations summary",
               style = "color: #2d3748; font-weight: 600;"),
            DTOutput("acm_cah_illustrative_numeric_table")
          )
        )
      )
    )
  })

  output$acm_cah_illustrative_numeric_plot <- renderPlot({
    cors <- illustrative_numeric_results()
    req(cors)

    # Cercle unité
    t <- seq(0, 2 * pi, length.out = 200)
    circle_x <- cos(t)
    circle_y <- sin(t)

    plot(circle_x, circle_y, type = "l",
         xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1),
         asp = 1,
         xlab = "Correlation with Dim 1",
         ylab = "Correlation with Dim 2",
         main = "Correlation Circle — Quantitative illustrative variables")

    abline(h = 0, v = 0, lty = 3, col = "grey70")

    # Flèches depuis l'origine vers chaque variable
    arrows(0, 0,
           cors$cor.Dim1,
           cors$cor.Dim2,
           length = 0.08, lwd = 1.5)

    # Étoiles aux extrémités
    points(cors$cor.Dim1, cors$cor.Dim2,
           pch = 8, cex = 1.3)

    # Labels légèrement décalés
    text(cors$cor.Dim1 * 1.05,
         cors$cor.Dim2 * 1.05,
         labels = rownames(cors),
         cex = 0.8)
  })

  output$acm_cah_illustrative_numeric_table <- renderDT({
    cors <- illustrative_numeric_results()
    req(cors)

    df <- cbind(
      Variable = rownames(cors),
      cors,
      AbsCorr = sqrt(cors$cor.Dim1^2 + cors$cor.Dim2^2)
    )
    df <- df[order(-df$AbsCorr),]

    datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE),
      class = "cell-border stripe"
    )
  })

  # Return model for external use
  return(acm_cah_model)
}
