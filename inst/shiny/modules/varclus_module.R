################################################################################
# MODULE VARCLUS
#
# Module Shiny pour clustering de variables quantitatives (VarClus)
# Approche descendante (divisive) basée sur ACP + rotation
#
# Date : 28/11/2024
################################################################################

# ==============================================================================
# FONCTION UI
# ==============================================================================

varclus_ui <- function() {
  tagList(
    # ===== Hero Section - Purple Theme =====
    div(
      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
               padding: 25px 20px;
               border-radius: 10px;
               margin-bottom: 25px;
               box-shadow: 0 4px 15px rgba(102, 126, 234, 0.25);",
      h3("VarClus: Variable Clustering",
         style = "margin: 0 0 8px 0;
                  font-weight: 700;
                  font-size: 1.8em;
                  color: white;
                  text-align: center;"),
      p("Divisive hierarchical clustering of quantitative variables using latent components",
        style = "text-align: center;
                 font-size: 0.95em;
                 color: rgba(255,255,255,0.95);
                 margin: 0;")
    ),

    # ===== Row 1: Model Overview + Elbow Plot =====
    fluidRow(
      # Model Overview (col 4)
      column(
        width = 4,
        div(
          style = "background: white;
                   padding: 25px;
                   border-radius: 10px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                   border-left: 4px solid #667eea;
                   height: 450px;
                   display: flex;
                   flex-direction: column;
                   justify-content: center;",

          div(
            style = "text-align: center; margin-bottom: 20px;",
            div(style = "font-size: 40px; margin-bottom: 10px;", "📋"),
            h4("Model Overview", style = "margin: 0; color: #2d3748; font-weight: 700; font-size: 1.3em;")
          ),

          tags$head(
            tags$style(HTML("
              #varclus_print {
                background-color: white !important;
                color: #2d3748 !important;
                font-family: 'Consolas', 'Monaco', monospace !important;
                font-size: 13px !important;
                padding: 20px !important;
                border-radius: 8px !important;
                border: 1px solid #e2e8f0 !important;
                line-height: 1.8 !important;
                box-shadow: 0 2px 8px rgba(0,0,0,0.06) !important;
              }
            "))
          ),
          verbatimTextOutput("varclus_print")
        )
      ),

      # Elbow Plot (col 8)
      column(
        width = 8,
        div(
          style = "background: white;
                   padding: 20px;
                   border-radius: 10px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                   border-left: 4px solid #667eea;
                   height: 450px;",
          div(
            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 15px;",
            div(style = "font-size: 22px;", "📈"),
            h4("Elbow Method", style = "margin: 0; color: #2d3748; font-weight: 600; font-size: 1.2em;")
          ),
          plotOutput("varclus_elbow", height = "380px")
        )
      )
    ),

    tags$div(style = "height: 20px;"),

    # ===== Row 2: Heatmap + Dendrogram =====
    fluidRow(
      column(
        width = 6,
        div(
          style = "background: white;
                   padding: 20px;
                   border-radius: 10px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                   border-left: 4px solid #667eea;
                   height: 550px;",
          div(
            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 15px;",
            div(style = "font-size: 22px;", "🔥"),
            h4("Correlation Heatmap", style = "margin: 0; color: #2d3748; font-weight: 600; font-size: 1.2em;")
          ),
          plotly::plotlyOutput("varclus_heatmap", height = "480px")
        )
      ),

      column(
        width = 6,
        div(
          style = "background: white;
                   padding: 20px;
                   border-radius: 10px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                   border-left: 4px solid #667eea;
                   height: 550px;",
          div(
            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 15px;",
            div(style = "font-size: 22px;", "🌳"),
            h4("Hierarchical Dendrogram", style = "margin: 0; color: #2d3748; font-weight: 600; font-size: 1.2em;")
          ),
          plotOutput("varclus_dendrogram", height = "480px")
        )
      )
    ),

    tags$div(style = "height: 20px;"),

    # ===== Section 3: Cluster Analysis =====
    div(
      style = "background: white;
               padding: 25px;
               border-radius: 10px;
               box-shadow: 0 2px 10px rgba(0,0,0,0.06);
               margin-bottom: 20px;
               border-left: 4px solid #667eea;",

      div(
        style = "display: flex; align-items: center; gap: 10px; margin-bottom: 20px;",
        div(style = "font-size: 24px;", "📊"),
        h4("Cluster Analysis", style = "margin: 0; color: #2d3748; font-weight: 700; font-size: 1.3em;")
      ),

      # Summary Text
      div(
        style = "background: #f8f9fa;
                 padding: 15px;
                 border-radius: 6px;
                 border: 1px solid #e2e8f0;
                 margin-bottom: 20px;
                 font-size: 0.9em;",
        verbatimTextOutput("varclus_summary_text")
      ),

      # Cluster Summary + R² Summary side by side
      fluidRow(
        column(
          width = 6,
          div(
            style = "margin-bottom: 25px;",
            h5(
              style = "color: #667eea;
                       font-weight: 600;
                       margin-bottom: 12px;
                       padding-bottom: 8px;
                       border-bottom: 2px solid #e2e8f0;
                       font-size: 1.05em;",
              "📋 Cluster Summary"
            ),
            DT::dataTableOutput("varclus_cluster_summary")
          )
        ),

        column(
          width = 6,
          div(
            h5(
              style = "color: #667eea;
                       font-weight: 600;
                       margin-bottom: 12px;
                       padding-bottom: 8px;
                       border-bottom: 2px solid #e2e8f0;
                       font-size: 1.05em;",
              "📈 Cluster Members & R² Values"
            ),
            DT::dataTableOutput("varclus_R2_summary")
          )
        )
      )
    ),

    # ===== Section 4: Illustrative Variables (conditionalPanel) =====
    conditionalPanel(
      condition = "output.varclus_has_illustrative",

      div(
        style = "background: white;
                 padding: 25px;
                 border-radius: 10px;
                 box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                 margin-bottom: 30px;
                 border-left: 4px solid #667eea;",

        div(
          style = "display: flex; align-items: center; gap: 10px; margin-bottom: 20px;",
          div(style = "font-size: 24px;", "🎯"),
          h4("Illustrative Variables", style = "margin: 0; color: #2d3748; font-weight: 700; font-size: 1.3em;")
        ),

        fluidRow(
          # Table on the left
          column(
            width = 5,
            div(
              h5(
                style = "color: #667eea;
                         font-weight: 600;
                         margin-bottom: 12px;
                         padding-bottom: 8px;
                         border-bottom: 2px solid #e2e8f0;
                         font-size: 1.05em;",
                "📊 Variable Summary"
              ),
              DT::dataTableOutput("varclus_illu_table")
            )
          ),

          # PCA Circle on the right
          column(
            width = 7,
            div(
              h5(
                style = "color: #667eea;
                         font-weight: 600;
                         margin-bottom: 12px;
                         padding-bottom: 8px;
                         border-bottom: 2px solid #e2e8f0;
                         font-size: 1.05em;",
                "⭕ PCA Correlation Circle"
              ),
              plotOutput("varclus_illu_plot", height = "500px")
            )
          )
        )
      )
    )
  )
}


# ==============================================================================
# FONCTION SERVEUR
# ==============================================================================

varclus_server <- function(model_reactive, illustrative_reactive = reactive(NULL)) {

  moduleServer(
    id = NULL,
    function(input, output, session) {

      # ========================================================================
      # OUTPUT 1: Print
      # ========================================================================

      output$varclus_print <- renderPrint({
        model <- model_reactive()
        req(model)

        tryCatch({
          model$print()
        }, error = function(e) {
          cat("❌ Error displaying model summary\n")
          cat("Error:", e$message, "\n")
        })
      })


      # ========================================================================
      # OUTPUT 2: Elbow Plot
      # ========================================================================

      output$varclus_elbow <- renderPlot({
        model <- model_reactive()
        req(model)

        tryCatch({
          # plot_elbow est une FONCTION, il faut l'appeler
          model$plot_elbow()
        }, error = function(e) {
          plot.new()
          text(0.5, 0.5, "❌ Error generating elbow plot", col = "red", cex = 1.2)
        })
      })


      # ========================================================================
      # OUTPUT 3: Dendrogram
      # ========================================================================

      output$varclus_dendrogram <- renderPlot({
        model <- model_reactive()
        req(model)

        tryCatch({
          dend_func <- model$get_dendrogram()
          dend_func()
        }, error = function(e) {
          plot.new()
          text(0.5, 0.5, "❌ Error generating dendrogram", col = "red", cex = 1.2)
        })
      })


      # ========================================================================
      # OUTPUT 4: Heatmap
      # ========================================================================

      output$varclus_heatmap <- plotly::renderPlotly({
        model <- model_reactive()
        req(model)

        tryCatch({
          # Vérifier que la matrice de similarité existe
          req(model$model)
          req(model$model$sim)

          cor_mat <- model$model$sim

          plotly::plot_ly(
            x = colnames(cor_mat),
            y = rownames(cor_mat),
            z = cor_mat,
            type = "heatmap",
            colorscale = "Oranges",
            zmin = min(cor_mat, na.rm = TRUE),
            zmax = max(cor_mat, na.rm = TRUE)
          ) %>%
            plotly::layout(
              xaxis = list(title = ""),
              yaxis = list(title = "", autorange = "reversed")
            )
        }, error = function(e) {
          plotly::plot_ly() %>%
            plotly::add_annotations(
              text = paste("❌ Error:", e$message),
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5,
              showarrow = FALSE,
              font = list(color = "red", size = 14)
            )
        })
      })


      # ========================================================================
      # OUTPUT 5: Summary Text
      # ========================================================================

      output$varclus_summary_text <- renderPrint({
        model <- model_reactive()
        req(model)

        tryCatch({
          summary_obj <- model$summary()
          cat(summary_obj$text)
        }, error = function(e) {
          cat("❌ Error displaying summary\n")
          cat("Error:", e$message, "\n")
        })
      })


      # ========================================================================
      # OUTPUT 6: Cluster Summary Table
      # ========================================================================

      output$varclus_cluster_summary <- DT::renderDataTable({
        model <- model_reactive()
        req(model)

        tryCatch({
          summary_obj <- model$summary()
          DT::datatable(
            summary_obj$cluster_summary,
            options = list(
              pageLength = 10,
              searching = FALSE,
              info = FALSE,
              lengthChange = FALSE
            ),
            rownames = FALSE
          )
        }, error = function(e) {
          data.frame(Error = "Unable to generate cluster summary")
        })
      })


      # ========================================================================
      # OUTPUT 7: R² Summary Table
      # ========================================================================

      output$varclus_R2_summary <- DT::renderDataTable({
        model <- model_reactive()
        req(model)

        tryCatch({
          summary_obj <- model$summary()
          DT::datatable(
            summary_obj$R2_summary,
            options = list(
              pageLength = 15,
              searching = TRUE,
              info = TRUE,
              lengthChange = TRUE
            ),
            rownames = FALSE
          )
        }, error = function(e) {
          data.frame(Error = "Unable to generate R² summary")
        })
      })


      # ========================================================================
      # OUTPUT 8: Illustrative Variables - Condition
      # ========================================================================

      output$varclus_has_illustrative <- reactive({
        illust <- illustrative_reactive()
        !is.null(illust) && ncol(illust) > 0
      })
      outputOptions(output, "varclus_has_illustrative", suspendWhenHidden = FALSE)


      # ========================================================================
      # OUTPUT 9: Illustrative Variables - Table
      # ========================================================================

      output$varclus_illu_table <- DT::renderDataTable({
        model <- model_reactive()
        illust <- illustrative_reactive()
        req(model, illust)

        tryCatch({
          illust_res <- model$illustrative(illust)
          DT::datatable(
            illust_res$table,
            options = list(
              pageLength = 10,
              searching = TRUE,
              info = TRUE
            ),
            rownames = FALSE
          )
        }, error = function(e) {
          data.frame(Error = "Unable to compute illustrative variables")
        })
      })


      # ========================================================================
      # OUTPUT 10: Illustrative Variables - PCA Plot
      # ========================================================================

      output$varclus_illu_plot <- renderPlot({
        model <- model_reactive()
        illust <- illustrative_reactive()
        req(model, illust)

        tryCatch({
          illust_res <- model$illustrative(illust)
          illust_res$plot()
        }, error = function(e) {
          plot.new()
          text(0.5, 0.5, "❌ Error generating PCA circle", col = "red", cex = 1.2)
        })
      })

    }
  )
}


# ==============================================================================
# WRAPPER - Création modèle VarClus
# ==============================================================================

create_varclus_model <- function(data, similarity = "pearson", n_clusters = NULL) {

  # Vérifier dépendances
  if (!requireNamespace("R6", quietly = TRUE)) {
    stop("Package 'R6' requis. Installez-le avec : install.packages('R6')")
  }

  if (!requireNamespace("Hmisc", quietly = TRUE)) {
    stop("Package 'Hmisc' requis. Installez-le avec : install.packages('Hmisc')")
  }

  if (!requireNamespace("dendextend", quietly = TRUE)) {
    stop("Package 'dendextend' requis. Installez-le avec : install.packages('dendextend')")
  }

  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' requis. Installez-le avec : install.packages('plotly')")
  }

  # Vérifier que la classe VarClus existe
  if (!exists("VarClus")) {
    stop("Classe VarClus introuvable. Assurez-vous que varclus.R est chargé.")
  }

  # Vérifier que la fonction varclus_elbow existe
  if (!exists("varclus_elbow")) {
    stop("Fonction varclus_elbow() introuvable. Elle doit être définie avant VarClus.")
  }

  # Créer et ajuster le modèle
  model <- VarClus$new(similarity = similarity, n_clusters = n_clusters)
  model$fit(data)

  return(model)
}
