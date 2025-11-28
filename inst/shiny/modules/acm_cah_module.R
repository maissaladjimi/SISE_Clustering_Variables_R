################################################################################
# MODULE ACM-CAH (Analyse Correspondances Multiples + CAH)
#
# Classification des MODALITÉS de variables qualitatives
# 2 méthodes : "dice" (distance Dice) ou "acm" (ACM + CAH sur coords factorielles)
#
# Cohérence 100% avec classe R6 ClustModalities (acm_cah.R)
################################################################################

# ==============================================================================
# UI - ACM-CAH MODULE
# ==============================================================================

acm_cah_ui <- function() {
  tagList(
    # ===== Hero Section =====
    div(
      style = "background: linear-gradient(135deg, #ed8936 0%, #dd6b20 100%);
               padding: 25px 20px;
               border-radius: 10px;
               margin-bottom: 25px;
               box-shadow: 0 4px 15px rgba(237, 137, 54, 0.25);",
      h3("ACM-CAH: Modalities Clustering",
         style = "margin: 0 0 8px 0;
                  font-weight: 700;
                  font-size: 1.8em;
                  color: white;
                  text-align: center;"),
      p("Hierarchical clustering of categorical variable modalities using Dice distance or MCA",
        style = "text-align: center;
                 font-size: 0.95em;
                 color: rgba(255,255,255,0.95);
                 margin: 0;")
    ),

    # ===== Method Selection =====
    fluidRow(
      column(
        width = 12,
      )
    ),

    # ===== Section 1: Model Overview + Elbow =====
    fluidRow(
      column(
        width = 4,
        div(
          style = "background: white;
                   padding: 20px;
                   border-radius: 10px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                   border-left: 4px solid #ed8936;
                   height: 450px;
                   display: flex;
                   flex-direction: column;",
          div(
            style = "text-align: center; margin-bottom: 15px;",
            div(style = "font-size: 32px;", "📋"),
            h4("Model Overview",
               style = "margin: 5px 0 0 0; color: #2d3748; font-weight: 600; font-size: 1.2em;")
          ),
          div(
            style = "flex: 1; overflow-y: auto;",
            verbatimTextOutput("acm_cah_print")
          )
        )
      ),
      column(
        width = 8,
        div(
          style = "background: white;
                   padding: 20px;
                   border-radius: 10px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                   border-left: 4px solid #ed8936;
                   height: 450px;",
          div(
            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 15px;",
            div(style = "font-size: 22px;", "📈"),
            h4("Elbow Method",
               style = "margin: 0; color: #2d3748; font-weight: 600; font-size: 1.2em;")
          ),
          plotOutput("acm_cah_elbow", height = "380px")
        )
      )
    ),

    tags$div(style = "height: 20px;"),

    # ===== Section 2: Dendrogram + Cluster Table =====
    fluidRow(
      column(
        width = 6,
        div(
          style = "background: white;
                   padding: 20px;
                   border-radius: 10px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                   border-left: 4px solid #ed8936;",
          div(
            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 15px;",
            div(style = "font-size: 22px;", "🌳"),
            h4("Hierarchical Dendrogram",
               style = "margin: 0; color: #2d3748; font-weight: 600; font-size: 1.2em;")
          ),
          plotOutput("acm_cah_dendrogram", height = "450px")
        )
      ),
      column(
        width = 6,
        div(
          style = "background: white;
                   padding: 20px;
                   border-radius: 10px;
                   box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                   border-left: 4px solid #ed8936;",
          div(
            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 15px;",
            div(style = "font-size: 22px;", "📊"),
            h4("Cluster Composition",
               style = "margin: 0; color: #2d3748; font-weight: 600; font-size: 1.2em;")
          ),
          div(
            style = "max-height: 450px; overflow-y: auto;",
            DT::dataTableOutput("acm_cah_cluster_table")
          )
        )
      )
    ),

    tags$div(style = "height: 20px;"),

    # ===== Section 3: ACM-SPECIFIC VISUALIZATIONS =====
    conditionalPanel(
      condition = "input.acm_cah_method == 'acm'",

      # Row 1: Factor Map + Scree Plot
      fluidRow(
        column(
          width = 6,
          div(
            style = "background: white;
                     padding: 20px;
                     border-radius: 10px;
                     box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                     border-left: 4px solid #ed8936;",
            div(
              style = "display: flex; align-items: center; gap: 10px; margin-bottom: 15px;",
              div(style = "font-size: 22px;", "🗺️"),
              h4("Factorial Map (MCA)",
                 style = "margin: 0; color: #2d3748; font-weight: 600; font-size: 1.2em;")
            ),
            plotOutput("acm_cah_factor_map", height = "450px")
          )
        ),
        column(
          width = 6,
          div(
            style = "background: white;
                     padding: 20px;
                     border-radius: 10px;
                     box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                     border-left: 4px solid #ed8936;",
            div(
              style = "display: flex; align-items: center; gap: 10px; margin-bottom: 15px;",
              div(style = "font-size: 22px;", "📉"),
              h4("Scree Plot (MCA)",
                 style = "margin: 0; color: #2d3748; font-weight: 600; font-size: 1.2em;")
            ),
            plotOutput("acm_cah_scree", height = "450px")
          )
        )
      ),

      tags$div(style = "height: 20px;"),

      # Row 2: Contributions
      fluidRow(
        column(
          width = 12,
          div(
            style = "background: white;
                     padding: 20px;
                     border-radius: 10px;
                     box-shadow: 0 2px 10px rgba(0,0,0,0.06);
                     border-left: 4px solid #ed8936;",
            div(
              style = "display: flex; align-items: center; gap: 10px; margin-bottom: 15px;",
              div(style = "font-size: 22px;", "📊"),
              h4("Modality Contributions (%)",
                 style = "margin: 0; color: #2d3748; font-weight: 600; font-size: 1.2em;")
            ),
            fluidRow(
              column(
                width = 6,
                plotOutput("acm_cah_contrib_dim1", height = "400px")
              ),
              column(
                width = 6,
                plotOutput("acm_cah_contrib_dim2", height = "400px")
              )
            )
          )
        )
      )
    )
  )
}


# ==============================================================================
# SERVER - ACM-CAH MODULE
# ==============================================================================

acm_cah_server <- function(model_reactive) {
  # model_reactive() doit retourner un objet ClustModalities fitted

  moduleServer(
    id = NULL,
    module = function(input, output, session) {

      # ====================================================================
      # 1. MODEL OVERVIEW (print)
      # ====================================================================
      output$acm_cah_print <- renderPrint({
        req(model_reactive())
        model <- model_reactive()

        tryCatch({
          print(model)
        }, error = function(e) {
          cat("Error displaying model:\n")
          cat(e$message, "\n")
        })
      })

      # ====================================================================
      # 2. ELBOW PLOT
      # ====================================================================
      output$acm_cah_elbow <- renderPlot({
        req(model_reactive())
        model <- model_reactive()

        # Synchroniser k pour ACM-CAH
        observeEvent(input$num_k, {
          updateNumericInput(session, "acm_cah_k", value = input$num_k)
        })

        observeEvent(input$acm_cah_k, {
          updateNumericInput(session, "num_k", value = input$acm_cah_k)
        })

        tryCatch({
          model$plot_elbow(k_max = 10)
        }, error = function(e) {
          plot.new()
          text(0.5, 0.5, paste("Error:", e$message), col = "red")
        })
      })

      # ====================================================================
      # 3. DENDROGRAM
      # ====================================================================
      output$acm_cah_dendrogram <- renderPlot({
        req(model_reactive())
        model <- model_reactive()

        tryCatch({
          model$plot_dendrogram(k = model$k)
        }, error = function(e) {
          plot.new()
          text(0.5, 0.5, paste("Error:", e$message), col = "red")
        })
      })

      # ====================================================================
      # 4. CLUSTER TABLE
      # ====================================================================
      output$acm_cah_cluster_table <- DT::renderDataTable({
        req(model_reactive())
        model <- model_reactive()

        tryCatch({
          table_data <- model$cluster_table(k = model$k)

          DT::datatable(
            table_data,
            options = list(
              pageLength = 15,
              scrollY = "350px",
              scrollCollapse = TRUE,
              dom = 'frtip'
            ),
            rownames = FALSE,
            class = 'cell-border stripe'
          )
        }, error = function(e) {
          data.frame(Error = paste("Error:", e$message))
        })
      })

      # ====================================================================
      # 5. FACTOR MAP (ACM only)
      # ====================================================================
      output$acm_cah_factor_map <- renderPlot({
        req(model_reactive())
        req(input$acm_cah_method == "acm")
        model <- model_reactive()

        tryCatch({
          model$plot_factor_map(dim1 = 1, dim2 = 2, show_labels = TRUE)
        }, error = function(e) {
          plot.new()
          text(0.5, 0.5, paste("Error:", e$message), col = "red")
        })
      })

      # ====================================================================
      # 6. SCREE PLOT (ACM only)
      # ====================================================================
      output$acm_cah_scree <- renderPlot({
        req(model_reactive())
        req(input$acm_cah_method == "acm")
        model <- model_reactive()

        tryCatch({
          model$plot_scree(cumulative = FALSE)
        }, error = function(e) {
          plot.new()
          text(0.5, 0.5, paste("Error:", e$message), col = "red")
        })
      })

      # ====================================================================
      # 7. CONTRIBUTIONS DIM 1 (ACM only)
      # ====================================================================
      output$acm_cah_contrib_dim1 <- renderPlot({
        req(model_reactive())
        req(input$acm_cah_method == "acm")
        model <- model_reactive()

        tryCatch({
          model$plot_contrib(dim = 1, top = 15)
        }, error = function(e) {
          plot.new()
          text(0.5, 0.5, paste("Error:", e$message), col = "red")
        })
      })

      # ====================================================================
      # 8. CONTRIBUTIONS DIM 2 (ACM only)
      # ====================================================================
      output$acm_cah_contrib_dim2 <- renderPlot({
        req(model_reactive())
        req(input$acm_cah_method == "acm")
        model <- model_reactive()

        tryCatch({
          model$plot_contrib(dim = 2, top = 15)
        }, error = function(e) {
          plot.new()
          text(0.5, 0.5, paste("Error:", e$message), col = "red")
        })
      })
    }
  )
}


# ==============================================================================
# WRAPPER FUNCTION - Création du modèle ACM-CAH
# ==============================================================================

create_acm_cah_model <- function(data, method = "dice", n_axes = 2, k = 3) {
  #' Crée et ajuste un modèle ClustModalities
  #'
  #' @param data data.frame de variables qualitatives
  #' @param method "dice" ou "acm"
  #' @param n_axes nombre d'axes ACM (si method = "acm")
  #' @param k nombre de clusters
  #'
  #' @return objet ClustModalities fitted

  # Vérification dépendances
  if (!requireNamespace("R6", quietly = TRUE)) {
    stop("Le package R6 est requis. Installez-le avec: install.packages('R6')")
  }
  if (!requireNamespace("ade4", quietly = TRUE)) {
    stop("Le package ade4 est requis. Installez-le avec: install.packages('ade4')")
  }

  # Source de la classe R6
  if (!exists("ClustModalities")) {
    source("/mnt/user-data/uploads/acm_cah.R", local = TRUE)
  }

  # Création du modèle
  model <- ClustModalities$new(method = method, n_axes = n_axes)

  # Fit
  model$fit(X = data, k = k)

  return(model)
}


# ==============================================================================
# EXEMPLE D'UTILISATION DANS APP SHINY PRINCIPALE
# ==============================================================================

# Dans ui.R :
# tabPanel("ACM-CAH", acm_cah_ui())

# Dans server.R :
#
# # Modèle réactif
# acm_cah_model <- eventReactive(input$run_acm_cah, {
#   req(current_data())
#
#   # Sélectionner variables qualitatives
#   quali_vars <- current_data()[, sapply(current_data(), is.factor), drop = FALSE]
#
#   if (ncol(quali_vars) == 0) {
#     showNotification("No categorical variables found!", type = "error")
#     return(NULL)
#   }
#
#   withProgress(message = "Running ACM-CAH...", {
#     setProgress(0.3, detail = "Initializing model...")
#
#     model <- create_acm_cah_model(
#       data = quali_vars,
#       method = input$acm_cah_method,
#       n_axes = input$acm_cah_n_axes,
#       k = input$acm_cah_k
#     )
#
#     setProgress(1, detail = "Done!")
#     model
#   })
# })
#
# # Appel du module
# acm_cah_server(model_reactive = acm_cah_model)
