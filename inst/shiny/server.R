# ---------------------------
# Server.R for ClusteringVariables Shiny App
# CORRECTED VERSION - All bugs fixed
# ---------------------------

source("varclus_ui.R")
source("kmeans_ui.R")
source("acm_cah_ui.R")

server <- function(input, output, session) {

  # Store all uploaded datasets
  datasets <- reactiveValues(data = list())

  # ---------------------------
  # Reactive preview of the CSV with XLSX support
  # ---------------------------
  preview_data <- reactive({
    req(input$file1)

    tryCatch({
      file_ext <- tools::file_ext(input$file1$name)

      if (file_ext %in% c("xlsx", "xls")) {
        # FIX: Support Excel files
        if (!requireNamespace("readxl", quietly = TRUE)) {
          showNotification(
            "Package 'readxl' required for Excel files. Installing...",
            type = "warning",
            duration = 5
          )
          install.packages("readxl")
        }
        df <- readxl::read_excel(input$file1$datapath)

      } else {
        # CSV/TXT files
        df <- read.csv(
          input$file1$datapath,
          header = input$header,
          sep = input$sep,
          quote = input$quote,
          dec = ","  # FIX: Support comma as decimal separator
        )
      }

      # FIX: Convert character columns with comma decimals to numeric
      for (col in names(df)) {
        if (is.character(df[[col]])) {
          # Try to convert if it looks like numbers with commas
          test_numeric <- gsub(",", ".", df[[col]])
          if (all(grepl("^-?[0-9]+(\\.[0-9]+)?$", test_numeric, perl = TRUE) | is.na(test_numeric))) {
            df[[col]] <- as.numeric(gsub(",", ".", df[[col]]))
          }
        }
      }

      df

    }, error = function(e) {
      showNotification(
        paste("Error reading file:", e$message),
        type = "error",
        duration = 10
      )
      NULL
    })
  })

  # ---------------------------
  # Show preview in Data Import tab
  # ---------------------------
  output$data_preview <- renderUI({
    if (is.null(input$file1)) {
      div(
        style = "font-style: italic; color: #666; font-size: 16px; margin-top:10px; padding: 10px; border: 1px dashed #ccc; border-radius: 6px; background-color:#f9f9f9;",
        "📂 Please upload a dataset to get started."
      )
    } else {
      df <- preview_data()
      if (is.null(df)) return(div("Error reading file with current settings"))
      n_quanti <- sum(sapply(df, is.numeric))
      n_quali  <- sum(sapply(df, function(x) !is.numeric(x)))

      fluidRow(
        column(
          width = 3,
          div(
            style = "background-color: #f8f9fa; padding: 10px; border-radius: 6px; line-height:1.5;",
            strong("Summary:"),
            br(),
            paste("Quantitative variables:", n_quanti),
            br(),
            paste("Qualitative variables:", n_quali)
          )
        ),
        column(
          width = 9,
          tableOutput("contents")
        )
      )
    }
  })

  output$contents <- renderTable({
    req(preview_data())
    head(preview_data())
  })

  # ---------------------------
  # Upload button: Save dataset
  # ---------------------------

  save_msg <- reactiveVal(NULL)
  observeEvent(input$file1, {
    save_msg(NULL)
  })

  observeEvent(input$save_dataset, {
    req(preview_data())
    datasets$data[[input$file1$name]] <- preview_data()

    updateSelectInput(
      session,
      "dataset_choice",
      choices = names(datasets$data),
      selected = input$file1$name
    )

    save_msg(
      div(
        style = "color: green; font-weight: bold; margin-top: 10px;",
        HTML("✅ Dataset saved successfully!"))
    )

    output$save_msg <- renderUI({
      save_msg()
    })
  })

  # ---------------------------
  # Update variable selectors when dataset changes
  # ---------------------------
  observeEvent(input$dataset_choice, {
    req(datasets$data)
    df <- datasets$data[[input$dataset_choice]]
    req(df)
    cols <- names(df)

    updateSelectInput(session, "active_vars", choices = cols)
    updateSelectInput(session, "illustrative_vars", choices = cols)
  })

  # ---------------------------
  # Ensure active / illustrative variables are mutually exclusive
  # ---------------------------
  observeEvent(input$active_vars, {
    req(input$dataset_choice)
    df <- datasets$data[[input$dataset_choice]]
    updateSelectInput(
      session,
      "illustrative_vars",
      choices  = setdiff(names(df), input$active_vars),
      selected = intersect(input$illustrative_vars,
                           setdiff(names(df), input$active_vars))
    )
  })

  observeEvent(input$illustrative_vars, {
    req(input$dataset_choice)
    df <- datasets$data[[input$dataset_choice]]
    updateSelectInput(
      session,
      "active_vars",
      choices  = setdiff(names(df), input$illustrative_vars),
      selected = intersect(input$active_vars,
                           setdiff(names(df), input$illustrative_vars))
    )
  })

  # ---------------------------
  # Return selected dataset for clustering
  # ---------------------------
  selected_data <- reactive({
    req(input$dataset_choice)
    datasets$data[[input$dataset_choice]]
  })

  # Disable slider when auto_k checked
  observe({
    if (isTRUE(input$auto_k)) {
      disable("num_k")
    } else {
      enable("num_k")
    }
  })

  # ---------------------------
  # Clustering Output UI
  # ---------------------------

  clustering_result <- eventReactive(input$run_clustering, {
    list(
      algo = input$algorithm
    )
  })

  output$clustering_output <- renderUI({
    req(clustering_result())

    algo <- clustering_result()$algo

    if (algo == "kmeans") {
      tagList(
        kmeans_ui()
      )
    }

    else if (algo == "varclus") {
      tagList(
        varclus_ui()
      )
    }

    else if (algo == "acm_cah") {
      tagList(
        acm_cah_ui()
      )
    }
  })


  # ---------------------------
  # Clustering engine with FIXED error handling
  # ---------------------------
  clustering_engine <- eventReactive(input$run_clustering, {

    showNotification(
      "Running clustering... Please wait.",
      type = "message",
      duration = NULL,
      id = "clustering_notif"
    )

    tryCatch({
      req(selected_data(), input$active_vars, input$algorithm)

      df <- selected_data()[, input$active_vars, drop = FALSE]

      # FIX: Filter and convert variables based on algorithm
      if (input$algorithm %in% c("varclus", "kmeans")) {
        # Keep only numeric variables
        numeric_cols <- sapply(df, is.numeric)

        if (!any(numeric_cols)) {
          stop("No numeric variables found. Please select quantitative variables.")
        }

        df <- df[, numeric_cols, drop = FALSE]

        # FIX: Ensure all are truly numeric (not factors)
        df <- as.data.frame(lapply(df, as.numeric))

        if (ncol(df) < 2) {
          stop("At least 2 numeric variables are required for this algorithm.")
        }
      }

      # FIX: Filter qualitative variables for ACM-CAH
      if (input$algorithm == "acm_cah") {
        quali_cols <- !sapply(df, is.numeric)

        if (!any(quali_cols)) {
          stop("No qualitative variables found. Please select categorical variables.")
        }

        df <- df[, quali_cols, drop = FALSE]

        # Convert to factors
        df <- as.data.frame(lapply(df, as.factor))

        if (ncol(df) < 1) {
          stop("At least 1 qualitative variable is required for ACM-CAH.")
        }
      }

      # FIX: Handle n_clusters based on algorithm
      if (input$algorithm == "kmeans") {
        # KMeans uses 'k' parameter, not 'n_clusters'
        if (input$auto_k) {
          # Créer un modèle temporaire pour l'elbow
          km_temp <- ClusteringVariables::KMeansVariablesQuant$new(k = 2)
          km_temp$fit(df)

          # Exécuter la méthode elbow
          elbow_result <- km_temp$elbow(k_range = 2:10, n_init = 20, plot = FALSE)
          k_optimal <- elbow_result$optimal_k

          # Créer le modèle final avec k optimal
          km <- ClusteringVariables::KMeansVariablesQuant$new(k = k_optimal)
          km$fit(df)
          engine <- list(model = km)

          showNotification(
            paste("K optimal détecté:", k_optimal),
            type = "message",
            duration = 3
          )
        } else {
          # Manual K - FIX: Ensure k is integer >= 2
          k_value <- as.integer(input$num_k)
          if (k_value < 2) k_value <- 2

          km <- ClusteringVariables::KMeansVariablesQuant$new(k = k_value)
          km$fit(df)
          engine <- list(model = km)
        }
      }

      else if (input$algorithm == "varclus") {
        # VarClus accepts n_clusters
        n_clusters <- if (input$auto_k) NULL else as.integer(input$num_k)

        vc <- ClusteringVariables::VarClus$new(n_clusters = n_clusters)
        vc$fit(df)
        engine <- list(model = vc)
      }

      else if (input$algorithm == "acm_cah") {
        # FIX: ClustModalities doesn't take n_clusters in constructor
        # It uses method argument instead
        hc <- ClusteringVariables::ClustModalities$new(method = "acm")
        hc$fit(df)

        # FIX: Cut tree after fitting
        if (!input$auto_k) {
          k_value <- as.integer(input$num_k)
          hc$clusters <- data.frame(
            modality = rownames(hc$clusters),
            cluster = cutree(hc$hclust, k = k_value)
          )
        }

        engine <- list(model = hc)
      }

      removeNotification("clustering_notif")
      showNotification(
        "Clustering completed successfully!",
        type = "message",
        duration = 3
      )

      engine

    }, error = function(e) {
      removeNotification("clustering_notif")
      showNotification(
        paste("Error:", e$message),
        type = "error",
        duration = 10
      )
      NULL
    })
  })

  # ---------------------------
  # Illustrative variables - FIXED
  # ---------------------------
  illust_results <- reactive({
    engine <- clustering_engine()
    req(engine)
    req(input$illustrative_vars)

    tryCatch({
      illust_df <- selected_data()[, input$illustrative_vars, drop = FALSE]

      # FIX: Filter only numeric for quantitative algorithms
      if (input$algorithm %in% c("varclus", "kmeans")) {
        numeric_cols <- sapply(illust_df, is.numeric)

        if (!any(numeric_cols)) {
          stop("Illustrative variables must be numeric for this algorithm.")
        }

        illust_df <- illust_df[, numeric_cols, drop = FALSE]
        illust_df <- as.data.frame(lapply(illust_df, as.numeric))
      }

      # FIX: Filter only qualitative for ACM-CAH
      if (input$algorithm == "acm_cah") {
        quali_cols <- !sapply(illust_df, is.numeric)

        if (!any(quali_cols)) {
          stop("Illustrative variables must be qualitative for ACM-CAH.")
        }

        illust_df <- illust_df[, quali_cols, drop = FALSE]
        illust_df <- as.data.frame(lapply(illust_df, as.factor))
      }

      # Try illustrative method
      if ("illustrative" %in% names(engine$model)) {
        engine$model$illustrative(illust_df)
      } else {
        NULL
      }

    }, error = function(e) {
      showNotification(
        paste("Illustrative variables error:", e$message),
        type = "warning",
        duration = 5
      )
      NULL
    })
  })

  # ============================================================================
  # VARCLUS OUTPUTS
  # ============================================================================

  output$varclus_elbow <- renderPlot({
    engine <- clustering_engine()
    req(engine)
    if ("plot_elbow" %in% names(engine$model)) {
      engine$model$plot_elbow()
    }
  })

  output$varclus_dendrogram <- renderPlot({
    engine <- clustering_engine()
    req(engine)
    if ("get_dendrogram" %in% names(engine$model)) {
      dend_func <- engine$model$get_dendrogram()
      if (is.function(dend_func)) {
        dend_func()
      }
    }
  })

  output$varclus_heatmap <- plotly::renderPlotly({
    engine <- clustering_engine()
    req(engine)
    if ("get_heatmap" %in% names(engine$model)) {
      heatmap_func <- engine$model$get_heatmap()
      if (is.function(heatmap_func)) {
        heatmap_func()
      }
    }
  })

  output$varclus_print <- renderPrint({
    engine <- clustering_engine()
    req(engine)
    engine$model$print()
  })

  output$varclus_summary_text <- renderText({
    engine <- clustering_engine()
    req(engine)
    summary_obj <- engine$model$summary()
    if (is.list(summary_obj) && "text" %in% names(summary_obj)) {
      summary_obj$text
    } else {
      "Summary not available"
    }
  })

  output$varclus_similarity_matrix <- renderTable({
    engine <- clustering_engine()
    req(engine)
    summary_obj <- engine$model$summary()
    if (is.list(summary_obj) && "similarity_matrix" %in% names(summary_obj)) {
      summary_obj$similarity_matrix
    }
  })

  output$varclus_cluster_summary <- renderTable({
    engine <- clustering_engine()
    req(engine)
    summary_obj <- engine$model$summary()
    if (is.list(summary_obj) && "cluster_summary" %in% names(summary_obj)) {
      summary_obj$cluster_summary
    }
  })

  output$varclus_R2_summary <- renderTable({
    engine <- clustering_engine()
    req(engine)
    summary_obj <- engine$model$summary()
    if (is.list(summary_obj) && "R2_summary" %in% names(summary_obj)) {
      summary_obj$R2_summary
    }
  })

  output$varclus_illu_table <- renderTable({
    results <- illust_results()
    req(results)
    if (is.list(results) && "table" %in% names(results)) {
      results$table
    }
  })

  output$varclus_assignments <- renderTable({
    engine <- clustering_engine()
    req(engine)

    if ("get_clusters_table" %in% names(engine$model)) {
      engine$model$get_clusters_table()
    }
  })

  output$varclus_illu_plot <- renderPlot({
    results <- illust_results()
    req(results)
    if (is.list(results) && "plot" %in% names(results)) {
      results$plot()
    }
  })

  # ============================================================================
  # KMEANS OUTPUTS - FIXED
  # ============================================================================

  output$kmeans_elbow <- renderPlot({
    engine <- clustering_engine()
    req(engine)
    if ("plot_elbow" %in% names(engine$model)) {
      engine$model$plot_elbow()
    }
  })

  output$kmeans_print <- renderPrint({
    engine <- clustering_engine()
    req(engine)
    engine$model$print()
  })

  output$kmeans_summary_text <- renderPrint({
    engine <- clustering_engine()
    req(engine)

    # La méthode summary() retourne une liste, on l'affiche proprement
    summary_obj <- engine$model$summary()

    if (is.list(summary_obj)) {
      # Afficher les informations principales
      cat("=== K-Means Clustering Summary ===\n\n")

      cat("Number of clusters:", engine$model$k, "\n")
      cat("Number of variables:", ncol(engine$model$data), "\n")
      cat("Total inertia:", round(engine$model$inertia_total, 4), "\n")
      cat("Iterations:", engine$model$n_iter, "\n\n")

      # Afficher la composition des clusters
      if ("cluster_composition" %in% names(summary_obj)) {
        cat("--- Cluster Composition ---\n")
        print(summary_obj$cluster_composition)
      }
    }
  })

  output$kmeans_composition <- renderTable({
    engine <- clustering_engine()
    req(engine)

    if ("clusters" %in% names(engine$model)) {
      # clusters est un vecteur d'entiers
      clusters_vec <- engine$model$clusters

      # Créer le tableau de composition
      composition <- as.data.frame(table(clusters_vec))
      names(composition) <- c("Cluster", "Nombre de variables")

      # Ajouter le pourcentage
      composition$Pourcentage <- round(
        100 * composition$`Nombre de variables` / sum(composition$`Nombre de variables`),
        1
      )

      composition
    }
  })

  output$kmeans_inertia <- renderPlot({
    engine <- clustering_engine()
    req(engine)

    if ("inertia_by_cluster" %in% names(engine$model)) {
      by_cluster <- engine$model$inertia_by_cluster
      total <- engine$model$inertia_total
      k <- engine$model$k

      # Créer un barplot par cluster
      barplot(
        by_cluster,
        names.arg = paste0("Cluster ", 1:k),
        main = "Inertie par cluster (somme des R²)",
        col = rainbow(k),
        ylab = "Inertie (λk)",
        ylim = c(0, max(by_cluster) * 1.2),
        border = "white"
      )

      # Ajouter les valeurs sur les barres
      text(
        x = seq_along(by_cluster) * 1.2 - 0.5,
        y = by_cluster / 2,
        labels = round(by_cluster, 2),
        col = "black",
        font = 2
      )

      # Ajouter l'inertie totale
      mtext(
        paste("Inertie totale:", round(total, 2)),
        side = 3,
        line = 0.5,
        font = 2
      )

      # Ajouter une ligne horizontale pour la moyenne
      abline(h = mean(by_cluster), lty = 2, col = "gray50")
    }
  })

  output$kmeans_cluster_summary <- renderTable({
    engine <- clustering_engine()
    req(engine)
    summary_obj <- engine$model$summary()

    if (is.list(summary_obj) && "cluster_summary" %in% names(summary_obj)) {
      summary_obj$cluster_summary
    }
  })

  output$kmeans_assignments <- renderTable({
    engine <- clustering_engine()
    req(engine)

    if ("get_clusters_table" %in% names(engine$model)) {
      clusters_vec <- engine$model$get_clusters_table()
      var_names <- colnames(engine$model$data)

      # Créer un tableau avec variables et clusters
      assignments <- data.frame(
        Variable = var_names,
        Cluster = clusters_vec,
        stringsAsFactors = FALSE
      )

      # Trier par cluster puis par nom de variable
      assignments <- assignments[order(assignments$Cluster, assignments$Variable), ]

      # Réinitialiser les indices de lignes
      rownames(assignments) <- NULL

      assignments
    }
  })

  output$kmeans_illu_table <- renderTable({
    results <- illust_results()
    req(results)
    if (is.list(results) && "table" %in% names(results)) {
      results$table
    } else if (is.data.frame(results)) {
      results
    }
  })

  output$kmeans_illu_plot <- renderPlot({
    results <- illust_results()
    req(results)
    if (is.list(results) && "plot" %in% names(results)) {
      results$plot()
    }
  })

  # ============================================================================
  # ACM-CAH OUTPUTS
  # ============================================================================

  output$acm_cah_elbow <- renderPlot({
    engine <- clustering_engine()
    req(engine)
    if ("plot_elbow" %in% names(engine$model)) {
      engine$model$plot_elbow()
    }
  })

  output$acm_cah_print <- renderPrint({
    engine <- clustering_engine()
    req(engine)
    engine$model$print()
  })

  output$acm_cah_summary_text <- renderText({
    engine <- clustering_engine()
    req(engine)
    summary_obj <- engine$model$summary()
    if (is.list(summary_obj) && "text" %in% names(summary_obj)) {
      summary_obj$text
    } else if (is.character(summary_obj)) {
      summary_obj
    } else {
      "Summary not available"
    }
  })

  output$acm_cah_dendrogram <- renderPlot({
    engine <- clustering_engine()
    req(engine)
    if ("get_dendrogram" %in% names(engine$model)) {
      dend_func <- engine$model$get_dendrogram()
      if (is.function(dend_func)) {
        dend_func()
      }
    }
  })


  output$acm_cah_modalities <- renderTable({
    engine <- clustering_engine()
    req(engine)

    if ("clusters" %in% names(engine$model)) {
      engine$model$clusters
    }
  })

  output$acm_cah_modalities <- renderTable({
    engine <- clustering_engine()
    req(engine)

    if ("get_clusters_table" %in% names(engine$model)) {
      engine$model$get_clusters_table()
    }
  })

  output$acm_cah_quality <- renderTable({
    engine <- clustering_engine()
    req(engine)
    summary_obj <- engine$model$summary()

    if (is.list(summary_obj) && "quality_metrics" %in% names(summary_obj)) {
      summary_obj$quality_metrics
    } else if (is.list(summary_obj) && "cluster_summary" %in% names(summary_obj)) {
      summary_obj$cluster_summary
    }
  })

  output$acm_cah_illu_table <- renderTable({
    results <- illust_results()
    req(results)
    if (is.list(results) && "table" %in% names(results)) {
      results$table
    } else if (is.data.frame(results)) {
      results
    }
  })

  output$acm_cah_illu_plot <- renderPlot({
    results <- illust_results()
    req(results)
    if (is.list(results) && "plot" %in% names(results)) {
      results$plot()
    }
  })

  # Carte factorielle ACM
  output$acm_cah_factorial_map <- renderPlot({
    engine <- clustering_engine()
    req(engine)

    if ("plot_factorial_map" %in% names(engine$model)) {
      # Vérifier que c'est bien ACM (pas DICE qui n'a pas de coords)
      if (engine$model$method == "acm" && !is.null(engine$model$mod_coords)) {
        engine$model$plot_factorial_map(
          dim1 = 1,
          dim2 = 2,
          show_labels = TRUE
        )
      } else {
        plot.new()
        text(0.5, 0.5, "Carte factorielle disponible uniquement pour méthode ACM",
             cex = 1.2, col = "red")
      }
    }
  })

  # Scree plot ACM
  output$acm_cah_scree <- renderPlot({
    engine <- clustering_engine()
    req(engine)

    if ("plot_scree" %in% names(engine$model)) {
      if (engine$model$method == "acm" && !is.null(engine$model$eig_raw)) {
        engine$model$plot_scree(cumulative = FALSE)
      } else {
        plot.new()
        text(0.5, 0.5, "Scree plot disponible uniquement pour méthode ACM",
             cex = 1.2, col = "red")
      }
    }
  })

  # Contributions ACM
  output$acm_cah_contrib <- renderPlot({
    engine <- clustering_engine()
    req(engine)

    if ("plot_contrib" %in% names(engine$model)) {
      if (engine$model$method == "acm" && !is.null(engine$model$acm)) {
        # Utiliser la dimension sélectionnée par l'utilisateur
        dim_to_plot <- input$acm_dim
        if (is.null(dim_to_plot)) dim_to_plot <- 1

        engine$model$plot_contrib(dim = dim_to_plot, top = 15)
      } else {
        plot.new()
        text(0.5, 0.5, "Contributions disponibles uniquement pour méthode ACM",
             cex = 1.2, col = "red")
      }
    }
  })
}
