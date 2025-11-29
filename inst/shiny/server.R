# =============================================================================
# inst/shiny/server.R
# Logique serveur de l'application Shiny - VERSION FINALE
# =============================================================================

library(shiny)
library(DT)
library(shinyjs)

# Charger les classes R6
source("../../R/n_clusters.R")  # Fonctions elbow (DOIT être chargé AVANT les classes)
source("../../R/kmeans.R")      # Classe K-means
source("../../R/acm_cah.R")     # Classe ACM-CAH
source("../../R/varclus.R")     # Classe VarClus

# Charger les modules Shiny
source("modules/kmeans_module.R")
source("modules/acm_cah_module.R")
source("modules/varclus_module.R")

# =============================================================================
# FONCTION SERVEUR
# =============================================================================

server <- function(input, output, session) {

  # ===========================================================================
  # STOCKAGE DES DATASETS
  # ===========================================================================

  # Store all uploaded datasets
  datasets <- reactiveValues(data = list())

  # ===========================================================================
  # REACTIVE 1: Preview des données uploadées
  # ===========================================================================

  preview_data <- reactive({
    req(input$file1)

    tryCatch({
      file_ext <- tools::file_ext(input$file1$name)

      if (file_ext %in% c("xlsx", "xls")) {
        # Support Excel files
        if (!requireNamespace("readxl", quietly = TRUE)) {
          showNotification(
            "Package 'readxl' requis pour les fichiers Excel. Installation...",
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
          dec = ","  # Support comma as decimal separator
        )
      }

      # Convert character columns with comma decimals to numeric
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
        paste("Erreur de lecture:", e$message),
        type = "error",
        duration = 10
      )
      NULL
    })
  })

  # ===========================================================================
  # OUTPUT: Afficher la preview des données
  # ===========================================================================

  output$data_preview <- renderUI({
    if (is.null(input$file1)) {
      div(
        class = "data-preview-box",
        style = "font-style: italic; color: #666; font-size: 16px; text-align: center; padding: 50px;",
        icon("upload", class = "fa-3x", style = "color: #ccc;"),
        br(), br(),
        "📂 Veuillez charger un fichier de données pour commencer."
      )
    } else {
      df <- preview_data()

      if (is.null(df)) {
        return(div(
          class = "alert alert-danger",
          icon("exclamation-triangle"),
          " Erreur de lecture du fichier avec les paramètres actuels."
        ))
      }

      n_quanti <- sum(sapply(df, is.numeric))
      n_quali  <- sum(sapply(df, function(x) !is.numeric(x)))

      tagList(
        # Résumé
        fluidRow(
          column(
            width = 12,
            div(
              style = "background-color: #e3f2fd; padding: 15px; border-radius: 6px; margin-bottom: 15px; border-left: 4px solid #2196f3;",
              strong(style = "color: #1976d2; font-size: 1.1em;", "📊 Résumé des données"),
              br(), br(),
              fluidRow(
                column(6,
                       div(style = "font-size: 1.05em;",
                           icon("table"),
                           strong(" Dimensions:"),
                           paste(nrow(df), "lignes ×", ncol(df), "colonnes")
                       )
                ),
                column(6,
                       div(style = "font-size: 1.05em;",
                           icon("file"),
                           strong(" Fichier:"),
                           input$file1$name
                       )
                )
              ),
              br(),
              fluidRow(
                column(6,
                       div(style = "font-size: 1.05em;",
                           icon("chart-bar", style = "color: #4caf50;"),
                           strong(" Variables quantitatives:"),
                           n_quanti
                       )
                ),
                column(6,
                       div(style = "font-size: 1.05em;",
                           icon("tags", style = "color: #ff9800;"),
                           strong(" Variables qualitatives:"),
                           n_quali
                       )
                )
              )
            )
          )
        ),

        # Table preview
        fluidRow(
          column(
            width = 12,
            h5(icon("eye"), " Aperçu des 10 premières lignes", style = "color: #555;"),
            div(
              style = "overflow-x: auto;",
              tableOutput("contents")
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

  # ===========================================================================
  # OBSERVER: Sauvegarder le dataset
  # ===========================================================================

  save_msg <- reactiveVal(NULL)

  # Reset message when new file uploaded
  observeEvent(input$file1, {
    save_msg(NULL)
  })

  observeEvent(input$save_dataset, {
    req(preview_data())

    # Sauvegarder dans reactiveValues
    datasets$data[[input$file1$name]] <- preview_data()

    # Mettre à jour le sélecteur de datasets
    updateSelectInput(
      session,
      "dataset_choice",
      choices = names(datasets$data),
      selected = input$file1$name
    )

    # Message de confirmation
    save_msg(
      div(
        style = "color: #28a745; font-weight: bold; margin-top: 10px; padding: 10px; background-color: #d4edda; border-radius: 5px; border: 1px solid #c3e6cb;",
        icon("check-circle"),
        HTML(" Dataset enregistré avec succès !")
      )
    )
  })

  output$save_msg <- renderUI({
    save_msg()
  })

  # ===========================================================================
  # OBSERVER: Mettre à jour les sélecteurs de variables
  # ===========================================================================

  observeEvent(input$dataset_choice, {
    req(datasets$data)
    df <- datasets$data[[input$dataset_choice]]
    req(df)

    cols <- names(df)

    # Séparer variables numériques et qualitatives
    num_vars <- names(df)[sapply(df, is.numeric)]

    # Par défaut, sélectionner toutes les variables numériques comme actives
    updateSelectInput(
      session,
      "active_vars",
      choices = cols,
      selected = num_vars
    )

    updateSelectInput(
      session,
      "illustrative_vars",
      choices = cols
    )
  })

  # ===========================================================================
  # OBSERVER: Variables actives/illustratives mutuellement exclusives
  # ===========================================================================

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

  # ===========================================================================
  # OBSERVER: Désactiver slider k quand auto_k est coché
  # ===========================================================================

  observe({
    if (isTRUE(input$auto_k)) {
      disable("num_k")
    } else {
      enable("num_k")
    }
  })

  # ===========================================================================
  # REACTIVE: Dataset sélectionné
  # ===========================================================================

  selected_data <- reactive({
    req(input$dataset_choice)
    datasets$data[[input$dataset_choice]]
  })

  # ===========================================================================
  # REACTIVE: Créer l'objet clustering
  # ===========================================================================

  clustering_engine <- eventReactive(input$run_clustering, {

    req(selected_data(), input$active_vars)

    if (length(input$active_vars) < 2) {
      showNotification(
        "Veuillez sélectionner au moins 2 variables actives",
        type = "error",
        duration = 5
      )
      return(NULL)
    }

    df <- selected_data()
    X <- df[, input$active_vars, drop = FALSE]

    # =========================================================================
    # K-MEANS
    # =========================================================================

    if (!is.null(input$algorithm) && input$algorithm == "kmeans") {
      if (!all(sapply(X, is.numeric))) {
        showNotification(
          "K-means nécessite uniquement des variables quantitatives",
          type = "error",
          duration = 5
        )
        return(NULL)
      }

      withProgress(message = "Clustering en cours...", {

        # Déterminer k optimal si demandé
        if (input$auto_k) {
          setProgress(0.2, detail = "Détection automatique de k...")

          # Créer un objet temporaire AVEC les données pour elbow
          km_temp <- KMeansVariablesQuant$new(
            k = 2,  # k initial temporaire
            max_iter = 100,
            n_init = 10,
            seed = 42
          )

          # Fit temporaire pour que elbow() ait accès aux données
          km_temp$fit(X)

          # Appeler elbow sur l'objet qui a maintenant les données
          elbow_res <- km_temp$elbow(
            k_range = 2:min(10, ncol(X)),
            n_init = 20,
            plot = FALSE
          )
          k_opt <- elbow_res$optimal_k

          showNotification(
            paste("✅ K-means: k optimal détecté =", k_opt),
            type = "message",
            duration = 5
          )

          # Créer l'objet final avec k optimal
          km <- KMeansVariablesQuant$new(
            k = k_opt,
            max_iter = 100,
            n_init = 10,
            seed = 42
          )

        } else {
          # Pas d'auto-détection : créer avec k choisi par l'utilisateur
          km <- KMeansVariablesQuant$new(
            k = input$num_k,
            max_iter = 100,
            n_init = 10,
            seed = 42
          )
        }

        # Fit final
        setProgress(0.7, detail = "Ajustement du modèle...")
        km$fit(X)

        setProgress(1, detail = "Terminé !")
      })

      # Ajouter variables illustratives si sélectionnées
      result <- list(
        model = km,
        type = "kmeans"
      )

      # Ajouter variables illustratives si sélectionnées
      if (!is.null(input$illustrative_vars) && length(input$illustrative_vars) > 0) {
        X_illust <- df[, input$illustrative_vars, drop = FALSE]
        # Garder uniquement les variables QUANTITATIVES (K-means = tout quantitatif)
        X_illust_num <- X_illust[, sapply(X_illust, is.numeric), drop = FALSE]

        if (ncol(X_illust_num) > 0) {
          result$illustrative <- X_illust_num
        }
      }

      result

      # =========================================================================
      # ACM-CAH
      # =========================================================================

    } else if (!is.null(input$algorithm) && input$algorithm == "acm_cah") {

      # ACM-CAH : variables qualitatives uniquement
      quali_data <- X[, sapply(X, is.factor) | sapply(X, is.character), drop = FALSE]

      # Convertir en factors si nécessaire
      if (ncol(quali_data) > 0) {
        quali_data[] <- lapply(quali_data, factor)
      }

      if (ncol(quali_data) == 0) {
        showNotification(
          "ACM-CAH nécessite au moins une variable qualitative",
          type = "error",
          duration = 5
        )
        return(NULL)
      }

      withProgress(message = "ACM-CAH en cours...", {

        # Déterminer k optimal si demandé
        if (input$auto_k) {
          setProgress(0.2, detail = "Détection automatique de k...")

          elbow_res <- acm_cah_elbow(
            X_quali = quali_data,
            method = input$acm_cah_method,
            k_max = min(10, nrow(quali_data))
          )

          k_opt <- elbow_res$optimal_k
          suggested <- elbow_res$suggested_k  # Top 3 k candidats

          showNotification(
            paste0("✅ ACM-CAH: k optimal = ", k_opt,
                   " (k suggérés: ", paste(suggested, collapse = ", "),
                   ") - Vérifiez le graphique"),
            type = "message",
            duration = 8
          )
        } else {
          k_opt <- input$num_k
        }

        setProgress(0.5, detail = "Construction du modèle...")

        model <- create_acm_cah_model(
          data = quali_data,
          method = input$acm_cah_method,
          n_axes = if (input$acm_cah_method == "acm") input$acm_cah_n_axes else 2,
          k = k_opt
        )

        setProgress(1, detail = "Terminé !")
      })

      list(
        model = model,
        type = "acm_cah"
      )

      # =========================================================================
      # VARCLUS
      # =========================================================================

    } else if (!is.null(input$algorithm) && input$algorithm == "varclus") {

      # VarClus : variables quantitatives uniquement
      numeric_cols <- sapply(X, is.numeric)
      quant_data <- X[, numeric_cols, drop = FALSE]

      if (ncol(quant_data) == 0) {
        showNotification(
          "VarClus nécessite au moins une variable quantitative",
          type = "error",
          duration = 5
        )
        return(NULL)
      }

      if (ncol(quant_data) < 2) {
        showNotification(
          "VarClus nécessite au moins 2 variables quantitatives",
          type = "error",
          duration = 5
        )
        return(NULL)
      }

      # CONVERTIR EN MATRICE NUMÉRIQUE (CRITIQUE pour Hmisc::varclus)
      quant_data <- as.matrix(quant_data)
      mode(quant_data) <- "numeric"

      withProgress(message = "VarClus en cours...", {

        # Déterminer k optimal si demandé
        if (input$auto_k) {
          setProgress(0.2, detail = "Détection automatique de k...")

          elbow_res <- varclus_elbow(
            X_num = quant_data,
            similarity = "pearson"
          )

          k_opt <- elbow_res$optimal_k

          showNotification(
            paste("✅ VarClus: k optimal détecté =", k_opt),
            type = "message",
            duration = 5
          )
        } else {
          k_opt <- input$num_k
        }

        setProgress(0.5, detail = "Construction du modèle...")

        model <- create_varclus_model(
          data = quant_data,
          similarity = "pearson",
          n_clusters = k_opt
        )

        setProgress(1, detail = "Terminé !")
      })

      # Ajouter variables illustratives si sélectionnées
      result <- list(
        model = model,
        type = "varclus"
      )

      if (!is.null(input$illustrative_vars) && length(input$illustrative_vars) > 0) {
        X_illust <- df[, input$illustrative_vars, drop = FALSE]
        X_illust_num <- X_illust[, sapply(X_illust, is.numeric), drop = FALSE]

        if (ncol(X_illust_num) > 0) {
          # Convertir aussi en matrice pour illustratives
          result$illustrative <- as.matrix(X_illust_num)
          mode(result$illustrative) <- "numeric"
        }
      }

      result

    } else {
      showNotification("Algorithme non implémenté", type = "warning")
      NULL
    }
  })

  # ===========================================================================
  # MODULE SERVEUR: K-means
  # ===========================================================================

  kmeansServer("kmeans_tab", engine_reactive = clustering_engine)

  # ===========================================================================
  # MODULE SERVEUR: ACM-CAH
  # ===========================================================================

  acm_cah_model <- reactive({
    engine <- clustering_engine()
    if (!is.null(engine) && engine$type == "acm_cah") {
      engine$model
    } else {
      NULL
    }
  })

  acm_cah_server(model_reactive = acm_cah_model)

  # ===========================================================================
  # MODULE SERVEUR: VarClus
  # ===========================================================================

  varclus_model <- reactive({
    engine <- clustering_engine()
    if (!is.null(engine) && engine$type == "varclus") {
      engine$model
    } else {
      NULL
    }
  })

  varclus_illustrative <- reactive({
    engine <- clustering_engine()
    if (!is.null(engine) && engine$type == "varclus") {
      engine$illustrative
    } else {
      NULL
    }
  })

  varclus_server(
    model_reactive = varclus_model,
    illustrative_reactive = varclus_illustrative
  )

  # ===========================================================================
  # OBSERVER: Changer d'onglet selon l'algo sélectionné
  # ===========================================================================

  observeEvent(input$algorithm, {
    updateTabsetPanel(session, "algo_tabs", selected = input$algorithm)
  })

  # ===========================================================================
  # OBSERVER: Passer à l'onglet Clustering après avoir lancé le clustering
  # ===========================================================================

  observeEvent(input$run_clustering, {
    updateTabsetPanel(session, "main_tabs", selected = "clustering")
  })

}
