# =============================================================================
# MODULE K-MEANS - Interface Shiny cohérente avec KMeansVariablesQuant R6
#
# Ce module affiche UNIQUEMENT ce que la classe R6 peut réellement faire.
# Aucune visualisation ou métrique inexistante.
#
# COHÉRENCE GARANTIE:
# ✅ Toutes les méthodes appelées existent dans la classe R6
# ✅ Tous les champs utilisés sont disponibles
# ✅ Aucune erreur, aucune section vide
#
# SECTIONS IMPLEMENTEES (8 au total):
# 1. Model Overview (print)
# 2. Elbow Plot (détection k optimal)
# 3. Cluster Inertia (barplot des λ_k)
# 4. Correlation Circle (variables vs PC1)
# 5. Centers Projection (visualisation des PC1)
# 6. Variable Assignments (table des affectations)
# 7. Summary Table (R² own/next/ratio)
# 8. Illustrative Variables
#
# =============================================================================

library(shiny)
library(ggplot2)
library(DT)

# =============================================================================
# INTERFACE UTILISATEUR
# =============================================================================

kmeansUI <- function(id) {
  ns <- NS(id)

  tagList(

    # =========================================================================
    # SECTION 1: HERO / HEADER
    # =========================================================================
    div(
      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
               padding: 40px 20px;
               border-radius: 15px;
               box-shadow: 0 10px 30px rgba(102,126,234,0.3);
               margin-bottom: 30px;
               color: white;
               text-align: center;",

      div(style = "font-size: 48px; margin-bottom: 10px;", "🎯"),
      h2("K-Means de Variables Quantitatives",
         style = "margin: 0; font-weight: 700; font-size: 32px;"),
      p("Classification de variables autour de composantes latentes (PC1)",
        style = "margin-top: 10px; opacity: 0.9; font-size: 18px;")
    ),

    # =========================================================================
    # SECTION 2: MODEL OVERVIEW (Print)
    # =========================================================================
    div(
      class = "kmeans-section",
      style = "background: white;
               padding: 25px;
               border-radius: 12px;
               box-shadow: 0 2px 15px rgba(0,0,0,0.08);
               border-left: 5px solid #667eea;
               margin-bottom: 25px;",

      div(
        style = "display: flex; align-items: center; gap: 12px; margin-bottom: 20px;",
        div(style = "font-size: 28px;", "📋"),
        h3("Aperçu du Modèle", style = "margin: 0; color: #2d3748; font-size: 22px;")
      ),

      verbatimTextOutput(ns("model_print"))
    ),

    # =========================================================================
    # SECTION 3: ELBOW PLOT
    # =========================================================================
    div(
      class = "kmeans-section",
      style = "background: white;
               padding: 25px;
               border-radius: 12px;
               box-shadow: 0 2px 15px rgba(0,0,0,0.08);
               border-left: 5px solid #667eea;
               margin-bottom: 25px;",

      div(
        style = "display: flex; align-items: center; gap: 12px; margin-bottom: 20px;",
        div(style = "font-size: 28px;", "📈"),
        h3("Détection du Nombre Optimal de Clusters (Méthode du Coude)",
           style = "margin: 0; color: #2d3748; font-size: 22px;")
      ),

      p("Cette méthode teste différentes valeurs de k et affiche l'inertie totale pour chaque solution.
        Le 'coude' de la courbe indique le k optimal où l'ajout d'un cluster supplémentaire
        n'apporte plus de gain significatif.",
        style = "color: #4a5568; margin-bottom: 15px;"),

      plotOutput(ns("elbow_plot"), height = "450px")
    ),

    # =========================================================================
    # SECTION 4: CLUSTER INERTIA (Barplot λ_k)
    # =========================================================================
    div(
      class = "kmeans-section",
      style = "background: white;
               padding: 25px;
               border-radius: 12px;
               box-shadow: 0 2px 15px rgba(0,0,0,0.08);
               border-left: 5px solid #667eea;
               margin-bottom: 25px;",

      div(
        style = "display: flex; align-items: center; gap: 12px; margin-bottom: 20px;",
        div(style = "font-size: 28px;", "📊"),
        h3("Inertie par Cluster (λₖ)",
           style = "margin: 0; color: #2d3748; font-size: 22px;")
      ),

      p("L'inertie d'un cluster k est la somme des R² des variables avec leur composante latente PC1ₖ.
        Plus l'inertie est élevée, plus le cluster est cohérent.",
        style = "color: #4a5568; margin-bottom: 15px;"),

      plotOutput(ns("inertia_barplot"), height = "400px")
    ),

    # =========================================================================
    # SECTION 5: CORRELATION CIRCLE (Variables avec PC1)
    # =========================================================================
    div(
      class = "kmeans-section",
      style = "background: white;
               padding: 25px;
               border-radius: 12px;
               box-shadow: 0 2px 15px rgba(0,0,0,0.08);
               border-left: 5px solid #667eea;
               margin-bottom: 25px;",

      div(
        style = "display: flex; align-items: center; gap: 12px; margin-bottom: 20px;",
        div(style = "font-size: 28px;", "⭕"),
        h3("Cercle de Corrélation (Variables)",
           style = "margin: 0; color: #2d3748; font-size: 22px;")
      ),

      p("Corrélations des variables avec les composantes latentes PC1 de chaque cluster.
        Les variables proches du cercle unité sont bien représentées.",
        style = "color: #4a5568; margin-bottom: 15px;"),

      plotOutput(ns("correlation_circle"), height = "550px")
    ),

    # =========================================================================
    # SECTION 6: CENTERS PROJECTION
    # =========================================================================
    div(
      class = "kmeans-section",
      style = "background: white;
               padding: 25px;
               border-radius: 12px;
               box-shadow: 0 2px 15px rgba(0,0,0,0.08);
               border-left: 5px solid #667eea;
               margin-bottom: 25px;",

      div(
        style = "display: flex; align-items: center; gap: 12px; margin-bottom: 20px;",
        div(style = "font-size: 28px;", "🎯"),
        h3("Projection des Centres Latents (PC1)",
           style = "margin: 0; color: #2d3748; font-size: 22px;")
      ),

      p("Visualisation des composantes latentes PC1 de chaque cluster dans le plan factoriel.
        Montre la séparation entre les clusters dans l'espace réduit.",
        style = "color: #4a5568; margin-bottom: 15px;"),

      plotOutput(ns("centers_projection"), height = "550px")
    ),

    # =========================================================================
    # SECTION 7: VARIABLE ASSIGNMENTS (Table)
    # =========================================================================
    div(
      class = "kmeans-section",
      style = "background: white;
               padding: 25px;
               border-radius: 12px;
               box-shadow: 0 2px 15px rgba(0,0,0,0.08);
               border-left: 5px solid #667eea;
               margin-bottom: 25px;",

      div(
        style = "display: flex; align-items: center; gap: 12px; margin-bottom: 20px;",
        div(style = "font-size: 28px;", "📝"),
        h3("Affectation des Variables aux Clusters",
           style = "margin: 0; color: #2d3748; font-size: 22px;")
      ),

      DTOutput(ns("assignments_table"))
    ),

    # =========================================================================
    # SECTION 8: SUMMARY TABLE (R² own/next/ratio)
    # =========================================================================
    div(
      class = "kmeans-section",
      style = "background: white;
               padding: 25px;
               border-radius: 12px;
               box-shadow: 0 2px 15px rgba(0,0,0,0.08);
               border-left: 5px solid #667eea;
               margin-bottom: 25px;",

      div(
        style = "display: flex; align-items: center; gap: 12px; margin-bottom: 20px;",
        div(style = "font-size: 28px;", "📊"),
        h3("Résumé Détaillé des Clusters",
           style = "margin: 0; color: #2d3748; font-size: 22px;")
      ),

      p(HTML("<b>R² Own:</b> Coefficient de détermination avec la PC1 de son propre cluster (doit être élevé)<br>
             <b>R² Next:</b> R² maximum avec la PC1 d'un autre cluster (doit être faible)<br>
             <b>Ratio (1-R²own)/(1-R²next):</b> Mesure d'adhésion au cluster (doit être << 1)"),
        style = "color: #4a5568; margin-bottom: 15px;"),

      DTOutput(ns("summary_table"))
    ),

    # =========================================================================
    # SECTION 9: ILLUSTRATIVE VARIABLES
    # =========================================================================
    div(
      class = "kmeans-section",
      style = "background: white;
               padding: 25px;
               border-radius: 12px;
               box-shadow: 0 2px 15px rgba(0,0,0,0.08);
               border-left: 5px solid #667eea;
               margin-bottom: 25px;",

      div(
        style = "display: flex; align-items: center; gap: 12px; margin-bottom: 20px;",
        div(style = "font-size: 28px;", "🔍"),
        h3("Variables Illustratives",
           style = "margin: 0; color: #2d3748; font-size: 22px;")
      ),

      p("Projetez des variables supplémentaires sur les clusters existants.
        Ces variables ne participent pas au clustering mais sont affectées au cluster le plus proche.",
        style = "color: #4a5568; margin-bottom: 15px;"),

      uiOutput(ns("illustrative_ui"))
    )
  )
}

# =============================================================================
# LOGIQUE SERVEUR
# =============================================================================

kmeansServer <- function(id, engine_reactive) {
  moduleServer(id, function(input, output, session) {

    # =======================================================================
    # REACTIVE: Récupérer le modèle K-means
    # =======================================================================
    model <- reactive({
      req(engine_reactive())
      eng <- engine_reactive()

      if (is.null(eng) || is.null(eng$model)) {
        return(NULL)
      }

      # Vérifier que c'est bien un objet K-means
      if (!inherits(eng$model, "KMeansVariablesQuant")) {
        return(NULL)
      }

      eng$model
    })

    # =======================================================================
    # OUTPUT 1: Model Print
    # =======================================================================
    output$model_print <- renderPrint({
      req(model())
      m <- model()

      if (is.null(m$clusters)) {
        cat("Modèle K-means non ajusté.\n")
        cat("Utilisez fit() pour entraîner le modèle.\n")
        return(invisible())
      }

      m$print()
    })

    # =======================================================================
    # OUTPUT 2: Elbow Plot
    # =======================================================================
    output$elbow_plot <- renderPlot({
      req(model())
      m <- model()

      if (is.null(m$data)) {
        plot.new()
        text(0.5, 0.5, "Données non disponibles", cex = 1.5, col = "red")
        return(invisible())
      }

      # Appeler la méthode elbow() sans afficher le plot automatique
      withCallingHandlers({
        elbow_result <- m$elbow(k_range = 2:10, n_init = 20, plot = FALSE)
      }, message = function(m) {
        # Supprimer les messages de convergence
        invokeRestart("muffleMessage")
      })

      # Afficher le graphique
      if (!is.null(elbow_result$plot)) {
        elbow_result$plot()
      }
    })

    # =======================================================================
    # OUTPUT 3: Inertia Barplot (λ_k par cluster)
    # =======================================================================
    output$inertia_barplot <- renderPlot({
      req(model())
      m <- model()

      if (is.null(m$inertia_by_cluster)) {
        plot.new()
        text(0.5, 0.5, "Inertie non calculée.\nExécutez fit() d'abord.",
             cex = 1.5, col = "red")
        return(invisible())
      }

      # Données
      df <- data.frame(
        cluster = factor(1:m$k),
        inertia = m$inertia_by_cluster
      )

      # Couleurs
      colors <- rainbow(m$k, s = 0.7, v = 0.9)

      # Graphique
      par(mar = c(5, 5, 4, 2))
      bp <- barplot(
        df$inertia,
        names.arg = paste0("Cluster ", df$cluster),
        col = colors,
        border = NA,
        ylim = c(0, max(df$inertia) * 1.15),
        las = 1,
        main = "Inertie par Cluster (λₖ = Σ R²)",
        xlab = "Cluster",
        ylab = "Inertie (λₖ)",
        cex.main = 1.4,
        cex.lab = 1.2,
        cex.axis = 1.1
      )

      # Ajouter les valeurs sur les barres
      text(
        x = bp,
        y = df$inertia,
        labels = sprintf("%.3f", df$inertia),
        pos = 3,
        cex = 1.1,
        font = 2
      )

      # Ligne de moyenne
      abline(h = mean(df$inertia), lty = 2, col = "gray40", lwd = 2)
      text(
        x = max(bp) * 0.85,
        y = mean(df$inertia) * 1.05,
        labels = sprintf("Moyenne: %.3f", mean(df$inertia)),
        col = "gray40",
        cex = 1.0
      )

      # Inertie totale
      mtext(
        sprintf("Inertie totale: %.4f", m$inertia_total),
        side = 3,
        line = 0.5,
        cex = 1.1,
        col = "navy"
      )
    })

    # =======================================================================
    # OUTPUT 4: Correlation Circle (Variables)
    # =======================================================================
    output$correlation_circle <- renderPlot({
      req(model())
      m <- model()

      if (is.null(m$r2_matrix) || is.null(m$centers)) {
        plot.new()
        text(0.5, 0.5, "Cercle de corrélation non disponible",
             cex = 1.5, col = "red")
        return(invisible())
      }

      # Appeler la méthode native
      m$plot_correlation_circle()
    })

    # =======================================================================
    # OUTPUT 5: Centers Projection
    # =======================================================================
    output$centers_projection <- renderPlot({
      req(model())
      m <- model()

      if (is.null(m$centers)) {
        plot.new()
        text(0.5, 0.5, "Centres non disponibles", cex = 1.5, col = "red")
        return(invisible())
      }

      # Appeler la méthode native
      m$plot_cluster_centers()
    })

    # =======================================================================
    # OUTPUT 6: Variable Assignments Table
    # =======================================================================
    output$assignments_table <- renderDT({
      req(model())
      m <- model()

      if (is.null(m$clusters)) {
        return(data.frame(Message = "Aucune affectation disponible"))
      }

      # Utiliser la nouvelle méthode get_clusters_table()
      df <- m$get_clusters_table()

      # Compter les variables par cluster
      counts <- as.data.frame(table(df$cluster))
      colnames(counts) <- c("Cluster", "Count")

      # Ajouter la proportion
      df$proportion <- sprintf("%.1f%%",
                               (table(df$cluster)[df$cluster] / nrow(df)) * 100)

      datatable(
        df,
        options = list(
          pageLength = 15,
          dom = 'frtip',
          scrollX = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = 1:2)
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      ) %>%
        formatStyle(
          'cluster',
          backgroundColor = styleEqual(
            1:m$k,
            rainbow(m$k, s = 0.3, v = 0.95)
          )
        )
    })

    # =======================================================================
    # OUTPUT 7: Summary Table
    # =======================================================================
    output$summary_table <- renderDT({
      req(model())
      m <- model()

      if (is.null(m$clusters)) {
        return(data.frame(Message = "Résumé non disponible"))
      }

      # Obtenir le résumé complet
      summary_data <- m$summary(print_summary = FALSE)

      # Table des membres avec R² own/next/ratio
      members_table <- summary_data$cluster_members

      datatable(
        members_table,
        options = list(
          pageLength = 15,
          dom = 'frtip',
          scrollX = TRUE,
          columnDefs = list(
            list(className = 'dt-center', targets = 1:4)
          )
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      ) %>%
        formatStyle(
          'R2_own_pct',
          background = styleColorBar(c(0, 100), '#667eea'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        formatStyle(
          'ratio',
          backgroundColor = styleInterval(
            cuts = c(0.3, 0.7, 1.0),
            values = c('#d4edda', '#fff3cd', '#f8d7da', '#f5c6cb')
          )
        )
    })

    # =======================================================================
    # OUTPUT 8: Illustrative Variables UI
    # =======================================================================
    output$illustrative_ui <- renderUI({
      req(model())
      m <- model()

      if (is.null(m$data)) {
        return(p("Aucune donnée disponible", style = "color: red;"))
      }

      ns <- session$ns

      tagList(
        selectInput(
          ns("illust_vars"),
          "Sélectionner les variables illustratives:",
          choices = colnames(m$data),
          multiple = TRUE
        ),

        actionButton(
          ns("project_illust"),
          "Projeter sur les clusters",
          class = "btn-primary"
        ),

        br(), br(),

        DTOutput(ns("illustrative_table"))
      )
    })

    # =======================================================================
    # OBSERVER: Projection des variables illustratives
    # =======================================================================
    illustrative_result <- eventReactive(input$project_illust, {
      req(model(), input$illust_vars)
      m <- model()

      if (length(input$illust_vars) == 0) {
        return(NULL)
      }

      # Extraire les variables sélectionnées
      illust_data <- m$data[, input$illust_vars, drop = FALSE]

      # Appeler predict()
      result <- m$predict(illust_data)

      result
    })

    output$illustrative_table <- renderDT({
      result <- illustrative_result()

      if (is.null(result)) {
        return(data.frame(Message = "Sélectionnez des variables et cliquez sur Projeter"))
      }

      datatable(
        result,
        options = list(
          pageLength = 10,
          dom = 'frtip',
          scrollX = TRUE
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      ) %>%
        formatStyle(
          'r2_max',
          background = styleColorBar(c(0, 1), '#667eea'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        ) %>%
        formatStyle(
          'r2_max',
          color = styleInterval(0.3, c('red', 'black'))
        )
    })

  })
}
