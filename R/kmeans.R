# =============================================================================
# KMeansVariablesQuant: K-means de variables quantitatives autour de composantes latentes
#
# VERSION FINALE CORRIGÉE - BUGS RÉSOLUS:
# 1. Formule correcte pour lambda (somme des R²)
# 2. Formule correcte pour variance_explained
# 3. Correction du predict() (vérification dimensions)
# 4. Vrai cercle de corrélation (variables, pas centres)
# 5. Échelles ajustées pour les graphiques
# 6. Visualisations séparées
#
# PRINCIPE (d'après Ricco Rakotomalala / Vigneau-Qannari):
#   - Centre d'un cluster = PC1 des variables du cluster (1ère composante principale)
#   - Allocation d'une variable j -> cluster k maximisant R²(X_j, PC1_k)
#   - Inertie d'un cluster = λ_k = Σ R²(X_j, PC1_k) pour j ∈ cluster_k
#   - Inertie totale = somme des λ_k (solution retenue = max inertie)
#   - Variance expliquée = λ_k / nombre_variables_cluster × 100
#
# =============================================================================

library(R6)

KMeansVariablesQuant <- R6::R6Class(
  "KMeansVariablesQuant",

  public = list(

    # ===========================
    # Champs publics
    # ===========================
    data = NULL,             # data.frame brut fourni à fit
    X_scaled = NULL,         # matrice standardisée (n x p)
    k = NULL,                # nombre de clusters
    max_iter = NULL,         # nombre max d'itérations par run
    n_init = NULL,           # nombre d'initialisations aléatoires
    tol = NULL,              # tolérance pour convergence
    seed = NULL,             # graine aléatoire (reproductibilité)

    # Résultats du clustering
    clusters = NULL,         # vecteur d'assignation finale (length p)
    centers = NULL,          # matrice (n x k) des PC1 de chaque cluster
    r2_matrix = NULL,        # matrice (p x k) des R² de chaque variable avec chaque PC1
    inertia_total = NULL,    # inertie totale = somme des λ_k
    inertia_by_cluster = NULL, # vecteur des λ_k (somme des R²)
    n_iter = NULL,           # nombre d'itérations du meilleur run

    # Paramètres de standardisation (pour predict)
    scale_center = NULL,
    scale_scale = NULL,

    # ===========================
    # Constructeur
    # ===========================
    initialize = function(k = 3, max_iter = 100, n_init = 10, tol = 1e-4, seed = NULL) {
      if (!is.numeric(k) || k < 2) {
        stop("'k' doit être un entier >= 2")
      }
      self$k <- as.integer(k)
      self$max_iter <- as.integer(max_iter)
      self$n_init <- as.integer(n_init)
      self$tol <- tol
      self$seed <- if (!is.null(seed)) as.integer(seed) else NULL
    },

    # ===========================
    # Validation des données
    # ===========================
    check_data = function(X, min_vars = 2) {
      if (is.matrix(X)) X <- as.data.frame(X)
      if (!is.data.frame(X)) {
        stop("X doit être un data.frame ou une matrice")
      }
      if (ncol(X) < min_vars) {
        stop(sprintf("Au moins %d variable(s) quantitative(s) requise(s)", min_vars))
      }
      if (anyNA(X)) {
        stop("Les données ne doivent pas contenir de NA")
      }
      if (!all(sapply(X, is.numeric))) {
        stop("Toutes les colonnes doivent être numériques")
      }

      # Assurer des noms de colonnes
      if (is.null(colnames(X))) {
        colnames(X) <- paste0("V", seq_len(ncol(X)))
      }

      X
    },

    # ===========================
    # FIT - Entraînement du modèle
    # ===========================
    fit = function(X, k = NULL) {
      # Validation des données
      X <- self$check_data(X)
      self$data <- X

      # Mise à jour de k si fourni
      if (!is.null(k)) {
        if (!is.numeric(k) || k < 2 || k > ncol(X)) {
          stop("k invalide: doit être entre 2 et le nombre de variables")
        }
        self$k <- as.integer(k)
      }

      # Vérifier que k <= p
      if (self$k > ncol(X)) {
        stop("k ne peut pas être supérieur au nombre de variables")
      }

      # Initialiser la graine aléatoire si fournie
      if (!is.null(self$seed)) set.seed(self$seed)

      # Standardisation des données (centrage + réduction)
      Xs <- scale(X)
      self$X_scaled <- as.matrix(Xs)
      self$scale_center <- attr(Xs, "scaled:center")
      self$scale_scale <- attr(Xs, "scaled:scale")

      n <- nrow(self$X_scaled)
      p <- ncol(self$X_scaled)
      K <- self$k

      # Initialisation multiple : tester n_init configurations
      best_inertia <- -Inf
      best <- NULL

      message(sprintf("K-means: %d initialisations avec k=%d clusters...", self$n_init, K))

      for (run in seq_len(self$n_init)) {
        # Graine différente pour chaque run
        if (!is.null(self$seed)) {
          set.seed(self$seed + run * 1000)
        }

        # Lancer un run complet
        res_run <- private$single_run(self$X_scaled, K, self$max_iter, self$tol)

        # Garder le meilleur (inertie maximale)
        if (res_run$inertia_total > best_inertia) {
          best_inertia <- res_run$inertia_total
          best <- res_run
        }
      }

      # Stocker les meilleurs résultats
      self$clusters <- best$clusters
      self$centers <- best$centers
      self$r2_matrix <- best$r2_matrix
      self$inertia_total <- best$inertia_total
      self$inertia_by_cluster <- best$inertia_by_cluster
      self$n_iter <- best$n_iter

      message(sprintf("Convergence atteinte après %d itérations (meilleur run)", self$n_iter))
      message(sprintf("Inertie totale: %.4f", self$inertia_total))

      invisible(self)
    },

    # ===========================
    # PREDICT - Variables illustratives
    # ===========================
    predict = function(X_new) {
      if (is.null(self$clusters) || is.null(self$centers)) {
        stop("fit() doit être exécuté avant predict()")
      }

      # Accepter n'importe quel nombre de variables (même 1 seule)
      X_new <- self$check_data(X_new, min_vars = 1)

      # CORRECTION CRITIQUE: Vérifier le nombre d'INDIVIDUS, pas de variables
      # X_new peut avoir n'importe quel nombre de variables (variables illustratives)
      # Mais doit avoir le même nombre d'individus pour calculer les corrélations
      if (nrow(X_new) != nrow(self$data)) {
        stop(sprintf("X_new doit avoir %d individus (actuellement: %d)",
                     nrow(self$data), nrow(X_new)))
      }

      # Standardiser les nouvelles variables (centrage et réduction)
      # Chaque variable est standardisée indépendamment
      Xn <- scale(X_new, center = TRUE, scale = TRUE)
      Xn <- as.matrix(Xn)

      # Calculer R² avec chaque centre (composante latente)
      cor_mat <- cor(Xn, self$centers)
      cor_mat[is.na(cor_mat)] <- 0
      r2_new <- cor_mat^2

      # Affecter au cluster avec R² max
      cl_new <- apply(r2_new, 1, which.max)
      r2_max <- apply(r2_new, 1, max)
      dist_new <- sqrt(1 - r2_max)

      result <- data.frame(
        variable = colnames(X_new),
        cluster = cl_new,
        r2_max = round(r2_max, 4),
        distance = round(dist_new, 4),
        stringsAsFactors = FALSE
      )

      # Ajouter un avertissement si R² très faible
      low_r2 <- result$r2_max < 0.30
      if (any(low_r2)) {
        warning(sprintf(
          "%d variable(s) ont un R² < 30%% : %s\nCes variables sont mal représentées par les clusters existants.",
          sum(low_r2),
          paste(result$variable[low_r2], collapse = ", ")
        ))
      }

      result[order(result$cluster, -result$r2_max), ]
    },

    # ===========================
    # SUMMARY - Résumé des résultats
    # ===========================
    summary = function(print_summary = FALSE) {
      if (is.null(self$clusters)) {
        stop("fit() doit être exécuté avant summary()")
      }

      X <- self$X_scaled
      var_names <- colnames(X)
      K <- self$k

      # =============================
      # 1. Cluster members (R² own/next/ratio)
      # =============================
      r2 <- self$r2_matrix

      # R² avec son propre cluster
      own <- r2[cbind(seq_len(nrow(r2)), self$clusters)]

      # R² maximum avec un autre cluster
      next_r2 <- vapply(seq_len(nrow(r2)), function(i) {
        k_i <- self$clusters[i]
        if (K == 1) return(0)
        max(r2[i, -k_i, drop = TRUE])
      }, numeric(1))

      # Ratio (1-R²own)/(1-R²next) : doit être << 1
      ratio <- (1 - own) / pmax(1 - next_r2, .Machine$double.eps)

      clust_members <- data.frame(
        cluster = self$clusters,
        variable = var_names,
        R2_own_pct = round(own * 100, 2),
        R2_next_pct = round(next_r2 * 100, 2),
        ratio = round(ratio, 3),
        stringsAsFactors = FALSE
      )
      clust_members <- clust_members[order(clust_members$cluster, -clust_members$R2_own_pct), ]

      # =============================
      # 2. Cluster summary
      # =============================
      cl_sizes <- as.numeric(table(factor(self$clusters, levels = 1:K)))
      lambdas <- self$inertia_by_cluster

      # Proportion de variance expliquée par chaque cluster
      prop <- lambdas / sum(lambdas)

      # CORRECTION: Variance expliquée = λ_k / p_k (d'après Ricco p.19)
      var_explained <- (lambdas / pmax(cl_sizes, 1)) * 100

      clust_summary <- data.frame(
        cluster = 1:K,
        n_members = cl_sizes,
        eigenvalue = round(lambdas, 4),
        variance_explained_pct = round(var_explained, 2),
        prop_inertia = round(prop, 4),
        stringsAsFactors = FALSE
      )

      # =============================
      # 3. Corrélations entre composantes latentes
      # =============================
      cor_latent <- cor(self$centers)
      colnames(cor_latent) <- rownames(cor_latent) <- paste0("Cluster_", 1:K)

      # =============================
      # Affichage optionnel
      # =============================
      if (print_summary) {
        self$print()
        cat("\n=== CLUSTER SUMMARY ===\n")
        print(clust_summary, row.names = FALSE)
        cat("\n=== CLUSTER MEMBERS (top variables) ===\n")
        print(head(clust_members, 15), row.names = FALSE)
        cat("\n=== CORRELATIONS ENTRE COMPOSANTES LATENTES ===\n")
        print(round(cor_latent, 3))
      }

      invisible(list(
        clust_summary = clust_summary,
        clust_members = clust_members,
        cor_latent = round(cor_latent, 4),
        inertia_total = self$inertia_total,
        r2_matrix = as.data.frame(round(self$r2_matrix, 4))
      ))
    },

    # ===========================
    # ELBOW - Courbe du coude
    # ===========================
    compute_elbow = function(k_range = 2:10) {
      if (is.null(self$X_scaled)) {
        stop("fit() doit être exécuté une fois avant compute_elbow()")
      }

      p <- ncol(self$X_scaled)
      k_range <- k_range[k_range >= 2 & k_range <= p]

      if (length(k_range) < 2) {
        stop("k_range invalide: doit contenir au moins 2 valeurs entre 2 et p")
      }

      message(sprintf("Calcul de l'elbow pour k in [%d, %d]...", min(k_range), max(k_range)))

      inertias <- numeric(length(k_range))
      for (i in seq_along(k_range)) {
        K <- k_range[i]
        # Utiliser plusieurs runs pour chaque k
        best_inertia <- -Inf
        for (trial in 1:5) {
          if (!is.null(self$seed)) set.seed(self$seed + i * 1000 + trial)
          res <- private$single_run(self$X_scaled, K, self$max_iter, self$tol)
          if (res$inertia_total > best_inertia) {
            best_inertia <- res$inertia_total
          }
        }
        inertias[i] <- best_inertia
      }

      data.frame(k = k_range, inertia = round(inertias, 4))
    },

    plot_elbow = function(k_range = 2:10) {
      df <- self$compute_elbow(k_range)

      graphics::plot(df$k, df$inertia, type = "b", pch = 19,
                     xlab = "Nombre de clusters k",
                     ylab = "Inertie totale (somme des λk)",
                     main = "Courbe elbow - K-means de variables",
                     col = "steelblue", lwd = 2,
                     ylim = range(df$inertia) * c(0.95, 1.05))

      # Marquer le k actuel si présent
      if (!is.null(self$clusters) && self$k %in% df$k) {
        graphics::points(self$k, df$inertia[df$k == self$k],
                         pch = 19, cex = 2, col = "red")
        graphics::abline(v = self$k, lty = 2, col = "red")
        graphics::text(self$k, df$inertia[df$k == self$k],
                       labels = paste0("k=", self$k),
                       pos = 4, col = "red", font = 2)
      }

      graphics::grid()
      invisible(df)
    },

    # ===========================
    # Visualisations
    # ===========================

    # Cercle de corrélation avec les variables (corrélations avec PC1 et PC2 globaux)
    plot_correlation_circle = function() {
      if (is.null(self$X_scaled) || is.null(self$clusters)) {
        stop("fit() doit être exécuté avant plot_correlation_circle()")
      }

      # ACP globale sur toutes les variables
      pca <- prcomp(self$X_scaled, center = TRUE, scale. = FALSE)

      # CORRECTION CRITIQUE: Calculer les VRAIES CORRÉLATIONS
      # cor(variables, composantes principales) au lieu de pca$rotation
      var_coords <- cor(self$X_scaled, pca$x[, 1:2])
      # var_coords[i, j] = corrélation entre variable i et composante principale j

      # Couleurs par cluster
      col <- rainbow(self$k)[self$clusters]

      # Calculer % variance expliquée
      var_expl <- summary(pca)$importance[2, 1:2] * 100

      # Échelles: cercle unité
      plot(var_coords[, 1], var_coords[, 2],
           xlim = c(-1, 1), ylim = c(-1, 1),
           xlab = sprintf("PC1 (%.1f%%)", var_expl[1]),
           ylab = sprintf("PC2 (%.1f%%)", var_expl[2]),
           main = "Cercle des corrélations (ACP globale)",
           type = "n",
           asp = 1)

      # Cercle unité
      t <- seq(0, 2*pi, length = 200)
      lines(cos(t), sin(t), col = "gray70", lty = 2)

      # Axes
      abline(h = 0, v = 0, lty = 3, col = "gray")

      # Flèches vers les variables
      arrows(0, 0, var_coords[, 1], var_coords[, 2],
             length = 0.1, col = col, lwd = 2)

      # Labels des variables
      text(var_coords[, 1], var_coords[, 2],
           labels = rownames(var_coords),
           pos = 3, cex = 0.8, col = col, font = 2)

      # Légende
      legend("topright",
             legend = paste("Cluster", 1:self$k),
             col = rainbow(self$k),
             lwd = 2,
             cex = 0.7,
             bg = "white")
    },

    # Projection des variables sur le premier plan factoriel global
    plot_variable_map = function() {
      if (is.null(self$X_scaled) || is.null(self$clusters)) {
        stop("fit() doit être exécuté avant plot_variable_map()")
      }

      # ACP globale sur toutes les variables
      pca <- prcomp(self$X_scaled, center = TRUE, scale. = FALSE)

      # Coordonnées des variables (loadings)
      scores <- pca$rotation[, 1:2]
      col <- rainbow(self$k)[self$clusters]

      # Calculer % variance expliquée
      var_expl <- summary(pca)$importance[2, 1:2] * 100

      # CORRECTION: Échelles ajustées automatiquement
      xlim <- range(scores[,1])
      ylim <- range(scores[,2])
      xlim <- xlim + c(-1, 1) * diff(xlim) * 0.15
      ylim <- ylim + c(-1, 1) * diff(ylim) * 0.15

      plot(scores[, 1], scores[, 2],
           col = col, pch = 19, cex = 1.5,
           xlab = sprintf("PC1 (%.1f%%)", var_expl[1]),
           ylab = sprintf("PC2 (%.1f%%)", var_expl[2]),
           main = "Projection des variables (plan factoriel global)",
           xlim = xlim,
           ylim = ylim)

      text(scores[, 1], scores[, 2],
           labels = colnames(self$X_scaled),
           pos = 3, cex = 0.8, col = col)

      abline(h = 0, v = 0, lty = 3, col = "gray")

      # Légende
      legend("topright",
             legend = paste("Cluster", 1:self$k),
             col = rainbow(self$k),
             pch = 19,
             cex = 0.8,
             bg = "white")

      # Note explicative
      total_var <- sum(var_expl)
      mtext(sprintf("Note: Projection 2D (%.1f%% variance) - Points proches ≠ même cluster", total_var),
            side = 1, line = 4, cex = 0.7, col = "gray40")
    },

    # Projection des centres latents sur le plan factoriel global
    plot_center_map = function() {
      if (is.null(self$centers) || is.null(self$clusters)) {
        stop("fit() doit être exécuté avant plot_center_map()")
      }

      # ACP globale
      pca <- prcomp(self$X_scaled, center = TRUE, scale. = FALSE)

      # Coordonnées des centres par corrélation
      center_coord <- cor(self$centers, pca$x)[, 1:2]

      col <- rainbow(self$k)

      plot(center_coord[, 1], center_coord[, 2],
           col = col, pch = 15, cex = 3,
           xlab = "Corrélation avec PC1",
           ylab = "Corrélation avec PC2",
           main = "Projection des centres latents (PC1_k)",
           xlim = c(-1, 1), ylim = c(-1, 1),
           asp = 1)

      # Cercle unité
      t <- seq(0, 2*pi, length = 200)
      lines(cos(t), sin(t), col = "gray70", lty = 2)

      text(center_coord[, 1], center_coord[, 2],
           labels = paste0("C", 1:self$k),
           pos = 3, col = col, font = 2)

      abline(h = 0, v = 0, lty = 3, col = "gray")
    },
    #' Elbow method to determine optimal number of clusters
    #'
    #' @param k_range Range of k values to test (default: 2:10)
    #' @param n_init Number of initializations per k (default: 20)
    #' @param plot Logical, whether to display the plot (default: TRUE)
    #' @return List with optimal_k, results data.frame, and plot function
    #' @export
    elbow = function(k_range = 2:10, n_init = 20, plot = TRUE) {
      if (is.null(self$data)) {
        stop("No data available. Use fit() first or provide data to elbow.")
      }

      # Call the standalone elbow function
      result <- kmeans_elbow(
        X_num = self$data,
        k_range = k_range,
        n_init = n_init,
        seed = self$seed
      )

      # Display plot if requested
      if (plot) {
        result$plot()
      }

      # Print summary
      cat("\n=== K-Means Elbow Analysis ===\n")
      cat(sprintf("Optimal k: %d\n", result$optimal_k))
      cat(sprintf("Range tested: %d to %d\n", min(k_range), max(k_range)))
      cat("\nResults table:\n")
      print(result$results)

      invisible(result)
    },

    # ===========================
    # PRINT
    # ===========================
    print = function(...) {
      cat("=== K-Means de Variables Quantitatives ===\n")
      cat(sprintf("k = %d | max_iter = %d | n_init = %d\n",
                  self$k, self$max_iter, self$n_init))

      if (!is.null(self$data)) {
        cat(sprintf("Données: %d individus x %d variables\n",
                    nrow(self$data), ncol(self$data)))
      }

      if (!is.null(self$clusters)) {
        cat(sprintf("Inertie totale: %.4f | Itérations: %d\n",
                    self$inertia_total, self$n_iter))
        cat("Taille des clusters:\n")
        print(table(self$clusters))
      } else {
        cat("Modèle non ajusté. Utilisez fit() pour entraîner.\n")
      }

      invisible(self)
    },
    get_clusters_table = function() {
      if (is.null(self$clusters))
        stop("fit() doit être exécuté avant get_clusters_table()")

      var_names <- colnames(self$data)

      df <- data.frame(
        variable = var_names,
        cluster = self$clusters,
        stringsAsFactors = FALSE
      )

      df[order(df$cluster, df$variable), ]
    }
  ),

  # =========================================================================
  # METHODES PRIVEES
  # =========================================================================
  private = list(

    # ---------------------------
    # Calcul de la composante latente (PC1) d'un cluster
    # ---------------------------
    latent_center = function(X_cluster) {
      n_vars <- ncol(X_cluster)
      n_obs <- nrow(X_cluster)

      # CAS 1: Cluster vide
      if (n_vars == 0) {
        warning("Cluster vide détecté - retour d'un centre aléatoire")
        fake <- rnorm(n_obs)
        fake <- scale(fake, center = TRUE, scale = TRUE)
        return(list(center = as.numeric(fake)))
      }

      # CAS 2: Une seule variable
      if (n_vars == 1) {
        v <- X_cluster[, 1]
        if (sd(v) < .Machine$double.eps) {
          warning("Variable avec variance nulle détectée")
          fake <- rnorm(n_obs)
          fake <- scale(fake, center = TRUE, scale = TRUE)
          return(list(center = as.numeric(fake)))
        }
        center <- as.numeric(scale(v, center = TRUE, scale = TRUE))
        return(list(center = center))
      }

      # CAS 3: Plusieurs variables -> ACP
      pca <- prcomp(X_cluster, center = TRUE, scale. = FALSE, rank. = 1)

      # Extraire le PC1 (scores factoriels)
      center <- pca$x[, 1]

      # Normaliser le PC1 (moyenne 0, écart-type 1)
      center <- as.numeric(scale(center, center = TRUE, scale = TRUE))

      # CAS 4: PC1 dégénéré
      if (anyNA(center) || sd(center) < .Machine$double.eps) {
        warning("PC1 avec variance nulle détecté")
        fake <- rnorm(n_obs)
        fake <- scale(fake, center = TRUE, scale = TRUE)
        return(list(center = as.numeric(fake)))
      }

      list(center = center)
    },

    # ---------------------------
    # Un run complet de K-means
    # ---------------------------
    single_run = function(X, K, max_iter, tol) {
      n <- nrow(X)
      p <- ncol(X)

      # Initialisation aléatoire
      clusters <- sample.int(K, p, replace = TRUE)

      # S'assurer qu'aucun cluster n'est vide
      for (k in 1:K) {
        if (sum(clusters == k) == 0) {
          clusters[sample.int(p, 1)] <- k
        }
      }

      # Structures pour stocker les résultats
      inertia_old <- -Inf
      centers <- matrix(0, nrow = n, ncol = K)
      r2_mat <- matrix(0, nrow = p, ncol = K)

      # Boucle principale
      for (iter in seq_len(max_iter)) {

        # ETAPE 1: Recalculer les centres (PC1) de chaque cluster
        for (k in 1:K) {
          idx <- which(clusters == k)
          lc <- private$latent_center(X[, idx, drop = FALSE])
          centers[, k] <- lc$center
        }

        # ETAPE 2: Calculer les R² entre chaque variable et chaque centre
        cor_mat <- cor(X, centers)
        cor_mat[is.na(cor_mat)] <- 0
        r2_mat <- cor_mat^2

        # ETAPE 3: Réaffecter chaque variable au cluster avec R² max
        clusters_new <- apply(r2_mat, 1, which.max)

        # ETAPE 4: Gérer les clusters vides
        for (k in 1:K) {
          if (sum(clusters_new == k) == 0) {
            farthest <- which.min(apply(r2_mat, 1, max))
            clusters_new[farthest] <- k
          }
        }

        # CORRECTION: Calculer l'inertie comme SOMME des R²
        # λ_k = Σ r²(X_j, Z_k) pour j ∈ cluster_k
        lambdas <- numeric(K)
        for (k in 1:K) {
          idx <- which(clusters_new == k)
          if (length(idx) > 0) {
            lambdas[k] <- sum(r2_mat[idx, k])
          }
        }
        inertia_new <- sum(lambdas)

        # CONVERGENCE
        if (identical(clusters_new, clusters)) {
          clusters <- clusters_new
          inertia_old <- inertia_new
          break
        }

        if (abs(inertia_new - inertia_old) < tol) {
          clusters <- clusters_new
          inertia_old <- inertia_new
          break
        }

        clusters <- clusters_new
        inertia_old <- inertia_new
      }

      # Recalcul final propre
      for (k in 1:K) {
        idx <- which(clusters == k)
        lc <- private$latent_center(X[, idx, drop = FALSE])
        centers[, k] <- lc$center
      }

      cor_mat <- cor(X, centers)
      cor_mat[is.na(cor_mat)] <- 0
      r2_mat <- cor_mat^2

      # CORRECTION: Lambda final = somme des R² dans chaque cluster
      lambdas <- numeric(K)
      for (k in 1:K) {
        idx <- which(clusters == k)
        if (length(idx) > 0) {
          lambdas[k] <- sum(r2_mat[idx, k])
        }
      }

      # Noms
      colnames(centers) <- paste0("Cluster_", 1:K)
      colnames(r2_mat) <- paste0("Cluster_", 1:K)
      rownames(r2_mat) <- colnames(X)

      list(
        clusters = as.integer(clusters),
        centers = centers,
        r2_matrix = r2_mat,
        inertia_total = sum(lambdas),
        inertia_by_cluster = lambdas,
        n_iter = iter
      )
    }
  )
)
