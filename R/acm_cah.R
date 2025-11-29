################################################################################
# ClustModalities : Clustering de modalités qualitatives
#
# VERSION AMÉLIORÉE - AJOUTS:
# 1. ✅ Méthode illustrative() pour variables qualitatives illustratives
# 2. ✅ summary() amélioré avec statistiques détaillées
# 3. ✅ print() amélioré avec plus d'informations
# 4. ✅ project_numeric() renommé en illustrative_numeric() pour cohérence
#
# ─ Méthodes :
#   • method = "dice"
#       - distance Dice^2 sur indicatrices
#       - CAH ward.D2
#
#   • method = "acm"
#       - ACM (COA) sur tableau disjonctif
#       - coordonnées mises à l'échelle sqrt(lambda)
#       - PAS de pondération W (aligné Ricco)
#       - CAH ward.D
#
################################################################################

ClustModalities <- R6::R6Class(
  "ClustModalities",
  public = list(

    # =====================================================================
    # Champs
    # =====================================================================
    data = NULL,
    disj = NULL,
    method = NULL,

    # ACM
    n_axes = NULL,
    acm = NULL,
    eig_raw = NULL,
    eig_benzecri = NULL,
    eig_greenacre = NULL,
    ind_coords = NULL,
    mod_coords = NULL,

    # Clustering
    dist_mat = NULL,
    hclust = NULL,
    k = NULL,
    mod_clusters = NULL,

    # =====================================================================
    # Constructeur
    # =====================================================================
    initialize = function(method = c("dice", "acm"), n_axes = NULL) {
      self$method <- match.arg(method)
      self$n_axes <- n_axes
    },

    # =====================================================================
    # Vérification des données
    # =====================================================================
    check_data = function(X) {
      if (is.matrix(X)) X <- as.data.frame(X)
      if (!is.data.frame(X))
        stop("'X' doit être un data.frame ou une matrice.")
      if (ncol(X) < 1)
        stop("'X' doit contenir au moins 1 variable qualitative.")
      if (anyNA(X))
        stop("Les données ne doivent pas contenir de NA.")

      X[] <- lapply(X, function(col) if (!is.factor(col)) factor(col) else col)
      X
    },

    # =====================================================================
    # Distance Dice^2
    # =====================================================================
    compute_dice_distances = function(disj_mat) {
      disj_mat <- as.matrix(disj_mat)
      Q <- ncol(disj_mat)
      d2 <- matrix(0, Q, Q)
      colnames(d2) <- rownames(d2) <- colnames(disj_mat)

      for (j in seq_len(Q)) {
        for (j2 in j:Q) {
          diff_vec <- disj_mat[, j] - disj_mat[, j2]
          val <- 0.5 * sum(diff_vec * diff_vec)
          d2[j,j2] <- d2[j2,j] <- val
        }
      }
      as.dist(sqrt(d2))
    },

    # =====================================================================
    # FIT
    # =====================================================================
    fit = function(X, k = NULL) {
      X <- self$check_data(X)
      self$data <- X
      self$k <- k
      self$disj <- ade4::acm.disjonctif(X)

      # ------------------ DICE ------------------
      if (self$method == "dice") {

        # Dice^2 -> CAH
        self$dist_mat <- self$compute_dice_distances(self$disj)

        # IMPORTANT: labels bien alignés
        attr(self$dist_mat, "Labels") <- colnames(self$disj)

        self$hclust <- stats::hclust(self$dist_mat, method = "ward.D2")

        # ------------------ ACM ------------------
      } else {

        nf <- if (is.null(self$n_axes)) 2 else min(self$n_axes, 2)

        acm_res <- ade4::dudi.coa(self$disj, scannf = FALSE, nf = nf)
        self$acm <- acm_res

        # valeurs propres
        self$eig_raw <- as.numeric(acm_res$eig)
        Q <- ncol(self$disj)
        K <- ncol(self$data)

        eig_b <- (Q/(Q-1)) * (self$eig_raw - 1/Q)
        eig_b[eig_b < 0] <- 0
        self$eig_benzecri <- eig_b
        self$eig_greenacre <- eig_b * (K/Q)

        # Coordonnées modalités (brutes)
        coords <- as.matrix(acm_res$co[,1:nf])
        storage.mode(coords) <- "double"

        # mise à l'échelle sqrt(lambda)
        eig_nf <- as.numeric(acm_res$eig[1:nf])
        coords_scaled <- sweep(coords, 2, sqrt(eig_nf), FUN = "*")
        colnames(coords_scaled) <- paste0("Dim",1:nf)
        self$mod_coords <- coords_scaled

        # distances sur coords scaled (PAS de pondération W)
        self$dist_mat <- dist(coords_scaled)
        attr(self$dist_mat, "Labels") <- rownames(coords_scaled)
        self$hclust <- hclust(self$dist_mat, method = "ward.D")

        # coords individus
        ind <- as.matrix(acm_res$li[,1:nf])
        ind_scaled <- sweep(ind, 2, sqrt(eig_nf), FUN="*")
        colnames(ind_scaled) <- paste0("Dim",1:nf)
        self$ind_coords <- ind_scaled
      }

      # coupe
      if (!is.null(self$k))
        self$mod_clusters <- cutree(self$hclust, k = self$k)

      invisible(self)
    },

    # =====================================================================
    # COURBE ELBOW
    # =====================================================================
    compute_elbow = function(k_max = NULL) {
      if (is.null(self$hclust))
        stop("fit() doit être exécuté avant compute_elbow().")

      h <- self$hclust$height
      n_mod <- length(self$hclust$order)

      if (is.null(k_max)) k_max <- min(10, n_mod)

      # hauteurs pour k = 1..n_mod
      h_rev <- rev(h)
      heights_k <- c(h_rev, 0)

      k_vals <- 1:n_mod
      df <- data.frame(
        k = k_vals,
        height = heights_k
      )

      df[df$k <= k_max, , drop = FALSE]
    },

    plot_elbow = function(k_max = NULL) {
      df <- self$compute_elbow(k_max = k_max)

      graphics::plot(df$k, df$height, type="b", pch=19,
                     xlab="Nombre de clusters k",
                     ylab="Hauteur d'agglomération",
                     main="Courbe des hauteurs (aggregation levels)")
      invisible(df)
    },

    #' Elbow method with automatic detection of optimal k
    elbow = function(k_max = 10, plot = TRUE) {
      if (is.null(self$data)) {
        stop("No data available. Use fit() first.")
      }

      # Call the standalone elbow function
      result <- acm_cah_elbow(
        X_quali = self$data,
        method = self$method,
        k_max = k_max
      )

      # Display plot if requested
      if (plot) {
        result$plot()
      }

      # Print summary
      cat("\n=== ACM-CAH Elbow Analysis ===\n")
      cat(sprintf("Method: %s\n", self$method))
      cat(sprintf("Optimal k: %d\n", result$optimal_k))
      cat(sprintf("Range tested: 1 to %d\n", k_max))
      cat("\nResults table:\n")
      print(result$results)

      invisible(result)
    },

    # =====================================================================
    # DENDROGRAMME
    # =====================================================================
    plot_dendrogram = function(k = NULL, ...) {
      if (is.null(self$hclust))
        stop("fit() doit être exécuté avant plot_dendrogram().")

      plot(self$hclust,
           main = paste("Dendrogramme des modalités -", self$method),
           xlab = "", sub = "")

      kk <- if (!is.null(k)) k else self$k
      if (!is.null(kk))
        rect.hclust(self$hclust, k = kk, border = 2:(kk+1))
    },

    # =====================================================================
    # CARTE FACTORIELLE (ACM)
    # =====================================================================
    plot_factorial_map = function(dims = c(1,2), k = NULL) {
      if (self$method != "acm")
        stop("plot_factorial_map() uniquement ACM.")
      if (is.null(self$mod_coords))
        stop("fit() d'abord.")

      coords <- self$mod_coords[, dims]
      kk <- if (!is.null(k)) k else self$k

      if (!is.null(kk) && !is.null(self$mod_clusters)) {
        cols <- rainbow(kk)[self$mod_clusters]
      } else {
        cols <- "black"
      }

      eig <- self$eig_raw[dims]
      perc <- 100 * eig / sum(self$eig_raw)

      plot(coords[,1], coords[,2], pch=19, col=cols,
           xlab=paste0("Dim", dims[1], " (", round(perc[1],2), "%)"),
           ylab=paste0("Dim", dims[2], " (", round(perc[2],2), "%)"),
           main="Carte factorielle (modalités)")
      text(coords[,1], coords[,2], labels=rownames(coords), pos=3, cex=.8,font=2)

      abline(h=0,v=0,lty=3)
    },

    # =====================================================================
    # SCREE PLOT (% inertie)
    # =====================================================================
    plot_scree = function(cumulative = FALSE) {
      if (self$method != "acm")
        stop("plot_scree() uniquement ACM.")
      if (is.null(self$eig_raw))
        stop("fit() d'abord.")

      eig <- self$eig_raw
      perc <- 100 * eig / sum(eig)

      if (!cumulative) {
        plot(seq_along(perc), perc, type="b", pch=19,
             xlab="Dimension", ylab="% inertie expliquée",
             main="Scree plot (ACM)")
      } else {
        plot(seq_along(perc), cumsum(perc), type="b", pch=19,
             xlab="Dimension", ylab="% inertie cumulée",
             main="Inertie cumulée (ACM)")
      }
      abline(h=0,lty=3)
      invisible(perc)
    },

    # =====================================================================
    # CONTRIBUTIONS (%) MCA
    # =====================================================================
    plot_contrib = function(dim = 1, top = NULL) {
      if (self$method != "acm")
        stop("plot_contrib() uniquement ACM.")

      mass <- as.numeric(self$acm$cw)
      coords <- self$mod_coords[, dim]
      lambda <- as.numeric(self$acm$eig[dim])

      contrib <- 100 * (mass * coords^2) / lambda
      contrib <- sort(contrib, decreasing = TRUE)

      if (!is.null(top))
        contrib <- contrib[1:min(top, length(contrib))]

      barplot(contrib, las=2, cex.names=.7,
              main=paste("Contributions (%) — Axe",dim),
              ylab="Contribution (%)")

      invisible(contrib)
    },

    # =====================================================================
    # ILLUSTRATIVE NUMERIC - Variables quantitatives (cercle corrélations)
    # =====================================================================
    illustrative_numeric = function(X_quant, plot = TRUE) {
      if (self$method != "acm")
        stop("illustrative_numeric() disponible uniquement pour method='acm'.")
      if (is.null(self$ind_coords))
        stop("fit() doit être exécuté avant illustrative_numeric().")

      if (!is.data.frame(X_quant))
        stop("X_quant doit être un data.frame de variables numériques.")
      if (nrow(X_quant) != nrow(self$data))
        stop("X_quant doit avoir le même nombre d'individus.")
      if (!all(sapply(X_quant, is.numeric)))
        stop("Toutes les colonnes de X_quant doivent être numériques.")

      CF <- self$ind_coords

      cors <- sapply(X_quant, function(v){
        c(cor(v, CF[,1]), cor(v, CF[,2]))
      })
      cors <- t(cors)
      colnames(cors) <- c("cor.Dim1","cor.Dim2")

      if (plot) {
        graphics::plot(cors[,1], cors[,2], pch=19,
                       xlim=c(-1,1), ylim=c(-1,1),
                       xlab="Corrélation Dim1", ylab="Corrélation Dim2",
                       main="Cercle des corrélations — illustratives quantitatives")

        # Cercle complet
        t <- seq(0, 2*pi, length.out=200)
        lines(cos(t), sin(t), col="grey70")

        abline(h=0, v=0, lty=3)
        text(cors[,1], cors[,2], labels=rownames(cors), pos=3)
      }

      return(as.data.frame(cors))
    },

    # =====================================================================
    # ILLUSTRATIVE - Variables qualitatives illustratives
    # =====================================================================
    illustrative = function(X_illust, plot = TRUE) {
      if (is.null(self$mod_clusters))
        stop("fit() avec k doit être exécuté avant illustrative().")

      X_illust <- self$check_data(X_illust)

      if (nrow(X_illust) != nrow(self$data))
        stop("X_illust doit avoir le même nombre d'individus que les données d'entraînement.")

      # Créer le tableau disjonctif des variables illustratives
      disj_illust <- ade4::acm.disjonctif(X_illust)

      K <- self$k
      n_mod_illust <- ncol(disj_illust)

      # ============ MÉTHODE DICE ============
      if (self$method == "dice") {

        # Fonction distance Dice²
        dice_dist <- function(m1, m2) {
          0.5 * sum((m1 - m2)^2)
        }

        # Pour chaque modalité illustrative, calculer distance moyenne aux membres de chaque cluster
        result_list <- list()

        for (m in colnames(disj_illust)) {
          vec_m <- disj_illust[, m]

          # Distance moyenne à chaque cluster
          mean_dist <- numeric(K)

          for (k in 1:K) {
            # Modalités membres du cluster k
            members_k <- names(self$mod_clusters)[self$mod_clusters == k]

            # Calculer distances
            dists <- sapply(members_k, function(member) {
              dice_dist(vec_m, self$disj[, member])
            })

            mean_dist[k] <- mean(dists)
          }

          # Cluster assigné (distance minimale)
          assigned_cluster <- which.min(mean_dist)

          result_list[[m]] <- data.frame(
            modality = m,
            cluster_assigned = assigned_cluster,
            t(mean_dist),
            stringsAsFactors = FALSE
          )
        }

        result_table <- do.call(rbind, result_list)
        colnames(result_table)[3:(2+K)] <- paste0("dist_cluster_", 1:K)
        rownames(result_table) <- NULL

        # ============ MÉTHODE ACM ============
      } else {

        # Projeter les modalités illustratives dans l'espace factoriel ACM
        L <- ncol(self$ind_coords)
        coords_illust <- matrix(0, nrow = n_mod_illust, ncol = L,
                                dimnames = list(colnames(disj_illust),
                                                colnames(self$ind_coords)))

        for (m in colnames(disj_illust)) {
          idx <- which(disj_illust[, m] == 1)
          if (length(idx) > 0)
            coords_illust[m, ] <- colMeans(self$ind_coords[idx, , drop = FALSE])
        }

        # Calculer barycentres des clusters
        centers <- rowsum(self$mod_coords, self$mod_clusters) /
          as.numeric(table(self$mod_clusters))

        # Pour chaque modalité illustrative, calculer distance à chaque barycentre
        result_list <- list()

        for (m in colnames(disj_illust)) {
          dists <- apply(centers, 1, function(center) {
            sqrt(sum((coords_illust[m, ] - center)^2))
          })

          assigned_cluster <- which.min(dists)

          result_list[[m]] <- data.frame(
            modality = m,
            cluster_assigned = assigned_cluster,
            t(dists),
            stringsAsFactors = FALSE
          )
        }

        result_table <- do.call(rbind, result_list)
        colnames(result_table)[3:(2+K)] <- paste0("dist_cluster_", 1:K)
        rownames(result_table) <- NULL
      }

      # Fonction de visualisation
      plot_func <- function() {
        if (self$method == "acm" && !is.null(self$mod_coords)) {
          # Carte factorielle avec modalités actives et illustratives
          coords_active <- self$mod_coords[, 1:2]

          plot(coords_active[, 1], coords_active[, 2],
               pch = 19, col = rainbow(K)[self$mod_clusters],
               xlab = "Dim 1", ylab = "Dim 2",
               main = "Modalités actives (couleur) et illustratives (rouge)")

          text(coords_active[, 1], coords_active[, 2],
               labels = rownames(coords_active), pos = 3, cex = 0.6,
               col = rainbow(K)[self$mod_clusters])

          # Ajouter modalités illustratives en rouge
          if (exists("coords_illust")) {
            points(coords_illust[, 1], coords_illust[, 2],
                   pch = 17, col = "red", cex = 1.5)
            text(coords_illust[, 1], coords_illust[, 2],
                 labels = rownames(coords_illust), pos = 3, cex = 0.7,
                 col = "red", font = 2)
          }

          abline(h = 0, v = 0, lty = 3)

          legend("topright",
                 legend = c(paste("Cluster", 1:K), "Illustrative"),
                 col = c(rainbow(K), "red"),
                 pch = c(rep(19, K), 17),
                 cex = 0.8)
        } else {
          # Pour DICE, barplot des distances
          par(mfrow = c(min(2, ceiling(nrow(result_table)/2)), 2))

          for (i in 1:min(nrow(result_table), 4)) {
            dists <- as.numeric(result_table[i, 3:(2+K)])
            assigned <- result_table$cluster_assigned[i]

            barplot(dists,
                    names.arg = paste0("C", 1:K),
                    main = paste("Modalité:", result_table$modality[i]),
                    ylab = "Distance moyenne",
                    col = ifelse(seq_len(K) == assigned, "steelblue", "grey80"),
                    border = "black")
          }

          par(mfrow = c(1, 1))
        }
      }

      result <- list(
        table = result_table,
        plot = plot_func
      )

      if (plot) {
        plot_func()
      }

      invisible(result)
    },

    # =====================================================================
    # Table clusters
    # =====================================================================
    cluster_table = function(k = NULL) {
      if (is.null(self$hclust))
        stop("fit() doit être exécuté avant cluster_table().")

      kk <- if (!is.null(k)) k else self$k
      if (is.null(kk))
        stop("k manquant.")

      cl <- cutree(self$hclust, k = kk)
      out <- data.frame(
        cluster = cl,
        modality = names(cl),
        stringsAsFactors = FALSE
      )
      out[order(out$cluster, out$modality),]
    },

    # =====================================================================
    # PREDICT
    # =====================================================================
    predict = function(X_new) {
      if (is.null(self$hclust))
        stop("fit() d'abord.")
      if (is.null(self$k))
        stop("fit(...,k=) requis.")

      if (is.matrix(X_new)) X_new <- as.data.frame(X_new)
      X_new <- self$check_data(X_new)

      if (nrow(X_new) != nrow(self$data))
        stop("Nombre d'individus incompatible.")

      disj_new <- ade4::acm.disjonctif(X_new)

      # ------------ DICE ------------
      if (self$method == "dice") {

        old_disj <- as.matrix(self$disj)
        new_disj <- as.matrix(disj_new)
        old_names <- colnames(old_disj)
        new_names <- colnames(new_disj)

        assign_cl <- function(vec_new) {
          d2 <- apply(old_disj, 2, function(col_old) {
            0.5 * sum((vec_new - col_old)^2)
          })
          ix <- which.min(d2)
          closest_mod <- old_names[ix]

          cluster_i <- unname(self$mod_clusters[closest_mod])
          dist_i <- sqrt(d2[ix])

          c(cluster = cluster_i, distance = dist_i)
        }

        res <- t(sapply(seq_len(ncol(new_disj)), function(j)
          assign_cl(new_disj[, j])
        ))

        colnames(res) <- c("cluster", "distance")

        df <- data.frame(
          modality = new_names,
          cluster = as.integer(res[, "cluster"]),
          distance = as.numeric(res[, "distance"]),
          stringsAsFactors = FALSE
        )
        return(df[order(df$cluster, df$distance), ])
      }

      # ------------ ACM -------------
      L <- ncol(self$ind_coords)
      coords_new <- matrix(0, nrow=ncol(disj_new), ncol=L,
                           dimnames=list(colnames(disj_new),
                                         colnames(self$ind_coords)))

      for (m in colnames(disj_new)) {
        idx <- which(disj_new[,m] == 1)
        if (length(idx) > 0)
          coords_new[m,] <- colMeans(self$ind_coords[idx,,drop=FALSE])
      }

      centers <- rowsum(self$mod_coords, self$mod_clusters) /
        as.numeric(table(self$mod_clusters))

      res_cluster <- apply(coords_new, 1, function(v)
        which.min(colSums((t(centers) - v)^2))
      )
      res_dist <- apply(coords_new, 1, function(v)
        min(sqrt(colSums((t(centers) - v)^2)))
      )

      df <- data.frame(
        modality = rownames(coords_new),
        cluster = res_cluster,
        distance = res_dist
      )
      df[order(df$cluster, df$distance),]
    },

    # =====================================================================
    # PRINT / SUMMARY - AMÉLIORÉS
    # =====================================================================
    print = function(...) {
      cat("========================================\n")
      cat("  CLUSTERING DE MODALITÉS QUALITATIVES\n")
      cat("========================================\n")
      cat(sprintf("Méthode: %s\n", toupper(self$method)))

      if (!is.null(self$data)) {
        cat(sprintf("\nDonnées:\n"))
        cat(sprintf("  - Individus                 : %d\n", nrow(self$data)))
        cat(sprintf("  - Variables qualitatives    : %d\n", ncol(self$data)))
        if (!is.null(self$disj)) {
          cat(sprintf("  - Modalités totales         : %d\n", ncol(self$disj)))
        }
      }

      if (!is.null(self$k)) {
        cat(sprintf("\nClustering:\n"))
        cat(sprintf("  - Nombre de clusters (k)    : %d\n", self$k))
        if (!is.null(self$mod_clusters)) {
          tbl <- table(self$mod_clusters)
          cat(sprintf("\nTaille des clusters:\n"))
          for (i in 1:length(tbl)) {
            cat(sprintf("  - Cluster %d                 : %d modalités\n", i, tbl[i]))
          }
        }
      }

      if (self$method == "acm" && !is.null(self$eig_raw)) {
        cat(sprintf("\nACM:\n"))
        cat(sprintf("  - Inertie totale            : %.4f\n", sum(self$eig_raw)))
        cat(sprintf("  - Inertie Dim1              : %.2f%%\n",
                    100 * self$eig_raw[1] / sum(self$eig_raw)))
        cat(sprintf("  - Inertie Dim2              : %.2f%%\n",
                    100 * self$eig_raw[2] / sum(self$eig_raw)))
      }

      cat("========================================\n")
      invisible(self)
    },

    summary = function(print_output = TRUE) {
      if (is.null(self$data))
        stop("fit() doit être exécuté avant summary().")

      # Statistiques de base
      basic_stats <- data.frame(
        metric = c("Nombre d'individus", "Nombre de variables", "Nombre de modalités"),
        value = c(nrow(self$data), ncol(self$data),
                  if (!is.null(self$disj)) ncol(self$disj) else NA),
        stringsAsFactors = FALSE
      )

      # Statistiques ACM si applicable
      acm_stats <- NULL
      if (self$method == "acm" && !is.null(self$eig_raw)) {
        inertia_pct <- 100 * self$eig_raw / sum(self$eig_raw)
        acm_stats <- data.frame(
          dimension = 1:min(5, length(self$eig_raw)),
          eigenvalue = round(head(self$eig_raw, 5), 4),
          inertia_pct = round(head(inertia_pct, 5), 2),
          cumulative_pct = round(head(cumsum(inertia_pct), 5), 2),
          stringsAsFactors = FALSE
        )
      }

      # Statistiques des clusters si k défini
      cluster_stats <- NULL
      cluster_composition <- NULL

      if (!is.null(self$mod_clusters)) {
        # Taille des clusters
        cluster_sizes <- as.numeric(table(self$mod_clusters))
        cluster_stats <- data.frame(
          cluster = 1:self$k,
          n_modalities = cluster_sizes,
          pct_total = round(100 * cluster_sizes / sum(cluster_sizes), 2),
          stringsAsFactors = FALSE
        )

        # Composition détaillée par cluster
        cluster_composition <- data.frame(
          cluster = self$mod_clusters,
          modality = names(self$mod_clusters),
          stringsAsFactors = FALSE
        )
        cluster_composition <- cluster_composition[order(cluster_composition$cluster,
                                                         cluster_composition$modality), ]
      }

      if (print_output) {
        self$print()

        cat("\n========================================\n")
        cat("  STATISTIQUES DE BASE\n")
        cat("========================================\n")
        print(basic_stats, row.names = FALSE)

        if (!is.null(acm_stats)) {
          cat("\n========================================\n")
          cat("  VALEURS PROPRES ACM\n")
          cat("========================================\n")
          print(acm_stats, row.names = FALSE)
        }

        if (!is.null(cluster_stats)) {
          cat("\n========================================\n")
          cat("  STATISTIQUES DES CLUSTERS\n")
          cat("========================================\n")
          print(cluster_stats, row.names = FALSE)

          cat("\n========================================\n")
          cat("  COMPOSITION DES CLUSTERS (top 30)\n")
          cat("========================================\n")
          print(head(cluster_composition, 30), row.names = FALSE)
        }
      }

      invisible(list(
        basic_stats = basic_stats,
        acm_stats = acm_stats,
        cluster_stats = cluster_stats,
        cluster_composition = cluster_composition,
        method = self$method
      ))
    },

    get_clusters_table = function() {
      if (is.null(self$mod_clusters))
        stop("fit() doit être exécuté avec k avant get_clusters_table()")

      df <- data.frame(
        modality = names(self$mod_clusters),
        cluster = unname(self$mod_clusters),
        stringsAsFactors = FALSE
      )

      df[order(df$cluster, df$modality), ]
    }

  )
)

################################################################################
# FIN VERSION AMÉLIORÉE
################################################################################
