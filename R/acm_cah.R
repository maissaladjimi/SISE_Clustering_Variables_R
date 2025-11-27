################################################################################
# ClustModalities : Clustering de modalités qualitatives
#
# ─ Méthodes :
#   • method = "dice"
#       - distance Dice^2 sur indicatrices
#       - CAH ward.D2
#
#   • method = "acm"
#       - ACM (COA) sur tableau disjonctif
#       - coordonnées mises à l’échelle sqrt(lambda)
#       - PAS de pondération W (aligné Ricco)
#       - CAH ward.D
#
# ─ Outils :
#   - Scree plot (% inertie ou cumulée)
#   - Contributions (% correctes)
#   - Dendrogramme
#   - Carte factorielle
#   - Elbow (sans k*)
#   - project_numeric() (cercle corrélation robuste)
#   - predict() (corrigé)
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

        # 2A) Dice^2 -> CAH
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

      if (is.null(k_max)) k_max <- min(10, n_mod)  # focus sur petits k

      # hauteurs pour k = 1..n_mod
      h_rev <- rev(h)                 # h_rev[1] = hauteur k=1
      heights_k <- c(h_rev, 0)        # k=n_mod -> 0

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
    #'
    #' @param k_max Maximum number of clusters to consider (default: 10)
    #' @param plot Logical, whether to display the plot (default: TRUE)
    #' @return List with optimal_k, results data.frame, and plot function
    #' @export
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
    plot_factor_map = function(dim1=1, dim2=2, show_labels=TRUE) {
      if (self$method != "acm")
        stop("plot_factor_map() uniquement en ACM.")

      if (is.null(self$mod_coords))
        stop("fit() doit être exécuté avant.")

      x <- self$mod_coords[,dim1]
      y <- self$mod_coords[,dim2]

      clusters <- if (!is.null(self$mod_clusters))
        as.integer(as.factor(self$mod_clusters)) else 1

      xr <- range(x); yr <- range(y)

      plot(x, y, col=clusters, pch=19,
           xlab=paste0("Dim ",dim1),
           ylab=paste0("Dim ",dim2),
           xlim=xr + c(-1,1)*.2*diff(xr),
           ylim=yr + c(-1,1)*.2*diff(yr),
           main="Carte factorielle des modalités (ACM)")

      if (show_labels)
        text(x, y, labels=rownames(self$mod_coords), pos=3, cex=.75)

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
    # Variables quantitatives illustratives — cercle corrélations
    # =====================================================================
    project_numeric = function(X_quant, plot = TRUE) {
      if (self$method != "acm")
        stop("project_numeric() disponible uniquement pour method='acm'.")
      if (is.null(self$ind_coords))
        stop("fit() doit être exécuté avant project_numeric().")

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

          # >>> correction clé : enlever les noms portés par mod_clusters
          cluster_i <- unname(self$mod_clusters[closest_mod])
          dist_i <- sqrt(d2[ix])

          c(cluster = cluster_i, distance = dist_i)
        }

        res <- t(sapply(seq_len(ncol(new_disj)), function(j)
          assign_cl(new_disj[, j])
        ))

        # colonnes garanties clean
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
    # PRINT / SUMMARY
    # =====================================================================
    print = function(...) {
      cat("ClustModalities (modalités qualitatives)\n")
      cat("method :", self$method,"\n")
      if (!is.null(self$data))
        cat("n individus :",nrow(self$data),
            " | variables :",ncol(self$data),
            " | modalités :", if (!is.null(self$disj)) ncol(self$disj),"\n")
      if (!is.null(self$k)) cat("k =", self$k,"\n")
      invisible(self)
    },

    summary = function(...) {
      self$print()
      if (self$method=="acm" && !is.null(self$eig_raw)) {
        cat("\nValeurs propres (5 premières) :\n"); print(head(self$eig_raw,5))
        cat("\nBenzécri (5 premières) :\n"); print(head(self$eig_benzecri,5))
        cat("\nGreenacre (5 premières) :\n"); print(head(self$eig_greenacre,5))
      }
      if (!is.null(self$mod_clusters)) {
        cat("\nTaille clusters :\n"); print(table(self$mod_clusters))
      }
      invisible(self)
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
# FIN VERSION FINALE
################################################################################
