#' Variable Clustering R6 Class using VarClus
#'
#' An R6 class for performing variable clustering on numeric datasets.
#' Implements hierarchical clustering via `Hmisc::varclus`, with support for
#' elbow-based number of clusters selection, dendrogram plotting, and correlation heatmaps.
#'
#' @import R6
#' @import Hmisc
#' @import dendextend
#' @import plotly
#' @export
VarClus <- R6::R6Class(
  "VarClus",


  public = list(

    # --------------------
    # Fields
    # --------------------
    #' @field similarity Similarity measure used in clustering (default "pearson")
    similarity = NULL,
    #' @field n_clusters Number of clusters to cut the dendrogram (auto if NULL)
    n_clusters = NULL,
    #' @field model The fitted VarClus model object
    model = NULL,
    #' @field clusters Data frame of variable names and their cluster assignments
    clusters = NULL,
    #' @field data The numeric dataset used for clustering
    data = NULL,
    #' @field dendo Internal dendrogram object
    dendo = NULL,
    #' @field plot_elbow Plot object from the elbow method
    plot_elbow = NULL,


    # --------------------
    # Constructor
    # --------------------
    #' @description Initialize a new VarClus object
    #' @param similarity Character string. Similarity measure used for clustering (default "pearson").
    #' @param n_clusters Integer. Optional number of clusters; if NULL, it will be determined automatically.
    initialize = function(similarity = "pearson", n_clusters = NULL) {
      self$similarity <- similarity
      self$n_clusters <- n_clusters
    },



    # --------------------
    # Fit Method
    # --------------------
    #' @description Fit the clustering model
    #' @param X_num A numeric data frame or matrix
    fit = function(X_num) {

      self$data <- X_num

      # Validate input
      if (!is.numeric(X_num) && !is.matrix(X_num) && !all(sapply(X_num, is.numeric))) {
        stop("Input X must be fully numeric")
      }

      if (ncol(X_num) < 2) stop("At least two numeric variables are required.")

      # Run VarClus
      self$model <- Hmisc::varclus(x = X_num, similarity = self$similarity)

      # Run elbow method
      if (!exists("varclus_elbow")) stop("Function varclus_elbow() not found.")
      res <- varclus_elbow(X_num, similarity = self$similarity)
      self$plot_elbow <- res$plot

      # Set number of clusters automatically if not specified
      if (is.null(self$n_clusters)) {
        self$n_clusters <- res$optimal_k
      }

      # Cut dendrogram into clusters
      clust <- cutree(self$model$hclust, k = self$n_clusters)
      self$clusters <- data.frame(
        variable = names(clust),
        cluster = clust,
        stringsAsFactors = FALSE
      )

      invisible(self)
    },



    # --------------------
    # Get dendrogram
    # --------------------
    #' @description plots the dendrogram of clustered variables
    #' @return A function that plots the dendrogram
    get_dendrogram = function() {

      if (is.null(self$model) || is.null(self$clusters)) stop("Model not yet fitted or clusters not computed.")

      hc <- self$model$hclust
      dend <- as.dendrogram(hc)
      k <- length(unique(self$clusters$cluster))
      cols <- rainbow(k)

      dend <- dendextend::color_branches(dend, k = k, col = cols)


      function() {
        plot(dend, horiz = FALSE, main = NULL, xlab = NULL)
      }
    },



    # --------------------
    # Get heatmap
    # --------------------
    #' @description plots the correlation heatmap of the variables
    #' @return A function that plots the heatmap
    get_heatmap = function() {

      if (is.null(self$model)) stop("Model not yet fitted.")

      cor_mat <- self$model$sim

      function() {
        plotly::plot_ly(
          x = colnames(cor_mat),
          y = rownames(cor_mat),
          z = cor_mat,
          type = "heatmap",
          colorscale = "Oranges",
          zmin = min(cor_mat),
          zmax = max(cor_mat)
        ) %>%
          layout(
            xaxis = list(title = ""),
            yaxis = list(title = "", autorange = "reversed")
          )
      }
    },

    #-------------
    # Predict Method
    #-------------

    predict = function(new_var) {

      if (is.null(self$model) || is.null(self$clusters)) {
        stop("Model must be fitted before predicting.")
      }

      if (!is.numeric(new_var)) stop("new_var must be numeric.")
      if (length(new_var) != nrow(self$data)) {
        stop("new_var must have the same number of rows as the training data.")
      }

      if (!all(length(new_var) == nrow(self$data))) {
        new_var <- new_var[as.numeric(rownames(self$data))]
      }

      # Correlation of new variable with all existing variables
      cors <- abs(cor(new_var, self$data, use = "pairwise.complete.obs"))

      # Average similarity per cluster
      cluster_similarity <- tapply(
        cors,
        INDEX = self$clusters$cluster,
        FUN = mean
      )

      # Cluster assignment
      predicted_cluster <- unname(which.max(cluster_similarity))

      # Correlation with variables in assigned cluster
      vars_in_cluster <- self$clusters$variable[self$clusters$cluster == predicted_cluster]
      cor_values <- cors[1, vars_in_cluster]

      var_corr <- data.frame(
        variable = vars_in_cluster,
        correlation = round(cor_values, 3)
      )

      result <- list(
        predicted_cluster = predicted_cluster,
        cluster_similarity = round(cluster_similarity, 3),
        var_corr = var_corr
      )

      return(result)
    },

    #-------------
    # illustrative Method
    #-------------

    illustrative = function(illust_vars) {
      if (is.null(self$model)) stop("Model not yet fitted.")
      illust_vars <- as.data.frame(illust_vars)
      cluster_pcs <- private$compute_cluster_pcs_list()

      results_list <- lapply(colnames(illust_vars), function(yname) {
        y <- illust_vars[[yname]]
        df <- data.frame(Y = y)
        for (k in names(cluster_pcs)) df[[paste0("Cluster", k)]] <- cluster_pcs[[k]]
        formula_lm <- as.formula(paste("Y ~", paste(colnames(df)[-1], collapse = " + ")))
        fit <- lm(formula_lm, data = df)
        coef_tbl <- as.data.frame(summary(fit)$coefficients)
        pred_names <- rownames(coef_tbl)[-1]
        R <- sapply(pred_names, function(v) cor(y, df[[v]]))
        R2 <- R^2
        data.frame(
          Dependent = yname,
          Predictor = pred_names,
          R = round(R, 3),
          R2 = round(R2, 3),
          t = round(coef_tbl[-1, "t value"], 3),
          `Pr(>|t|)` = signif(coef_tbl[-1, "Pr(>|t|)"], 3),
          row.names = NULL
        )
      })

      res_table <- do.call(rbind, results_list)

      # -----------------------------
      # PCA correlation circle plot
      # -----------------------------

      pca_plot <- function() {
        active_data <- scale(self$data)
        pca_active <- prcomp(active_data, center = TRUE, scale. = TRUE)

        loadings <- pca_active$rotation[, 1:2]
        arrows_scaling <- 1
        circle <- seq(0, 2*pi, length.out = 100)

        plot(
          cos(circle), sin(circle), type = "l", asp = 1, col = "gray",
          xlab = "PC1", ylab = "PC2", main = "PCA Correlation Circle"
        )
        abline(h = 0, v = 0, lty = 2, col = "black")

        # Active variable arrows in blue
        arrows(0, 0, loadings[,1]*arrows_scaling, loadings[,2]*arrows_scaling, col = "blue", length = 0.1)
        text(loadings[,1]*arrows_scaling*1.1, loadings[,2]*arrows_scaling*1.1, labels = rownames(loadings), col = "blue")

        # Illustrative variables arrows in red
        illust_scaled <- scale(illust_vars)
        cor_illu <- cor(illust_scaled, pca_active$x[,1:2])
        arrows(0, 0, cor_illu[,1]*arrows_scaling, cor_illu[,2]*arrows_scaling, col = "red", length = 0.1)
        text(cor_illu[,1]*arrows_scaling*1.1, cor_illu[,2]*arrows_scaling*1.1, labels = rownames(cor_illu), col = "red")
      }

      return(list(table = res_table, plot = pca_plot))
    },


    # --------------------
    # Print method
    # --------------------
    #' @description Print a concise summary of the VarClus model
    #' @param ... Additional arguments (ignored)
    #' @return Prints a concise summary of the clustering model
    print = function() {
      cat("VarClus model\n")
      cat("Similarity:", self$similarity, "\n")

      if (!is.null(self$model)) {
        cat("Model status       : Fitted\n")
        cat("Number of variables:", length(self$model$hclust$order), "\n")
        cat("Number of clusters:", self$n_clusters, "\n")
      } else {
        cat("Model not yet fitted.\n")
      }
    },



    # --------------------
    # Summary method
    # --------------------
    #' @description Return a detailed summary of the VarClus model results
    #' @return A list with:
    #' \describe{
    #'   \item{text}{Text summary of the clustering model}
    #'   \item{cluster_summary}{Cluster summary (number of variables, eigenvalues, variance explained)}
    #'   \item{R2_summary}{R² summary for each variable}
    summary = function() {
      if (is.null(self$model)) stop("Model not yet fitted.")
      cluster_results <- private$compute_cluster_pcs()
      cluster_details <- private$compute_cluster_R2()

      text_summary <- paste("VarClus Model Summary\n",
                            "Similarity measure:", self$similarity, "\n",
                            "Number of variables:", ncol(self$data), "\n",
                            "Number of clusters:", self$n_clusters)

      list(
        text = text_summary,
        similarity_matrix = self$model$sim,
        cluster_summary = cluster_results,
        R2_summary = cluster_details
      )
    },

    get_clusters_table = function() {
      if (is.null(self$clusters)) {
        stop("Model not yet fitted. Run fit() first.")
      }
      return(self$clusters)
    }
  ),



  private = list(

    # --------------------
    # Compute PC1 list for each cluster
    # --------------------
    compute_cluster_pcs_list = function() {

      clusters <- self$clusters
      cluster_pcs <- list()

      for (k in unique(clusters$cluster)) {
        vars <- clusters$variable[clusters$cluster == k]

        if (length(vars) == 1) {
          cluster_pcs[[as.character(k)]] <- scale(self$data[, vars])
        } else {
          mat <- scale(self$data[, vars, drop = FALSE])
          pca <- prcomp(mat, center = TRUE, scale. = TRUE)
          cluster_pcs[[as.character(k)]] <- pca$x[, 1]
        }
      }

      return(cluster_pcs)
    },



    # --------------------
    # Compute cluster PCA summary
    # --------------------
    compute_cluster_pcs = function() {

      cluster_pcs <- private$compute_cluster_pcs_list()
      cluster_results <- data.frame(
        cluster = integer(),
        n_variables = integer(),
        eigenvalue = numeric(),
        variance_explained = numeric()
      )

      for (k in names(cluster_pcs)) {

        vars_in_cluster <- self$clusters$variable[self$clusters$cluster == as.numeric(k)]
        n_vars <- length(vars_in_cluster)

        if (n_vars == 1) {
          eigenvalue <- 1.0000
          prop_explained <- 1.0000
        } else {
          mat <- scale(self$data[, vars_in_cluster, drop = FALSE])
          pca <- prcomp(mat, center = TRUE, scale. = TRUE)
          eigenvalue <- round(pca$sdev[1]^2, 3)
          prop_explained <- round(pca$sdev[1]^2 / sum(pca$sdev^2), 4)
        }

        cluster_results <- rbind(
          cluster_results,
          data.frame(
            cluster = as.integer(k),
            nbr_Members = n_vars,
            Variation_explained = formatC(eigenvalue, format = "f", digits = 3),
            Proportion_explained = formatC(prop_explained, format = "f", digits = 3)
          )
        )
      }

      cluster_results <- rbind(cluster_results)
      return(cluster_results)
    },



    # --------------------
    # Compute R² summary
    # --------------------
    compute_cluster_R2 = function() {

      clusters <- self$clusters
      clusters$variable <- as.character(clusters$variable)
      cluster_pcs <- private$compute_cluster_pcs_list()

      cluster_details <- data.frame(
        Cluster = integer(),
        Member = character(),
        Own_Cluster = numeric(),
        Next_Cluster = numeric(),
        `1-R2_Ratio` = numeric(),
        stringsAsFactors = FALSE
      )

      for (i in 1:nrow(clusters)) {

        var <- clusters$variable[i]
        clust <- clusters$cluster[i]

        R2_own <- cor(self$data[, var], cluster_pcs[[as.character(clust)]])^2

        other_clusters <- setdiff(names(cluster_pcs), as.character(clust))
        R2_next <- if (length(other_clusters) > 0) {
          max(sapply(other_clusters, function(k) cor(self$data[, var], cluster_pcs[[k]])^2))
        } else 0

        R2_all <- sapply(cluster_pcs, function(pc) cor(self$data[, var], pc)^2)
        R2_ratio <- if (length(R2_all) == 1) 0 else (1 - R2_own) / (1 - R2_next)

        cluster_details <- rbind(
          cluster_details,
          data.frame(
            Cluster = clust,
            Member = var,
            Own_Cluster = formatC(R2_own, format = "f", digits = 3),
            Next_Cluster = formatC(R2_next, format = "f", digits = 3),
            `1_R2_Ratio` = formatC(R2_ratio, format = "f", digits = 3)
          )
        )
      }

      return(cluster_details)
    }

  )
)
