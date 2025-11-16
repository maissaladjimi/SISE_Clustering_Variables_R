library(R6)
library(Hmisc)
library(dendextend)

VarClus <- R6::R6Class(
  "VarClus",

  public = list(
    similarity = NULL,
    n_clusters = NULL,
    model = NULL,
    clusters = NULL,
    data = NULL,
    dendo = NULL,
    plot_elbow = NULL,

    #-------------
    # Constructor
    #-------------
    initialize = function(similarity = "pearson", n_clusters = NULL) {
      self$similarity <- similarity
      self$n_clusters <- n_clusters
    },

    #-------------
    # Fit Method
    #-------------
    fit = function(X_num) {
      self$data <- X_num

      # Expect X to already be numeric
      if (!is.numeric(X_num) && !is.matrix(X_num) && !all(sapply(X_num, is.numeric))) {
        stop("Input X must be fully numeric")
      }

      if (ncol(X_num) < 2) stop("At least two numeric variables are required.")

      self$model <- Hmisc::varclus(x = X_num, similarity = self$similarity)

      # Compute elbow plot always
      if (!exists("varclus_elbow")) stop("Function varclus_elbow() not found.")
      res <- varclus_elbow(X_num)
      self$plot_elbow <- res$plot

      # Set n_clusters automatically only if not specified
      if (is.null(self$n_clusters)) {
        self$n_clusters <- res$optimal_k
      }

      # Cut dendrogram into a fixed number of clusters
      clust <- cutree(self$model$hclust, k = self$n_clusters)
      self$clusters <- data.frame(variable = names(clust), cluster = clust, stringsAsFactors = FALSE)

      invisible(self)
    },

    #-------------
    # Plots Methods
    #-------------

    # Get Dendrogram
    get_dendrogram = function() {
      if (is.null(self$model) || is.null(self$clusters)) stop("Model not yet fitted or clusters not computed.")

      hc <- self$model$hclust
      dend <- as.dendrogram(hc)
      k <- length(unique(self$clusters$cluster))
      cols <- rainbow(k)

      dend <- color_branches(dend, k = k, col = cols)

      # Return a function that plots it when called
      function() {
        plot(dend, horiz = FALSE, main = "Dendrogram of Variables", xlab = "Height")
      }
    },

    # get heatmap
    get_heatmap = function() {
      if (is.null(self$model)) stop("Model not yet fitted.")

      cor_mat <- self$model$sim  # correlation matrix from varclus

      function() {
        heatmap(cor_mat,
                Rowv = NA,  # no reordering
                Colv = NA,
                col = colorRampPalette(c("blue","white","red"))(50),
                scale = "none",
                main = "Variable Correlation Heatmap")
      }
    },

    #-------------
    # Print Method
    #-------------
    print = function(...) {
      cat("VarClus model\n")
      cat("Similarity:", self$similarity, "\n")
      if (!is.null(self$model)) {
        cat("Number of variables clustered:", length(self$model$hclust$order), "\n")
        cat("Number of clusters:", self$n_clusters, "\n")
      } else {
        cat("Model not yet fitted.\n")
      }
    },

    #-------------
    # Summary Method
    #-------------
    summary = function() {
      if (is.null(self$model)) stop("Model not yet fitted.")
      cluster_results <- private$compute_cluster_pcs()
      cluster_details <- private$compute_cluster_R2()

      text_summary <- paste("VarClusR Summary\n",
                            "Similarity measure:", self$similarity, "\n",
                            "Number of clusters:", self$n_clusters)

      list(
        text = text_summary,
        cluster_summary = cluster_results,
        R2_summary = cluster_details
      )
    }
  ),

  private = list(

    #-------------
    # Compute PC for each cluster
    #-------------
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

    #-------------
    # Compute cluster PCA summary
    #-------------
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
        pc1 <- cluster_pcs[[k]]

        if (n_vars == 1) {
          eigenvalue <- 1
          variance_explained <- 100
        } else {
          # Variance explained by first PC
          eigenvalue <- var(pc1) * (nrow(self$data) - 1)
          variance_explained <- (eigenvalue / n_vars) * 100
        }

        cluster_results <- rbind(cluster_results,
                                 data.frame(cluster = as.numeric(k),
                                            n_variables = n_vars,
                                            eigenvalue = eigenvalue,
                                            variance_explained = variance_explained))
      }

      return(cluster_results)
    },

    #-------------
    # Compute R² summary
    #-------------
    compute_cluster_R2 = function() {
      clusters <- self$clusters
      clusters$variable <- as.character(clusters$variable)

      # Use shared PC1 list
      cluster_pcs <- private$compute_cluster_pcs_list()

      cluster_details <- data.frame(
        cluster = integer(),
        variable = character(),
        R2_own_pct = numeric(),
        R2_next_pct = numeric(),
        unexplained_pct = numeric(),
        stringsAsFactors = FALSE
      )

      for (i in 1:nrow(clusters)) {
        var <- clusters$variable[i]
        clust <- clusters$cluster[i]

        # R² with own cluster
        R2_own <- cor(self$data[, var], cluster_pcs[[as.character(clust)]])^2

        # R² with other clusters
        other_clusters <- setdiff(names(cluster_pcs), as.character(clust))
        R2_next <- if(length(other_clusters) > 0) {
          max(sapply(other_clusters, function(k) cor(self$data[, var], cluster_pcs[[k]])^2))
        } else {
          0
        }

        cluster_details <- rbind(cluster_details,
                                 data.frame(
                                   cluster = clust,
                                   variable = var,
                                   R2_own_pct = R2_own * 100,
                                   R2_next_pct = R2_next * 100,
                                   unexplained_pct = (1 - R2_own) * 100
                                 ))
      }

      return(cluster_details)
    }
  )

)
