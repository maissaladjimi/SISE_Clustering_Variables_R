#' Variable Clustering using Hierarchical Methods
#'
#' @description
#' Implements hierarchical variable clustering using the VarClus algorithm from Hmisc.
#' Variables are grouped based on their correlation structure, with each cluster
#' represented by its first principal component (PC1).
#'
#' @details
#' The VarClus algorithm works as follows:
#' \enumerate{
#'   \item Calculate similarity matrix between all variables (correlation or Spearman)
#'   \item Perform hierarchical clustering on similarity matrix
#'   \item Cut dendrogram at specified number of clusters (or automatic selection)
#'   \item Each cluster is represented by its first principal component (PC1)
#' }
#'
#' The algorithm iteratively splits clusters to maximize the variance explained by
#' the first principal component of each cluster.
#'
#' @section Similarity Measures:
#' Two similarity measures are available:
#' \itemize{
#'   \item **pearson**: Pearson correlation coefficient (linear relationships)
#'   \item **spearman**: Spearman rank correlation (monotonic relationships)
#' }
#'
#' @section Cluster Quality:
#' Cluster quality is assessed by:
#' \itemize{
#'   \item **Eigenvalue**: First eigenvalue from PCA of cluster variables
#'   \item **Proportion explained**: Variance explained by PC1
#'   \item **R? own cluster**: Squared correlation with own cluster PC1
#'   \item **R? next cluster**: Squared correlation with nearest other cluster PC1
#'   \item **1-R? Ratio**: (1-R?_own)/(1-R?_next) - should be < 1 for good assignment
#' }
#'
#' @section Illustrative Variables:
#' New variables can be projected onto existing clusters using:
#' \itemize{
#'   \item \code{predict()}: Assigns a single variable to the best cluster
#'   \item \code{illustrative()}: Full analysis of multiple illustrative variables with regression and PCA visualization
#' }
#'
#' @examples
#' \dontrun{
#' # Example with numeric data
#' data(mtcars)
#' X_num <- mtcars[, c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec")]
#'
#' # Automatic k selection
#' vc <- VarClus$new(similarity = "pearson")
#' vc$fit(X_num)
#'
#' # Display results
#' vc$print()
#' vc$summary()
#'
#' # Visualizations
#' dend_plot <- vc$get_dendrogram()
#' dend_plot()
#'
#' heatmap_plot <- vc$get_heatmap()
#' heatmap_plot()
#'
#' # Manual k selection
#' vc_k3 <- VarClus$new(similarity = "pearson", n_clusters = 3)
#' vc_k3$fit(X_num)
#'
#' # Get cluster assignments
#' clusters_df <- vc_k3$get_clusters_table()
#'
#' # Predict new variable
#' new_var <- mtcars$vs
#' prediction <- vc_k3$predict(new_var)
#'
#' # Illustrative variables (full analysis)
#' illust_vars <- mtcars[, c("vs", "am", "gear")]
#' illust_results <- vc_k3$illustrative(illust_vars)
#' print(illust_results$table)
#' illust_results$plot()
#' }
#'
#' @references
#' \itemize{
#'   \item Sarle, W. S. (1990). The VARCLUS Procedure. SAS/STAT User's Guide.
#'   \item Harrell, F. E. (2001). Regression Modeling Strategies. Springer.
#' }
#'
#' @field similarity Similarity measure: "pearson" or "spearman"
#' @field n_clusters Number of clusters (NULL for automatic selection)
#' @field model Fitted VarClus model object from Hmisc::varclus()
#' @field clusters Data frame with variable names and cluster assignments
#' @field data Numeric dataset used for clustering
#' @field dendo Internal dendrogram object
#' @field plot_elbow Plot object from elbow method
#'
#' @import R6
#' @import Hmisc
#' @importFrom dendextend color_branches
#' @importFrom plotly plot_ly layout
#' @importFrom stats prcomp cor cutree as.dendrogram lm
#' @importFrom graphics plot abline arrows text points
#' @export
VarClus <- R6::R6Class(
  "VarClus",

  public = list(

    # ==========================================================================
    # PUBLIC FIELDS
    # ==========================================================================

    similarity = NULL,
    n_clusters = NULL,
    model = NULL,
    clusters = NULL,
    data = NULL,
    dendo = NULL,
    plot_elbow = NULL,

    # ==========================================================================
    # CONSTRUCTOR
    # ==========================================================================

    #' @description Create a new VarClus object
    #' @param similarity Similarity measure: "pearson" or "spearman" (default: "pearson")
    #' @param n_clusters Number of clusters (NULL for automatic selection)
    #' @return A new `VarClus` object
    initialize = function(similarity = "pearson", n_clusters = NULL) {
      self$similarity <- similarity
      self$n_clusters <- n_clusters
    },

    # ==========================================================================
    # FIT - Main clustering method
    # ==========================================================================

    #' @description Fit the variable clustering model
    #' @param X_num Numeric data frame or matrix
    #' @return Self (invisibly) for method chaining
    #' @examples
    #' \dontrun{
    #' data(crime)
    #' vc <- VarClus$new()
    #' vc$fit(crime)
    #' }
    fit = function(X_num) {

      self$data <- X_num

      # Validate input
      if (!is.numeric(X_num) && !is.matrix(X_num) && !all(sapply(X_num, is.numeric))) {
        stop("Input X must be fully numeric")
      }

      if (ncol(X_num) < 2) stop("At least two numeric variables are required")

      # Run VarClus
      self$model <- Hmisc::varclus(x = as.matrix(X_num), similarity = self$similarity)

      # Run elbow method
      if (!exists("varclus_elbow")) stop("Function varclus_elbow() not found")
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

    # ==========================================================================
    # GET_DENDROGRAM - Visualization
    # ==========================================================================

    #' @description Generate dendrogram plot of clustered variables
    #' @return Function that generates the dendrogram plot
    #' @examples
    #' \dontrun{
    #' data(crime)
    #' vc <- VarClus$new()
    #' vc$fit(crime)
    #' dend_plot <- vc$get_dendrogram()
    #' dend_plot()
    #' }
    get_dendrogram = function() {

      if (is.null(self$model) || is.null(self$clusters))
        stop("Model not yet fitted or clusters not computed")

      hc <- self$model$hclust
      dend <- as.dendrogram(hc)
      k <- length(unique(self$clusters$cluster))
      cols <- rainbow(k)

      dend <- dendextend::color_branches(dend, k = k, col = cols)

      function() {
        plot(dend, horiz = FALSE, main = NULL, xlab = NULL)
      }
    },

    # ==========================================================================
    # GET_HEATMAP - Correlation heatmap
    # ==========================================================================

    #' @description Generate correlation heatmap of variables
    #' @return Function that generates the interactive heatmap (plotly)
    #' @examples
    #' \dontrun{
    #' data(crime)
    #' vc <- VarClus$new()
    #' vc$fit(crime)
    #' heatmap_plot <- vc$get_heatmap()
    #' heatmap_plot()
    #' }
    get_heatmap = function() {

      if (is.null(self$model)) stop("Model not yet fitted")

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

    # ==========================================================================
    # PREDICT - Assign single variable to cluster
    # ==========================================================================

    #' @description Predict cluster assignment for a single new variable
    #' @param new_var Numeric vector (same length as training data)
    #' @return List containing:
    #' * `predicted_cluster` - Assigned cluster number
    #' * `cluster_similarity` - Average correlation with each cluster
    #' * `var_corr` - Correlations with variables in assigned cluster
    #' @examples
    #' \dontrun{
    #' data(crime)
    #' vc <- VarClus$new()
    #' vc$fit(crime)
    #' prediction <- vc$predict(new_variable)
    #' }
    predict = function(new_var) {

      if (is.null(self$model) || is.null(self$clusters)) {
        stop("Model must be fitted before predicting")
      }

      if (!is.numeric(new_var)) stop("new_var must be numeric")
      if (length(new_var) != nrow(self$data)) {
        stop("new_var must have the same number of rows as the training data")
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

    # ==========================================================================
    # ILLUSTRATIVE - Full analysis of illustrative variables
    # ==========================================================================

    #' @description Project illustrative variables onto clusters with full analysis
    #' @param illust_vars Data frame of numeric illustrative variables
    #' @return List containing:
    #' * `table` - Regression results (R, R?, t-values, p-values)
    #' * `plot` - Function to generate PCA correlation circle
    #' @examples
    #' \dontrun{
    #' data(crime)
    #' vc <- VarClus$new()
    #' vc$fit(crime)
    #' illust_results <- vc$illustrative(illustrative_vars)
    #' print(illust_results$table)
    #' illust_results$plot()
    #' }
    illustrative = function(illust_vars) {
      if (is.null(self$model)) stop("Model not yet fitted")
      illust_vars <- as.data.frame(illust_vars)
      cluster_pcs <- private$compute_cluster_pcs_list()

      # Regression analysis for each illustrative variable
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

      # PCA correlation circle plot
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
        arrows(0, 0, loadings[, 1]*arrows_scaling, loadings[, 2]*arrows_scaling,
               col = "blue", length = 0.1)
        text(loadings[, 1]*arrows_scaling*1.1, loadings[, 2]*arrows_scaling*1.1,
             labels = rownames(loadings), col = "blue")

        # Illustrative variable arrows in red
        illust_scaled <- scale(illust_vars)
        cor_illu <- cor(illust_scaled, pca_active$x[, 1:2])
        arrows(0, 0, cor_illu[, 1]*arrows_scaling, cor_illu[, 2]*arrows_scaling,
               col = "red", length = 0.1)
        text(cor_illu[, 1]*arrows_scaling*1.1, cor_illu[, 2]*arrows_scaling*1.1,
             labels = rownames(cor_illu), col = "red", font = 2)
      }

      return(list(table = res_table, plot = pca_plot))
    },

    # ==========================================================================
    # PRINT - Display summary
    # ==========================================================================

    #' @description Print concise summary of the VarClus model
    #' @return Self (invisibly)
    #' @examples
    #' \dontrun{
    #' data(crime)
    #' vc <- VarClus$new()
    #' vc$fit(crime)
    #' vc$print()
    #' }
    print = function() {
      cat("========================================\n")
      cat("  VARCLUS - VARIABLE CLUSTERING\n")
      cat("========================================\n")
      cat(sprintf("Similarity: %s\n", self$similarity))

      if (!is.null(self$model)) {
        cat(sprintf("\nStatus: Model fitted\n"))
        cat(sprintf("\nData:\n"))
        cat(sprintf("  - Number of variables       : %d\n", length(self$model$hclust$order)))
        cat(sprintf("  - Number of clusters        : %d\n", self$n_clusters))

        if (!is.null(self$clusters)) {
          tbl <- table(self$clusters$cluster)
          cat(sprintf("\nCluster sizes:\n"))
          for (i in 1:length(tbl)) {
            cat(sprintf("  - Cluster %d                 : %d variables\n", i, tbl[i]))
          }
        }
      } else {
        cat("\nStatus: Model not fitted. Use fit() to train.\n")
      }

      cat("========================================\n")
      invisible(self)
    },

    # ==========================================================================
    # SUMMARY - Detailed statistics
    # ==========================================================================

    #' @description Generate detailed clustering statistics
    #' @param print_output Logical, whether to print to console (default: TRUE)
    #' @return List containing:
    #' * `global_stats` - Basic statistics
    #' * `cluster_summary` - Per-cluster PCA statistics
    #' * `cluster_quality` - Average R? per cluster
    #' * `R2_details` - Detailed R? for each variable
    #' * `similarity_matrix` - Similarity matrix between variables
    #' @examples
    #' \dontrun{
    #' data(crime)
    #' vc <- VarClus$new()
    #' vc$fit(crime)
    #' summary_data <- vc$summary(print_output = FALSE)
    #' }
    summary = function(print_output = TRUE) {
      if (is.null(self$model)) stop("Model not yet fitted")

      cluster_pcs <- private$compute_cluster_pcs()
      cluster_r2 <- private$compute_cluster_R2()

      # Global statistics
      global_stats <- data.frame(
        metric = c("Number of variables", "Number of clusters", "Similarity measure"),
        value = c(ncol(self$data), self$n_clusters, self$similarity),
        stringsAsFactors = FALSE
      )

      # Average quality per cluster
      mean_r2_by_cluster <- tapply(
        as.numeric(cluster_r2$Own_Cluster),
        cluster_r2$Cluster,
        mean
      )

      cluster_quality <- data.frame(
        cluster = 1:self$n_clusters,
        mean_R2_own = round(mean_r2_by_cluster, 4),
        stringsAsFactors = FALSE
      )

      if (print_output) {
        self$print()

        cat("\n========================================\n")
        cat("  GLOBAL STATISTICS\n")
        cat("========================================\n")
        print(global_stats, row.names = FALSE)

        cat("\n========================================\n")
        cat("  CLUSTER SUMMARY\n")
        cat("========================================\n")
        cat("Note: Variance explained = eigenvalue / n_variables\n\n")
        print(cluster_pcs, row.names = FALSE)

        cat("\n========================================\n")
        cat("  AVERAGE QUALITY PER CLUSTER\n")
        cat("========================================\n")
        print(cluster_quality, row.names = FALSE)

        cat("\n========================================\n")
        cat("  R? DETAILS BY VARIABLE (top 30)\n")
        cat("========================================\n")
        cat("Note: 1-R? ratio should be < 1 for good assignment\n\n")
        print(head(cluster_r2, 30), row.names = FALSE)
      }

      invisible(list(
        global_stats = global_stats,
        cluster_summary = cluster_pcs,
        cluster_quality = cluster_quality,
        R2_details = cluster_r2,
        similarity_matrix = self$model$sim
      ))
    },

    # ==========================================================================
    # GET_CLUSTERS_TABLE
    # ==========================================================================

    #' @description Get cluster assignments as a data frame
    #' @return Data frame with variables and cluster assignments
    #' @examples
    #' \dontrun{
    #' data(crime)
    #' vc <- VarClus$new()
    #' vc$fit(crime)
    #' clusters_df <- vc$get_clusters_table()
    #' }
    get_clusters_table = function() {
      if (is.null(self$clusters)) {
        stop("Model not yet fitted. Run fit() first.")
      }
      return(self$clusters)
    }
  ),

  # ============================================================================
  # PRIVATE METHODS
  # ============================================================================

  private = list(

    # Compute PC1 for each cluster
    #
    # Calculates the first principal component of variables in each cluster.
    # For single-variable clusters, returns the standardized variable itself.
    #
    # @return List of PC1 vectors (one per cluster)
    compute_cluster_pcs_list = function() {

      clusters <- self$clusters
      cluster_pcs <- list()

      for (k in unique(clusters$cluster)) {
        vars <- clusters$variable[clusters$cluster == k]

        if (length(vars) == 1) {
          # Single variable: use standardized variable as PC1
          cluster_pcs[[as.character(k)]] <- scale(self$data[, vars])
        } else {
          # Multiple variables: compute PC1 via PCA
          mat <- scale(self$data[, vars, drop = FALSE])
          pca <- prcomp(mat, center = TRUE, scale. = TRUE)
          cluster_pcs[[as.character(k)]] <- pca$x[, 1]
        }
      }

      return(cluster_pcs)
    },

    # Compute cluster PCA summary
    #
    # Calculates PCA statistics for each cluster:
    # - Number of variables
    # - First eigenvalue
    # - Proportion of variance explained by PC1
    #
    # @return Data frame with cluster PCA statistics
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

    # Compute R? summary for cluster quality assessment
    #
    # For each variable, calculates:
    # - R? with own cluster PC1
    # - R? with nearest other cluster PC1
    # - 1-R? ratio: (1-R?_own)/(1-R?_next)
    #   (should be < 1 for good assignment)
    #
    # @return Data frame with R? details
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

        # R? with own cluster
        R2_own <- cor(self$data[, var], cluster_pcs[[as.character(clust)]])^2

        # R? with nearest other cluster
        other_clusters <- setdiff(names(cluster_pcs), as.character(clust))
        R2_next <- if (length(other_clusters) > 0) {
          max(sapply(other_clusters, function(k) cor(self$data[, var], cluster_pcs[[k]])^2))
        } else 0

        # Calculate 1-R? ratio (quality metric)
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
