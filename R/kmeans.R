#' K-means Variable Clustering
#'
#' @description
#' Implements a K-means algorithm adapted for clustering variables (not observations).
#' Variables are grouped by maximizing their correlation with cluster centroids,
#' which are computed as the first principal component (PC1) of each cluster.
#'
#' @details
#' The algorithm works as follows:
#' \enumerate{
#'   \item **Initialization**: Randomly assign variables to K clusters
#'   \item **Centroid Calculation**: For each cluster, compute PC1 via PCA as the cluster centroid
#'   \item **Distance Calculation**: Compute R? between each variable and each centroid
#'   \item **Reallocation**: Reassign each variable to the cluster with maximum R?
#'   \item **Convergence Check**: Repeat steps 2-4 until convergence or max iterations reached
#' }
#'
#' @section Distance Metric:
#' The algorithm uses R?-based distance:
#' \deqn{d(X_j, c_k) = \sqrt{1 - R^2(X_j, c_k)}}
#' where \eqn{R^2(X_j, c_k)} is the squared correlation between variable \eqn{X_j}
#' and centroid \eqn{c_k}.
#'
#' @section Objective Function:
#' The algorithm maximizes total inertia, defined as:
#' \deqn{I = \sum_{k=1}^K \lambda_k = \sum_{k=1}^K \sum_{X_j \in C_k} R^2(X_j, c_k)}
#' where \eqn{\lambda_k} is the first eigenvalue from PCA of cluster \eqn{k}.
#'
#' @section Convergence:
#' The algorithm stops when:
#' \itemize{
#'   \item No variable changes cluster between iterations
#'   \item Change in total inertia < \code{tol}
#'   \item Maximum iterations (\code{max_iter}) reached
#' }
#'
#' @section Multiple Initializations:
#' To avoid local maxima, the algorithm runs \code{n_init} times with different
#' random initializations. The solution with highest inertia is retained.
#'
#' @examples
#' \dontrun{
#' # Example with synthetic data
#' set.seed(123)
#' n <- 200
#'
#' # Create 3 groups of correlated variables
#' X1 <- MASS::mvrnorm(n, rep(0, 5), diag(0.7, 5) + 0.3)
#' X2 <- MASS::mvrnorm(n, rep(0, 5), diag(0.7, 5) + 0.3)
#' X3 <- MASS::mvrnorm(n, rep(0, 5), diag(0.7, 5) + 0.3)
#'
#' data_test <- data.frame(X1, X2, X3)
#' colnames(data_test) <- paste0("Var", 1:15)
#'
#' # Fit K-means with 3 clusters
#' km <- KMeansVariablesQuant$new(k = 3, seed = 42)
#' km$fit(data_test)
#'
#' # Display results
#' km$print()
#' km$summary()
#'
#' # Access cluster assignments
#' clusters_df <- km$get_clusters_table()
#'
#' # Visualizations
#' km$plot_correlation_circle()
#' km$plot_biplot()
#' km$plot_elbow(k_range = 2:8)
#'
#' # Predict new variables
#' new_vars <- data.frame(
#'   NewVar1 = rnorm(n),
#'   NewVar2 = rnorm(n)
#' )
#' predictions <- km$predict(new_vars)
#'
#' # Illustrative variables (full analysis)
#' illustrative_vars <- data.frame(
#'   IllustVar1 = rnorm(n),
#'   IllustVar2 = rnorm(n)
#' )
#' illust_results <- km$illustrative(illustrative_vars)
#' }
#'
#' @references
#' \itemize{
#'   \item Vigneau, E., & Qannari, E. M. (2003). Clustering of variables around
#'         latent components. Communications in Statistics-Simulation and Computation,
#'         32(4), 1131-1150.
#'   \item Rakotomalala, R. (2020). Analyse de donnees avec R - Clustering de variables.
#' }
#'
#' @field data Original data.frame provided to fit()
#' @field X_scaled Standardized data matrix (n x p)
#' @field k Number of clusters
#' @field max_iter Maximum number of iterations per run
#' @field n_init Number of random initializations
#' @field tol Convergence tolerance
#' @field seed Random seed for reproducibility
#' @field clusters Integer vector of cluster assignments (length p)
#' @field centers Matrix (n x k) of cluster centroids (PC1 for each cluster)
#' @field r2_matrix Matrix (p x k) of R? between variables and centroids
#' @field inertia_total Total inertia (sum of all cluster inertias)
#' @field inertia_by_cluster Vector of inertia per cluster
#' @field n_iter Number of iterations performed in best run
#' @field scale_center Centering parameters (for predict)
#' @field scale_scale Scaling parameters (for predict)
#'
#' @import R6
#' @export
KMeansVariablesQuant <- R6::R6Class(
  "KMeansVariablesQuant",

  public = list(

    # ==========================================================================
    # PUBLIC FIELDS
    # ==========================================================================

    data = NULL,
    X_scaled = NULL,
    k = NULL,
    max_iter = NULL,
    n_init = NULL,
    tol = NULL,
    seed = NULL,

    # Clustering results
    clusters = NULL,
    centers = NULL,
    r2_matrix = NULL,
    inertia_total = NULL,
    inertia_by_cluster = NULL,
    n_iter = NULL,

    # Standardization parameters (for predict)
    scale_center = NULL,
    scale_scale = NULL,

    # ==========================================================================
    # CONSTRUCTOR
    # ==========================================================================

    #' @description Create a new KMeansVariablesQuant object
    #' @param k Number of clusters (must be >= 2)
    #' @param max_iter Maximum iterations per run (default: 100)
    #' @param n_init Number of random initializations (default: 10)
    #' @param tol Convergence tolerance (default: 1e-4)
    #' @param seed Random seed for reproducibility (default: NULL)
    #' @return A new `KMeansVariablesQuant` object
    initialize = function(k = 3, max_iter = 100, n_init = 10, tol = 1e-4, seed = NULL) {
      if (!is.numeric(k) || k < 2) {
        stop("'k' must be an integer >= 2")
      }
      self$k <- as.integer(k)
      self$max_iter <- as.integer(max_iter)
      self$n_init <- as.integer(n_init)
      self$tol <- tol
      self$seed <- if (!is.null(seed)) as.integer(seed) else NULL
    },

    # ==========================================================================
    # DATA VALIDATION
    # ==========================================================================

    #' @description Validate input data
    #' @param X Data frame or matrix with numeric variables
    #' @param min_vars Minimum number of variables required (default: 2)
    #' @return Validated data.frame
    check_data = function(X, min_vars = 2) {
      if (is.matrix(X)) X <- as.data.frame(X)
      if (!is.data.frame(X)) {
        stop("X must be a data.frame or matrix")
      }
      if (ncol(X) < min_vars) {
        stop(sprintf("At least %d numeric variable(s) required", min_vars))
      }
      if (anyNA(X)) {
        stop("Data must not contain NA values")
      }
      if (!all(sapply(X, is.numeric))) {
        stop("All columns must be numeric")
      }

      # Ensure column names exist
      if (is.null(colnames(X))) {
        colnames(X) <- paste0("V", seq_len(ncol(X)))
      }

      X
    },

    # ==========================================================================
    # FIT - Train the model
    # ==========================================================================

    #' @description Fit the K-means clustering model on variables
    #' @param X Data frame or matrix with numeric variables (rows = observations, columns = variables)
    #' @param k Number of clusters (optional, overrides constructor value)
    #' @return Self (invisibly) for method chaining
    #' @examples
    #' km <- KMeansVariablesQuant$new(k = 3)
    #' km$fit(iris[, 1:4])
    fit = function(X, k = NULL) {
      # Validate data
      X <- self$check_data(X)
      self$data <- X

      # Update k if provided
      if (!is.null(k)) {
        if (!is.numeric(k) || k < 2 || k > ncol(X)) {
          stop("k must be between 2 and the number of variables")
        }
        self$k <- as.integer(k)
      }

      # Check k <= p
      if (self$k > ncol(X)) {
        stop("k cannot exceed the number of variables")
      }

      # Set random seed if provided
      if (!is.null(self$seed)) set.seed(self$seed)

      # Standardize data (center + scale)
      Xs <- scale(X)
      self$X_scaled <- as.matrix(Xs)
      self$scale_center <- attr(Xs, "scaled:center")
      self$scale_scale <- attr(Xs, "scaled:scale")

      n <- nrow(self$X_scaled)
      p <- ncol(self$X_scaled)
      K <- self$k

      # Multiple initializations: test n_init configurations
      best_inertia <- -Inf
      best <- NULL

      message(sprintf("K-means: %d initializations with k=%d clusters...", self$n_init, K))

      for (run in seq_len(self$n_init)) {
        # Different seed for each run
        if (!is.null(self$seed)) {
          set.seed(self$seed + run * 1000)
        }

        # Run a complete K-means iteration
        res_run <- private$single_run(self$X_scaled, K, self$max_iter, self$tol)

        # Keep the best (maximum inertia)
        if (res_run$inertia_total > best_inertia) {
          best_inertia <- res_run$inertia_total
          best <- res_run
        }
      }

      # Store best results
      self$clusters <- best$clusters
      self$centers <- best$centers
      self$r2_matrix <- best$r2_matrix
      self$inertia_total <- best$inertia_total
      self$inertia_by_cluster <- best$inertia_by_cluster
      self$n_iter <- best$n_iter

      message(sprintf("Convergence reached after %d iterations (best run)", self$n_iter))
      message(sprintf("Total inertia: %.4f", self$inertia_total))

      invisible(self)
    },

    # ==========================================================================
    # PREDICT - Assign new variables to clusters
    # ==========================================================================

    #' @description Assign new variables to existing clusters
    #' @param X_new Data frame with new variables (must have same number of observations as training data)
    #' @return Data frame with cluster assignments and R? values
    #' @examples
    #' \dontrun{
    #' km <- KMeansVariablesQuant$new(k = 3)
    #' km$fit(iris[, 1:4])
    #' new_data <- data.frame(NewVar = rnorm(nrow(iris)))
    #' predictions <- km$predict(new_data)
    #' }
    predict = function(X_new) {
      if (is.null(self$clusters) || is.null(self$centers)) {
        stop("fit() must be executed before predict()")
      }

      # Accept any number of variables (even 1)
      X_new <- self$check_data(X_new, min_vars = 1)

      # Check number of OBSERVATIONS (not variables)
      if (nrow(X_new) != nrow(self$data)) {
        stop(sprintf("X_new must have %d observations (currently: %d)",
                     nrow(self$data), nrow(X_new)))
      }

      # Standardize new variables
      Xn <- scale(X_new, center = TRUE, scale = TRUE)
      Xn <- as.matrix(Xn)

      # Calculate R? with each center (latent component)
      cor_mat <- cor(Xn, self$centers)
      cor_mat[is.na(cor_mat)] <- 0
      r2_new <- cor_mat^2

      # Assign to cluster with max R?
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

      # Warning if R? very low
      low_r2 <- result$r2_max < 0.30
      if (any(low_r2)) {
        warning(sprintf(
          "%d variable(s) have R? < 30%%: %s\nThese variables are poorly represented by existing clusters.",
          sum(low_r2),
          paste(result$variable[low_r2], collapse = ", ")
        ))
      }

      result[order(result$cluster, -result$r2_max), ]
    },

    # ==========================================================================
    # ILLUSTRATIVE - Full analysis of illustrative variables
    # ==========================================================================

    #' @description Project illustrative variables onto existing clusters
    #' @param X_illust Data frame with illustrative variables
    #' @param plot Logical, whether to display the barplot (default: TRUE)
    #' @return List containing:
    #' * `table` - Data frame with cluster assignments and R? values
    #' * `r2_all_clusters` - Matrix of R? with all clusters (in %)
    #' * `plot` - Function to generate the barplot
    #' @examples
    #' \dontrun{
    #' km <- KMeansVariablesQuant$new(k = 3)
    #' km$fit(iris[, 1:4])
    #' illustrative_vars <- data.frame(NewVar = rnorm(nrow(iris)))
    #' illust_results <- km$illustrative(illustrative_vars, plot = TRUE)
    #' }
    illustrative = function(X_illust, plot = TRUE) {
      if (is.null(self$clusters) || is.null(self$centers)) {
        stop("fit() must be executed before illustrative()")
      }

      # Validation
      X_illust <- self$check_data(X_illust, min_vars = 1)

      if (nrow(X_illust) != nrow(self$data)) {
        stop(sprintf("X_illust must have %d observations (currently: %d)",
                     nrow(self$data), nrow(X_illust)))
      }

      # Standardize illustrative variables
      Xi <- scale(X_illust, center = TRUE, scale = TRUE)
      Xi <- as.matrix(Xi)

      # Calculate R? with ALL clusters (latent components)
      cor_mat <- cor(Xi, self$centers)
      cor_mat[is.na(cor_mat)] <- 0
      r2_mat <- cor_mat^2

      # For each illustrative variable
      K <- self$k
      var_names <- colnames(X_illust)
      p_illust <- ncol(X_illust)

      # Assigned cluster (max R?)
      cl_assigned <- apply(r2_mat, 1, which.max)
      r2_max <- apply(r2_mat, 1, max)
      dist_assigned <- sqrt(1 - r2_max)

      # Build summary table
      table_result <- data.frame(
        variable = var_names,
        cluster = cl_assigned,
        r2_max = round(r2_max * 100, 2),
        distance = round(dist_assigned, 4),
        stringsAsFactors = FALSE
      )

      # R? matrix with all clusters (in %)
      r2_all <- r2_mat * 100
      rownames(r2_all) <- var_names
      colnames(r2_all) <- paste0("Cluster_", 1:K)

      # Barplot function
      plot_func <- function() {
        # Prepare data for barplot
        r2_for_plot <- r2_mat * 100
        rownames(r2_for_plot) <- var_names

        # Colors for each cluster
        cols <- rainbow(K)

        # Barplot
        par(mar = c(5, 8, 4, 2))
        bp <- barplot(
          t(r2_for_plot),
          beside = FALSE,
          horiz = TRUE,
          col = cols,
          border = NA,
          xlim = c(0, 100),
          xlab = "R? (%)",
          main = "Illustrative Variables - R? with Clusters",
          las = 1,
          cex.names = 0.8
        )

        # Add legend
        legend("topright",
               legend = paste("Cluster", 1:K),
               fill = cols,
               border = NA,
               bty = "n",
               cex = 0.9)

        # Add vertical lines for reference
        abline(v = c(25, 50, 75), lty = 3, col = "gray70")
      }

      # Display plot if requested
      if (plot) {
        plot_func()
      }

      # Return results
      invisible(list(
        table = table_result,
        r2_all_clusters = r2_all,
        plot = plot_func
      ))
    },

    # ==========================================================================
    # SUMMARY - Detailed statistics
    # ==========================================================================

    #' @description Generate detailed clustering statistics
    #' @param print_output Logical, whether to print to console (default: TRUE)
    #' @return List containing:
    #' * `global_quality` - Data frame with global quality metrics
    #' * `cluster_summary` - Data frame with per-cluster statistics
    #' * `cor_latent` - Correlation matrix between latent components
    #' * `r2_matrix` - R? matrix (variables x clusters)
    #' @examples
    #' \dontrun{
    #' km <- KMeansVariablesQuant$new(k = 3)
    #' km$fit(iris[, 1:4])
    #' summary_data <- km$summary(print_output = FALSE)
    #' }
    summary = function(print_output = TRUE) {
      if (is.null(self$clusters)) {
        stop("fit() must be executed before summary()")
      }

      K <- self$k
      p <- ncol(self$X_scaled)

      # === GLOBAL QUALITY ===
      # Average inertia per cluster
      avg_inertia <- mean(self$inertia_by_cluster)

      # Percentage of total inertia explained
      pct_explained <- (self$inertia_total / p) * 100

      global_quality <- data.frame(
        Metric = c("Total Inertia", "Avg Inertia per Cluster", "% Explained (of max possible)"),
        Value = c(
          round(self$inertia_total, 4),
          round(avg_inertia, 4),
          round(pct_explained, 2)
        ),
        stringsAsFactors = FALSE
      )

      # === CLUSTER SUMMARY ===
      cluster_summary <- data.frame(
        Cluster = 1:K,
        Size = as.integer(table(self$clusters)),
        Inertia = round(self$inertia_by_cluster, 4),
        Avg_R2 = round(self$inertia_by_cluster / table(self$clusters), 4),
        Pct_Variance = round((self$inertia_by_cluster / table(self$clusters)) * 100, 2),
        stringsAsFactors = FALSE
      )

      # === CORRELATION BETWEEN LATENT COMPONENTS ===
      cor_latent <- cor(self$centers)
      rownames(cor_latent) <- paste0("Cluster_", 1:K)
      colnames(cor_latent) <- paste0("Cluster_", 1:K)

      # === R? MATRIX ===
      r2_matrix <- self$r2_matrix

      # Print if requested
      if (print_output) {
        cat("\n")
        cat("========================================\n")
        cat("  CLUSTERING SUMMARY\n")
        cat("========================================\n\n")

        cat("Global Quality Metrics:\n")
        print(global_quality, row.names = FALSE)

        cat("\n")
        cat("Per-Cluster Statistics:\n")
        print(cluster_summary, row.names = FALSE)

        cat("\n")
        cat("Correlation between Latent Components:\n")
        print(round(cor_latent, 3))

        cat("\n")
        cat("R? Matrix (Variables x Clusters) - First 10 rows:\n")
        print(round(head(r2_matrix, 10), 4))

        cat("\n")
        cat("========================================\n")
      }

      # Return structured data
      invisible(list(
        global_quality = global_quality,
        cluster_summary = cluster_summary,
        cor_latent = cor_latent,
        r2_matrix = r2_matrix
      ))
    },

    # ==========================================================================
    # PLOT_ELBOW - Elbow method for optimal k
    # ==========================================================================

    #' @description Determine optimal number of clusters using elbow method
    #' @param k_range Range of k values to test (default: 2:10)
    #' @param plot Logical, whether to display the plot (default: TRUE)
    #' @return List containing:
    #' * `optimal_k` - Recommended number of clusters
    #' * `results` - Data frame with inertia for each k
    #' * `plot` - Function to generate the elbow plot
    #' @examples
    #' \dontrun{
    #' km <- KMeansVariablesQuant$new(k = 3)
    #' km$fit(iris[, 1:4])
    #' elbow_results <- km$plot_elbow(k_range = 2:8)
    #' }
    plot_elbow = function(k_range = 2:10, plot = TRUE) {
      if (is.null(self$data)) {
        stop("No data available. Run fit() first or provide data.")
      }

      X <- self$X_scaled
      if (is.null(X)) {
        X <- scale(self$data)
      }

      n <- nrow(X)
      p <- ncol(X)

      # Limit k_range to valid values
      k_range <- k_range[k_range >= 2 & k_range <= p]
      if (length(k_range) == 0) {
        stop("k_range must contain values between 2 and the number of variables")
      }

      # Test each k value
      results <- data.frame(
        k = integer(),
        inertia = numeric(),
        avg_inertia = numeric(),
        stringsAsFactors = FALSE
      )

      message(sprintf("Testing k from %d to %d...", min(k_range), max(k_range)))

      for (k_test in k_range) {
        # Temporary K-means model
        km_temp <- KMeansVariablesQuant$new(
          k = k_test,
          max_iter = self$max_iter,
          n_init = self$n_init,
          tol = self$tol,
          seed = self$seed
        )
        km_temp$fit(as.data.frame(X), k = k_test)

        results <- rbind(results, data.frame(
          k = k_test,
          inertia = km_temp$inertia_total,
          avg_inertia = km_temp$inertia_total / k_test
        ))
      }

      # Detect elbow using perpendicular distance method
      optimal_k <- private$detect_elbow(results$k, results$inertia)

      # Plot function
      plot_func <- function() {
        par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))

        plot(results$k, results$inertia,
             type = "b", pch = 19, col = "steelblue",
             xlab = "Number of clusters (k)",
             ylab = "Total Inertia",
             main = "Elbow Method - Optimal k Selection")

        # Highlight optimal k
        points(optimal_k, results$inertia[results$k == optimal_k],
               col = "red", pch = 19, cex = 2)
        text(optimal_k, results$inertia[results$k == optimal_k],
             labels = paste("k =", optimal_k),
             pos = 3, col = "red", font = 2)

        grid()
      }

      # Display plot if requested
      if (plot) {
        plot_func()
      }

      # Print summary
      cat("\n=== K-Means Elbow Analysis ===\n")
      cat(sprintf("Optimal k: %d\n", optimal_k))
      cat(sprintf("Range tested: %d to %d\n", min(k_range), max(k_range)))
      cat("\nResults table:\n")
      print(results)

      # Return results
      invisible(list(
        optimal_k = optimal_k,
        results = results,
        plot = plot_func
      ))
    },

    # ==========================================================================
    # PLOT_BIPLOT - Variables on factorial axes
    # ==========================================================================

    #' @description Plot variables in factorial space (PCA biplot)
    #' @param dims Dimensions to plot (default: c(1, 2))
    #' @return Coordinates matrix (invisibly)
    #' @examples
    #' \dontrun{
    #' km <- KMeansVariablesQuant$new(k = 3)
    #' km$fit(iris[, 1:4])
    #' km$plot_biplot(dims = c(1, 2))
    #' }
    plot_biplot = function(dims = c(1, 2)) {
      if (is.null(self$clusters)) {
        stop("fit() must be executed before plot_biplot()")
      }
      if (self$k < max(dims)) {
        stop(sprintf("dims[%d]=%d but k=%d", which.max(dims), max(dims), self$k))
      }

      # PCA of variables (transposed X)
      X_t <- t(self$X_scaled)
      pca <- prcomp(X_t, center = TRUE, scale. = FALSE)

      # Variable coordinates on factorial axes
      coords <- pca$x[, dims]

      # % variance explained
      var_expl <- summary(pca)$importance[2, dims] * 100

      plot(coords[, 1], coords[, 2],
           type = "n",
           xlab = sprintf("PC%d (%.1f%%)", dims[1], var_expl[1]),
           ylab = sprintf("PC%d (%.1f%%)", dims[2], var_expl[2]),
           main = "Biplot - Variables in Factorial Space")

      abline(h = 0, v = 0, lty = 3, col = "grey70")

      # Points colored by cluster
      cols <- rainbow(self$k)[self$clusters]
      points(coords[, 1], coords[, 2], pch = 19, col = cols, cex = 1.2)
      text(coords[, 1], coords[, 2],
           labels = colnames(self$X_scaled),
           pos = 3, cex = 0.7, col = cols)

      # Legend
      legend("topright", legend = paste("Cluster", 1:self$k),
             col = rainbow(self$k), pch = 19, cex = 0.8)

      invisible(coords)
    },

    # ==========================================================================
    # PLOT_CORRELATION_CIRCLE - Correlation circle
    # ==========================================================================

    #' @description Plot correlation circle of variables with latent components
    #' @param dims Dimensions to plot (default: c(1, 2))
    #' @return Correlation matrix (invisibly)
    #' @examples
    #' \dontrun{
    #' km <- KMeansVariablesQuant$new(k = 3)
    #' km$fit(iris[, 1:4])
    #' km$plot_correlation_circle(dims = c(1, 2))
    #' }
    plot_correlation_circle = function(dims = c(1, 2)) {
      if (is.null(self$clusters)) {
        stop("fit() must be executed before plot_correlation_circle()")
      }
      if (self$k < max(dims)) {
        stop(sprintf("dims[%d]=%d but k=%d", which.max(dims), max(dims), self$k))
      }

      # Correlations between variables and latent components
      cor_mat <- cor(self$X_scaled, self$centers[, dims])

      # Circle
      theta <- seq(0, 2*pi, length.out = 100)
      circle_x <- cos(theta)
      circle_y <- sin(theta)

      # Calculate % variance explained by each component (approximation)
      r2_dim1 <- mean(cor_mat[, 1]^2) * 100
      r2_dim2 <- mean(cor_mat[, 2]^2) * 100

      plot(circle_x, circle_y, type = "l", col = "grey70",
           xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
           asp = 1,
           xlab = sprintf("Latent Component %d (R?~%.1f%%)", dims[1], r2_dim1),
           ylab = sprintf("Latent Component %d (R?~%.1f%%)", dims[2], r2_dim2),
           main = "Correlation Circle - Active Variables")

      abline(h = 0, v = 0, lty = 3)

      # Variable arrows
      arrows(0, 0, cor_mat[, 1], cor_mat[, 2],
             length = 0.1, col = "steelblue", lwd = 1.5)

      # Variable text (colored by cluster)
      cols <- rainbow(self$k)[self$clusters]
      text(cor_mat[, 1], cor_mat[, 2],
           labels = colnames(self$X_scaled),
           pos = 3, cex = 0.7, col = cols)

      invisible(cor_mat)
    },

    # ==========================================================================
    # PRINT - Display summary
    # ==========================================================================

    #' @description Print model summary
    #' @param ... Additional arguments (ignored)
    #' @return Self (invisibly)
    #' @examples
    #' \dontrun{
    #' km <- KMeansVariablesQuant$new(k = 3)
    #' km$fit(iris[, 1:4])
    #' km$print()
    #' }
    print = function(...) {
      cat("========================================\n")
      cat("  K-MEANS VARIABLE CLUSTERING\n")
      cat("========================================\n")
      cat(sprintf("Parameters:\n"))
      cat(sprintf("  - Number of clusters (k)    : %d\n", self$k))
      cat(sprintf("  - Max iterations per run    : %d\n", self$max_iter))
      cat(sprintf("  - Number of initializations : %d\n", self$n_init))
      cat(sprintf("  - Convergence tolerance     : %.1e\n", self$tol))

      if (!is.null(self$seed)) {
        cat(sprintf("  - Random seed               : %d\n", self$seed))
      }

      if (!is.null(self$data)) {
        cat(sprintf("\nData:\n"))
        cat(sprintf("  - Observations              : %d\n", nrow(self$data)))
        cat(sprintf("  - Variables                 : %d\n", ncol(self$data)))
      }

      if (!is.null(self$clusters)) {
        cat(sprintf("\nResults:\n"))
        cat(sprintf("  - Total inertia             : %.4f\n", self$inertia_total))
        cat(sprintf("  - Iterations (best run)     : %d\n", self$n_iter))
        cat(sprintf("\nCluster sizes:\n"))
        tbl <- table(self$clusters)
        for (i in 1:length(tbl)) {
          cat(sprintf("  - Cluster %d                 : %d variables\n", i, tbl[i]))
        }
      } else {
        cat("\nStatus: Model not fitted. Use fit() to train.\n")
      }

      cat("========================================\n")

      invisible(self)
    },

    # ==========================================================================
    # GET_CLUSTERS_TABLE - Extract cluster assignments
    # ==========================================================================

    #' @description Get cluster assignments as a data frame
    #' @return Data frame with variables and their cluster assignments
    #' @examples
    #' \dontrun{
    #' km <- KMeansVariablesQuant$new(k = 3)
    #' km$fit(iris[, 1:4])
    #' clusters_df <- km$get_clusters_table()
    #' }
    get_clusters_table = function() {
      if (is.null(self$clusters))
        stop("fit() must be executed before get_clusters_table()")

      var_names <- colnames(self$data)

      df <- data.frame(
        variable = var_names,
        cluster = self$clusters,
        stringsAsFactors = FALSE
      )

      df[order(df$cluster, df$variable), ]
    }
  ),

  # ============================================================================
  # PRIVATE METHODS
  # ============================================================================

  private = list(

    # Calculate latent center (PC1) of a cluster
    #
    # Computes the first principal component of variables in a cluster.
    # This component serves as the cluster centroid.
    #
    # @param X_cluster Matrix of observations x variables in the cluster
    # @return List with centroid (PC1 scores)
    latent_center = function(X_cluster) {
      n_vars <- ncol(X_cluster)
      n_obs <- nrow(X_cluster)

      # CASE 1: Empty cluster
      if (n_vars == 0) {
        warning("Empty cluster detected - returning random center")
        fake <- rnorm(n_obs)
        fake <- scale(fake, center = TRUE, scale = TRUE)
        return(list(center = as.numeric(fake)))
      }

      # CASE 2: Single variable
      if (n_vars == 1) {
        v <- X_cluster[, 1]
        if (sd(v) < .Machine$double.eps) {
          warning("Variable with zero variance detected")
          fake <- rnorm(n_obs)
          fake <- scale(fake, center = TRUE, scale = TRUE)
          return(list(center = as.numeric(fake)))
        }
        center <- as.numeric(scale(v, center = TRUE, scale = TRUE))
        return(list(center = center))
      }

      # CASE 3: Multiple variables -> PCA
      pca <- prcomp(X_cluster, center = TRUE, scale. = FALSE, rank. = 1)

      # Extract PC1 (factorial scores)
      center <- pca$x[, 1]

      # Normalize PC1 (mean 0, sd 1)
      center <- as.numeric(scale(center, center = TRUE, scale = TRUE))

      # CASE 4: Degenerate PC1
      if (anyNA(center) || sd(center) < .Machine$double.eps) {
        warning("PC1 with zero variance detected")
        fake <- rnorm(n_obs)
        fake <- scale(fake, center = TRUE, scale = TRUE)
        return(list(center = as.numeric(fake)))
      }

      list(center = center)
    },

    # Complete K-means run
    #
    # Performs one complete K-means iteration with random initialization
    #
    # @param X Standardized data matrix (n x p)
    # @param K Number of clusters
    # @param max_iter Maximum iterations
    # @param tol Convergence tolerance
    # @return List with clustering results
    single_run = function(X, K, max_iter, tol) {
      n <- nrow(X)
      p <- ncol(X)

      # Random initialization
      clusters <- sample.int(K, p, replace = TRUE)

      # Ensure no empty clusters
      for (k in 1:K) {
        if (sum(clusters == k) == 0) {
          clusters[sample.int(p, 1)] <- k
        }
      }

      # Storage structures
      inertia_old <- -Inf
      centers <- matrix(0, nrow = n, ncol = K)
      r2_mat <- matrix(0, nrow = p, ncol = K)

      # Main loop
      for (iter in seq_len(max_iter)) {

        # STEP 1: Recalculate centers (PC1) of each cluster
        for (k in 1:K) {
          idx <- which(clusters == k)
          lc <- private$latent_center(X[, idx, drop = FALSE])
          centers[, k] <- lc$center
        }

        # STEP 2: Calculate R? between each variable and each center
        cor_mat <- cor(X, centers)
        cor_mat[is.na(cor_mat)] <- 0
        r2_mat <- cor_mat^2

        # STEP 3: Reassign each variable to cluster with max R?
        clusters_new <- apply(r2_mat, 1, which.max)

        # STEP 4: Handle empty clusters
        for (k in 1:K) {
          if (sum(clusters_new == k) == 0) {
            farthest <- which.min(apply(r2_mat, 1, max))
            clusters_new[farthest] <- k
          }
        }

        # Calculate inertia as SUM of R?
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

      # Final clean recalculation
      for (k in 1:K) {
        idx <- which(clusters == k)
        lc <- private$latent_center(X[, idx, drop = FALSE])
        centers[, k] <- lc$center
      }

      cor_mat <- cor(X, centers)
      cor_mat[is.na(cor_mat)] <- 0
      r2_mat <- cor_mat^2

      # Final lambda = sum of R? in each cluster
      lambdas <- numeric(K)
      for (k in 1:K) {
        idx <- which(clusters == k)
        if (length(idx) > 0) {
          lambdas[k] <- sum(r2_mat[idx, k])
        }
      }

      # Names
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
    },

    # Detect elbow using perpendicular distance method
    #
    # Finds the "elbow" point in a curve by maximizing perpendicular distance
    # to the line connecting first and last points
    #
    # @param k_vals Vector of k values
    # @param y_vals Vector of corresponding metric values (e.g., inertia)
    # @return Optimal k value
    detect_elbow = function(k_vals, y_vals) {
      line_start <- c(k_vals[1], y_vals[1])
      line_end <- c(k_vals[length(k_vals)], y_vals[length(y_vals)])

      # Distance from point to line
      distance_from_line <- function(point, start, end) {
        x0 <- point[1]; y0 <- point[2]
        x1 <- start[1]; y1 <- start[2]
        x2 <- end[1]; y2 <- end[2]
        abs((y2-y1)*x0 - (x2-x1)*y0 + x2*y1 - y2*x1) / sqrt((y2-y1)^2 + (x2-x1)^2)
      }

      # Calculate distances for all points
      distances <- sapply(seq_along(k_vals), function(i) {
        distance_from_line(c(k_vals[i], y_vals[i]), line_start, line_end)
      })

      k_vals[which.max(distances)]
    }
  )
)
