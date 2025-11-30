# ==============================================================================
# ELBOW METHODS FOR CLUSTERING
#
# This file contains elbow methods for automatic k selection in:
# - VarClus (hierarchical clustering of variables)
# - K-Means Variables (partitional clustering of variables)
# - ACM-CAH (clustering of qualitative modalities)
#
# All methods use perpendicular distance to detect the "elbow" point.
# ==============================================================================

#' @importFrom graphics par grid plot lines points abline legend
#' @importFrom stats na.omit
NULL

# ==============================================================================
# HELPER FUNCTION: Detect elbow using perpendicular distance method
# ==============================================================================

#' Detect Elbow Point in Curve
#'
#' @description
#' Finds the "elbow" point in a curve by maximizing the perpendicular distance
#' from each point to the line connecting the first and last points.
#'
#' @param k_vals Numeric vector of k values (x-axis)
#' @param y_vals Numeric vector of metric values (y-axis, e.g., inertia)
#'
#' @return Optimal k value (scalar)
#'
#' @details
#' The method computes the perpendicular distance from each point (k_i, y_i)
#' to the straight line connecting (k_1, y_1) and (k_max, y_max).
#' The k value with maximum perpendicular distance is selected as the elbow.
#'
#' @references
#' Satopaa, V., Albrecht, J., Irwin, D., & Raghavan, B. (2011).
#' Finding a "Kneedle" in a Haystack: Detecting Knee Points in System Behavior.
#'
#' @keywords internal
detect_elbow <- function(k_vals, y_vals) {
  line_start <- c(k_vals[1], y_vals[1])
  line_end   <- c(k_vals[length(k_vals)], y_vals[length(y_vals)])

  # Calculate perpendicular distance from point to line
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

  # Return k with maximum distance
  k_vals[which.max(distances)]
}

# ==============================================================================
# VARCLUS ELBOW
# ==============================================================================

#' Determine Optimal Number of Clusters for VarClus
#'
#' @description
#' Automatically selects the optimal number of clusters for hierarchical
#' variable clustering using the elbow method. The algorithm tests all
#' possible k values from 2 to (p-1) and maximizes average intra-cluster
#' similarity (squared correlation).
#'
#' @param X_num Numeric data matrix or data.frame (observations x variables)
#' @param similarity Similarity measure: "pearson" or "spearman" (default: "spearman")
#'
#' @return List containing:
#'   \item{optimal_k}{Recommended number of clusters}
#'   \item{plot}{Function to generate the elbow plot}
#'   \item{k_vals}{Vector of tested k values}
#'   \item{inertia}{Vector of average intra-cluster similarities}
#'
#' @details
#' For each k, the algorithm:
#' \enumerate{
#'   \item Cuts the VarClus dendrogram at k clusters
#'   \item Calculates average squared correlation within each cluster
#'   \item Computes mean of these averages across all clusters
#' }
#'
#' The optimal k maximizes the perpendicular distance to the line connecting
#' the first and last points of the inertia curve.
#'
#' @examples
#' \dontrun{
#' # Automatic k selection for VarClus
#' data <- matrix(rnorm(100 * 10), ncol = 10)
#' result <- varclus_elbow(data, similarity = "pearson")
#' print(result$optimal_k)
#' result$plot()
#' }
#'
#' @export
varclus_elbow <- function(X_num, similarity = "spearman") {

  # Validate input
  if (!is.matrix(X_num) && !all(sapply(X_num, is.numeric))) {
    stop("Input to varclus_elbow() must be numeric")
  }

  if (ncol(X_num) < 2) stop("Need at least 2 numeric variables")

  # Run VarClus
  vc <- Hmisc::varclus(x = X_num, similarity = similarity)
  hc <- vc$hclust
  max_k <- ncol(X_num) - 1

  # Compute average intra-cluster similarity for each k
  avg_inertia <- numeric(max_k)
  for (k in 2:max_k) {
    clust <- cutree(hc, k = k)
    cluster_sim <- numeric(k)

    for (i in 1:k) {
      vars <- names(clust[clust == i])
      if (length(vars) > 1) {
        # Average squared correlation within cluster
        mat <- cor(X_num[, vars])^2
        cluster_sim[i] <- mean(mat[upper.tri(mat)])
      } else {
        # Single variable: perfect similarity
        cluster_sim[i] <- 1
      }
    }
    # Mean across all clusters
    avg_inertia[k] <- mean(cluster_sim)
  }

  # Detect elbow
  k_vals <- 2:max_k
  y <- avg_inertia[2:max_k]
  optimal_k <- detect_elbow(k_vals, y)

  # Plot function
  plot_elbow <- function() {
    plot(k_vals, y, type = "b", pch = 19, col = "blue",
         xlab = "Number of clusters", ylab = "Average intra-cluster similarity",
         main = "VarClus Elbow Method")
    points(optimal_k, y[optimal_k - 1], col = "red", pch = 19, cex = 1.5)
    text(optimal_k, y[optimal_k - 1], labels = paste("K =", optimal_k),
         pos = 3, col = "red")
    grid()
  }

  return(list(
    optimal_k = optimal_k,
    plot = plot_elbow,
    k_vals = k_vals,
    inertia = y
  ))
}

# ==============================================================================
# K-MEANS ELBOW (Variables)
# ==============================================================================

#' Determine Optimal Number of Clusters for K-Means Variables
#'
#' @description
#' Automatically selects the optimal number of clusters for K-means variable
#' clustering using the elbow method. Tests a range of k values and maximizes
#' total inertia (sum of squared correlations with cluster centroids).
#'
#' @param X_num Numeric data matrix or data.frame (observations x variables)
#' @param k_range Range of k values to test (default: 2:10)
#' @param n_init Number of random initializations per k (default: 20)
#' @param seed Random seed for reproducibility (default: NULL)
#'
#' @return List containing:
#'   \item{optimal_k}{Recommended number of clusters}
#'   \item{results}{Data frame with k, inertia, and marginal gain}
#'   \item{plot}{Function to generate dual elbow plots (inertia + gain)}
#'
#' @details
#' For each k in k_range, the algorithm:
#' \enumerate{
#'   \item Runs K-means variable clustering with n_init initializations
#'   \item Keeps the solution with maximum total inertia
#'   \item Calculates marginal gain: D inertia = inertia(k) - inertia(k-1)
#' }
#'
#' The optimal k maximizes perpendicular distance in the inertia curve
#' (excluding k=2 to avoid edge effects).
#'
#' @examples
#' \dontrun{
#' # Automatic k selection for K-means variables
#' data <- data.frame(matrix(rnorm(100 * 10), ncol = 10))
#' result <- kmeans_elbow(data, k_range = 2:8, seed = 123)
#' print(result$optimal_k)
#' result$plot()
#' print(result$results)
#' }
#'
#' @export
kmeans_elbow <- function(X_num, k_range = 2:10, n_init = 20, seed = NULL) {

  # Validate input
  if (!is.matrix(X_num) && !is.data.frame(X_num)) {
    stop("X_num must be a numeric matrix or data.frame")
  }

  if (!all(sapply(X_num, is.numeric))) {
    stop("All columns must be numeric")
  }

  if (ncol(X_num) < 2) {
    stop("Need at least 2 variables")
  }

  # Set seed if provided
  if (!is.null(seed)) set.seed(seed)

  # Initialize storage
  results <- data.frame(
    k = integer(),
    inertia = numeric(),
    gain = numeric()
  )

  # Run K-means for each k
  for (k in k_range) {
    km <- KMeansVariablesQuant$new(k = k, n_init = n_init, seed = seed)
    km$fit(X_num)

    inertia <- km$inertia_total
    gain <- if (nrow(results) == 0) NA else inertia - results$inertia[nrow(results)]

    results <- rbind(results, data.frame(k = k, inertia = inertia, gain = gain))
  }

  # Detect optimal k (skip k=2 for elbow detection to avoid edge effects)
  k_vals <- results$k[results$k > 2]
  y_vals <- results$inertia[results$k > 2]

  if (length(k_vals) > 1) {
    optimal_k <- detect_elbow(k_vals, y_vals)
  } else {
    optimal_k <- k_range[1]
    warning("Not enough k values to detect elbow. Consider increasing k_range.")
  }

  # Plot function
  plot_elbow <- function() {
    par(mfrow = c(1, 2))

    # Plot 1: Inertia curve
    plot(results$k, results$inertia, type = "b", pch = 19, col = "blue",
         xlab = "Number of clusters (k)",
         ylab = "Total inertia (sum of eigenvalues)",
         main = "K-Means Variables: Elbow Method")
    abline(v = optimal_k, col = "red", lty = 2)
    points(optimal_k, results$inertia[results$k == optimal_k],
           col = "red", pch = 19, cex = 2)
    text(optimal_k, results$inertia[results$k == optimal_k],
         labels = paste("k* =", optimal_k), pos = 4, col = "red", font = 2)
    grid()

    # Plot 2: Marginal gain
    gain_data <- results[!is.na(results$gain), ]
    plot(gain_data$k, gain_data$gain, type = "b", pch = 19, col = "darkgreen",
         xlab = "Number of clusters (k)",
         ylab = "Marginal gain in inertia",
         main = "Marginal Gain (D inertia)")
    abline(h = 0, col = "gray", lty = 3)
    abline(v = optimal_k, col = "red", lty = 2)
    grid()

    par(mfrow = c(1, 1))
  }

  return(list(
    optimal_k = optimal_k,
    results = results,
    plot = plot_elbow
  ))
}

# ==============================================================================
# ACM-CAH ELBOW (Modalities)
# ==============================================================================

#' Determine Optimal Number of Clusters for ACM-CAH
#'
#' @description
#' Automatically selects the optimal number of clusters for categorical
#' variable clustering (ACM-CAH or DICE method) using the elbow method
#' based on dendrogram heights.
#'
#' @param X_quali Data frame with categorical variables (factors)
#' @param method Clustering method: "dice" or "acm" (default: "acm")
#' @param k_max Maximum number of clusters to test (default: 10)
#'
#' @return List containing:
#'   \item{optimal_k}{Recommended number of clusters}
#'   \item{results}{Data frame with k, height, and marginal gain}
#'   \item{suggested_k}{Top 3 k values based on marginal gain}
#'   \item{plot}{Function to generate dual elbow plots (height + gain)}
#'
#' @details
#' For hierarchical clustering, the optimal k corresponds to the largest
#' "jump" in the dendrogram, i.e., the maximum marginal gain:
#' \deqn{gain(k) = height(k-1) - height(k)}
#'
#' The algorithm:
#' \enumerate{
#'   \item Fits ACM-CAH or DICE hierarchical clustering
#'   \item Extracts dendrogram heights for k = 1 to k_max
#'   \item Calculates marginal gain (reduction in height) for each k
#'   \item Selects k with maximum gain as optimal
#' }
#'
#' @examples
#' \dontrun{
#' # Automatic k selection for ACM-CAH
#' data <- data.frame(
#'   var1 = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
#'   var2 = factor(sample(c("X", "Y", "Z"), 100, replace = TRUE))
#' )
#' result <- acm_cah_elbow(data, method = "acm", k_max = 10)
#' print(result$optimal_k)
#' print(result$suggested_k)
#' result$plot()
#' }
#'
#' @export
acm_cah_elbow <- function(X_quali, method = "acm", k_max = 10) {

  # Validate input
  if (!is.data.frame(X_quali)) {
    stop("X_quali must be a data.frame with qualitative variables")
  }

  if (ncol(X_quali) < 1) {
    stop("Need at least 1 qualitative variable")
  }

  # Convert to factors if needed
  X_quali[] <- lapply(X_quali, function(col) {
    if (!is.factor(col)) factor(col) else col
  })

  # Fit hierarchical clustering
  clust_mod <- ClustModalities$new(method = method)
  clust_mod$fit(X_quali)

  # Extract dendrogram heights
  heights <- clust_mod$hclust$height
  n_mod <- length(clust_mod$hclust$order)

  # Limit k_max to number of modalities
  k_max <- min(k_max, n_mod)

  # Reverse heights (last merge has highest height)
  h_rev <- rev(heights)

  # Create results table
  # k=1: all in one cluster, height = last merge
  # k=n_mod: each modality alone, height = 0
  k_vals <- 1:k_max
  heights_k <- c(h_rev, 0)[1:k_max]

  results <- data.frame(
    k = k_vals,
    height = heights_k,
    gain = c(NA, -diff(heights_k))  # Gain = reduction in height when splitting
  )

  # Detect optimal k using MAXIMUM MARGINAL GAIN
  # For CAH: optimal k is where we lose most information by merging
  # (i.e., biggest jump in dendrogram)
  if (k_max >= 3) {
    gain_data <- results$gain[!is.na(results$gain)]
    k_with_gain <- results$k[!is.na(results$gain)]

    # Optimal k = k with MAXIMUM gain
    max_gain_idx <- which.max(gain_data)
    optimal_k <- k_with_gain[max_gain_idx]

    # Ensure k >= 2
    if (optimal_k < 2) optimal_k <- 2

    # Get top 3 k values for suggestion
    top_gains_idx <- order(gain_data, decreasing = TRUE)[1:min(3, length(gain_data))]
    suggested_k <- k_with_gain[top_gains_idx]

  } else {
    optimal_k <- 2
    suggested_k <- c(2)
    warning("k_max too small for reliable elbow detection")
  }

  # Plot function
  plot_elbow <- function() {
    par(mfrow = c(1, 2))

    # Plot 1: Height curve
    plot(results$k, results$height, type = "b", pch = 19, col = "blue",
         xlab = "Number of clusters (k)",
         ylab = "Agglomeration height",
         main = "ACM-CAH: Elbow Method")
    abline(v = optimal_k, col = "red", lty = 2)
    points(optimal_k, results$height[results$k == optimal_k],
           col = "red", pch = 19, cex = 2)
    text(optimal_k, results$height[results$k == optimal_k],
         labels = paste("k* =", optimal_k), pos = 4, col = "red", font = 2)
    grid()

    # Plot 2: Marginal gain
    gain_data <- results[!is.na(results$gain), ]
    plot(gain_data$k, gain_data$gain, type = "b", pch = 19, col = "darkgreen",
         xlab = "Number of clusters (k)",
         ylab = "Gain (reduction in height)",
         main = "Marginal Gain")
    abline(h = 0, col = "gray", lty = 3)
    abline(v = optimal_k, col = "red", lty = 2)
    grid()

    par(mfrow = c(1, 1))
  }

  return(list(
    optimal_k = optimal_k,
    results = results,
    suggested_k = suggested_k,  # Top 3 candidate k values based on marginal gain
    plot = plot_elbow
  ))
}
