# ==============================================================================
# ELBOW METHODS FOR CLUSTERING
#
# This file contains elbow methods for:
# - VarClus (hierarchical clustering of variables)
# - K-Means de Variables (partitional clustering of variables)
# - ACM-CAH (clustering of qualitative modalities)
# ==============================================================================

# ==============================================================================
# HELPER FUNCTION: Detect elbow using perpendicular distance method
# ==============================================================================
detect_elbow <- function(k_vals, y_vals) {
  line_start <- c(k_vals[1], y_vals[1])
  line_end   <- c(k_vals[length(k_vals)], y_vals[length(y_vals)])

  distance_from_line <- function(point, start, end) {
    x0 <- point[1]; y0 <- point[2]
    x1 <- start[1]; y1 <- start[2]
    x2 <- end[1]; y2 <- end[2]
    abs((y2-y1)*x0 - (x2-x1)*y0 + x2*y1 - y2*x1) / sqrt((y2-y1)^2 + (x2-x1)^2)
  }

  distances <- sapply(seq_along(k_vals), function(i) {
    distance_from_line(c(k_vals[i], y_vals[i]), line_start, line_end)
  })

  k_vals[which.max(distances)]
}

# ==============================================================================
# VARCLUS ELBOW
# ==============================================================================
#' Determine optimal number of clusters K using elbow method (VarClus)
#'
#' @param X_num Numeric data matrix
#' @param similarity Similarity measure ("spearman" or "pearson")
#' @return List with optimal_k and plot function
#' @export
varclus_elbow <- function(X_num, similarity = "spearman") {

  # Expect numeric matrix
  if (!is.matrix(X_num) && !all(sapply(X_num, is.numeric))) {
    stop("Input to varclus_elbow() must be numeric.")
  }

  if (ncol(X_num) < 2) stop("Need at least 2 numeric variables.")

  # Run varclus
  vc <- Hmisc::varclus(x = X_num, similarity = similarity)
  hc <- vc$hclust
  max_k <- ncol(X_num) - 1

  # Compute average intra-cluster inertia
  avg_inertia <- numeric(max_k)
  for (k in 2:max_k) {
    clust <- cutree(hc, k = k)
    cluster_sim <- numeric(k)

    for (i in 1:k) {
      vars <- names(clust[clust == i])
      if (length(vars) > 1) {
        mat <- cor(X_num[, vars])^2
        cluster_sim[i] <- mean(mat[upper.tri(mat)])
      } else {
        cluster_sim[i] <- 1
      }
    }
    avg_inertia[k] <- mean(cluster_sim)
  }

  # Elbow detection
  k_vals <- 2:max_k
  y <- avg_inertia[2:max_k]
  optimal_k <- detect_elbow(k_vals, y)

  # Plot function
  plot_elbow <- function() {
    plot(k_vals, y, type = "b", pch = 19, col = "blue",
         xlab = "Number of clusters", ylab = "Average intra-cluster similarity",
         main = "VarClus Elbow Method")
    points(optimal_k, y[optimal_k - 1], col = "red", pch = 19, cex = 1.5)
    text(optimal_k, y[optimal_k - 1], labels = paste("K =", optimal_k), pos = 3, col = "red")
  }

  return(list(optimal_k = optimal_k, plot = plot_elbow, k_vals = k_vals, inertia = y))
}

# ==============================================================================
# K-MEANS ELBOW (Variables)
# ==============================================================================
#' Determine optimal number of clusters K using elbow method (K-Means Variables)
#'
#' @param X_num Numeric data matrix or data.frame
#' @param k_range Range of k values to test (default: 2:10)
#' @param n_init Number of random initializations per k (default: 20)
#' @param seed Random seed for reproducibility
#' @return List with optimal_k, plot function, results data.frame
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

  # Load KMeansVariablesQuant class (assume it's already loaded)
  # Run K-means for each k
  for (k in k_range) {
    km <- KMeansVariablesQuant$new(k = k, n_init = n_init, seed = seed)
    km$fit(X_num)

    inertia <- km$inertia_total
    gain <- if (nrow(results) == 0) NA else inertia - results$inertia[nrow(results)]

    results <- rbind(results, data.frame(k = k, inertia = inertia, gain = gain))
  }

  # Detect optimal k (skip k=2 for elbow detection)
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

    # Plot 2: Marginal gain
    gain_data <- results[!is.na(results$gain), ]
    plot(gain_data$k, gain_data$gain, type = "b", pch = 19, col = "darkgreen",
         xlab = "Number of clusters (k)",
         ylab = "Marginal gain in inertia",
         main = "Marginal Gain (Δ inertia)")
    abline(h = 0, col = "gray", lty = 3)
    abline(v = optimal_k, col = "red", lty = 2)

    par(mfrow = c(1, 1))
  }

  return(list(
    optimal_k = optimal_k,
    results = results,
    plot = plot_elbow
  ))
}

# ==============================================================================
# ACM-CAH ELBOW (Modalités)
# ==============================================================================
#' Determine optimal number of clusters K using elbow method (ACM-CAH)
#'
#' @param X_quali Qualitative data (data.frame with factors)
#' @param method "dice" or "acm" (default: "acm")
#' @param k_max Maximum number of clusters to consider (default: 10)
#' @return List with optimal_k, plot function, results data.frame
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

  # Fit CAH
  clust_mod <- ClustModalities$new(method = method)
  clust_mod$fit(X_quali)

  # Get heights from dendrogram
  heights <- clust_mod$hclust$height
  n_mod <- length(clust_mod$hclust$order)

  # Limit k_max
  k_max <- min(k_max, n_mod)

  # Reverse heights (last merge has highest height)
  h_rev <- rev(heights)

  # Create results: k and corresponding height
  # k=1: all in one cluster, height = last merge
  # k=n_mod: each modality alone, height = 0
  k_vals <- 1:k_max
  heights_k <- c(h_rev, 0)[1:k_max]

  results <- data.frame(
    k = k_vals,
    height = heights_k,
    gain = c(NA, -diff(heights_k))  # Gain = reduction in height
  )

  # Detect optimal k using MAXIMUM MARGINAL GAIN
  # For CAH: optimal k is where we lose the most information by merging
  # The gain represents the reduction in height when going from k-1 to k clusters
  if (k_max >= 3) {
    # Use gain (reduction in height) - already computed in results$gain
    gain_data <- results$gain[!is.na(results$gain)]
    k_with_gain <- results$k[!is.na(results$gain)]

    # Optimal k = k with MAXIMUM gain (biggest jump in the dendrogram)
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

    # Plot 2: Gain (reduction in height)
    gain_data <- results[!is.na(results$gain), ]
    plot(gain_data$k, gain_data$gain, type = "b", pch = 19, col = "darkgreen",
         xlab = "Number of clusters (k)",
         ylab = "Gain (reduction in height)",
         main = "Marginal Gain")
    abline(h = 0, col = "gray", lty = 3)
    abline(v = optimal_k, col = "red", lty = 2)

    par(mfrow = c(1, 1))
  }

  return(list(
    optimal_k = optimal_k,
    results = results,
    suggested_k = suggested_k,  # Top 3 k candidats basés sur gain marginal
    plot = plot_elbow
  ))
}
