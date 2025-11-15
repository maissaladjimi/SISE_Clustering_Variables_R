
# Determine optimal number of clusters K using elbow method
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

  # True elbow detection
  k_vals <- 2:max_k
  y <- avg_inertia[2:max_k]

  line_start <- c(k_vals[1], y[1])
  line_end   <- c(k_vals[length(k_vals)], y[length(y)])

  distance_from_line <- function(point, start, end) {
    x0 <- point[1]; y0 <- point[2]
    x1 <- start[1]; y1 <- start[2]
    x2 <- end[1]; y2 <- end[2]
    abs((y2-y1)*x0 - (x2-x1)*y0 + x2*y1 - y2*x1) / sqrt((y2-y1)^2 + (x2-x1)^2)
  }

  distances <- sapply(1:length(k_vals), function(i) {
    distance_from_line(c(k_vals[i], y[i]), line_start, line_end)
  })

  optimal_k <- k_vals[which.max(distances)]

  # Store plot function in a separate object (closure)
  plot_elbow <- function() {
    plot(k_vals, y, type = "b", pch = 19, col = "blue",
         xlab = "Number of clusters", ylab = "Average intra-cluster similarity",
         main = "VarClus Elbow Method")
    points(optimal_k, y[which.max(distances)], col = "red", pch = 19, cex = 1.5)
    text(optimal_k, y[which.max(distances)], labels = paste("K =", optimal_k), pos = 3, col = "red")
  }

  return(list(optimal_k = optimal_k, plot = plot_elbow ))
}
