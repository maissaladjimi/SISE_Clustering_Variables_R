#' Clustering of Qualitative Variables (Modalities)
#'
#' @description
#' Implements clustering of categorical variable modalities using two methods:
#' \itemize{
#'   \item **DICE**: Distance-based clustering using Dice² distance
#'   \item **ACM**: Multiple Correspondence Analysis (MCA) followed by hierarchical clustering
#' }
#'
#' @details
#' The class provides two distinct approaches for clustering qualitative modalities:
#'
#' **DICE Method**:
#' \enumerate{
#'   \item Compute disjunctive table (indicator matrix) of modalities
#'   \item Calculate Dice² distance between all pairs of modalities
#'   \item Apply hierarchical clustering with Ward's linkage (ward.D2)
#' }
#'
#' **ACM Method**:
#' \enumerate{
#'   \item Perform Multiple Correspondence Analysis (MCA) on disjunctive table
#'   \item Scale coordinates by sqrt(eigenvalues)
#'   \item Apply hierarchical clustering on factorial coordinates (ward.D)
#'   \item Optional Benzecri and Greenacre corrections for eigenvalues
#' }
#'
#' @section Distance Metrics:
#'
#' **Dice² distance**:
#' \deqn{d_{Dice}^2(m_j, m_{j'}) = \frac{1}{2} \sum_{i=1}^n (x_{ij} - x_{ij'})^2}
#' where \eqn{x_{ij}} is the indicator (0/1) for individual \eqn{i} and modality \eqn{j}.
#'
#' **Euclidean distance on MCA coordinates**:
#' \deqn{d_{MCA}(m_j, m_{j'}) = \sqrt{\sum_{s=1}^S (\phi_s \cdot f_{js} - \phi_s \cdot f_{j's})^2}}
#' where \eqn{f_{js}} are the factorial coordinates and \eqn{\phi_s = \sqrt{\lambda_s}}
#' are the scaling factors.
#'
#' @section Illustrative Variables:
#' The class supports two types of illustrative variables:
#' \itemize{
#'   \item **Qualitative**: Use \code{illustrative()} to project categorical variables
#'   \item **Quantitative**: Use \code{illustrative_numeric()} to display correlation circle (ACM only)
#' }
#'
#' @examples
#' \dontrun{
#' # Example with categorical data
#' data(mtcars)
#' mtcars_quali <- data.frame(
#'   cyl = factor(mtcars$cyl),
#'   vs = factor(mtcars$vs),
#'   am = factor(mtcars$am),
#'   gear = factor(mtcars$gear),
#'   carb = factor(mtcars$carb)
#' )
#'
#' # DICE method
#' cm_dice <- ClustModalities$new(method = "dice")
#' cm_dice$fit(mtcars_quali, k = 3)
#' cm_dice$print()
#' cm_dice$plot_dendrogram()
#'
#' # ACM method with automatic k selection
#' cm_acm <- ClustModalities$new(method = "acm", n_axes = 5)
#' cm_acm$fit(mtcars_quali, k = 3)
#' cm_acm$summary()
#'
#' # Visualizations (ACM only)
#' cm_acm$plot_factorial_map(dims = c(1, 2))
#' cm_acm$plot_scree()
#' cm_acm$plot_contrib(dim = 1, top = 10)
#'
#' # Predict new observations
#' new_data <- data.frame(
#'   cyl = factor(c(6, 8)),
#'   vs = factor(c(0, 1)),
#'   am = factor(c(1, 0)),
#'   gear = factor(c(4, 3)),
#'   carb = factor(c(2, 4))
#' )
#' predictions <- cm_acm$predict(new_data)
#'
#' # Illustrative qualitative variables
#' illust_quali <- data.frame(
#'   new_var = factor(sample(c("A", "B", "C"), nrow(mtcars), replace = TRUE))
#' )
#' illust_results <- cm_acm$illustrative(illust_quali)
#'
#' # Illustrative quantitative variables (correlation circle)
#' illust_quant <- mtcars[, c("mpg", "hp", "wt")]
#' cm_acm$illustrative_numeric(illust_quant, plot = TRUE)
#' }
#'
#' @references
#' \itemize{
#'   \item Chavent, M., Kuentz-Simonet, V., Labenne, A., & Saracco, J. (2012).
#'         ClustOfVar: An R Package for the Clustering of Variables.
#'         Journal of Statistical Software, 50(13), 1-16.
#'   \item Husson, F., Lê, S., & Pagès, J. (2017). Exploratory Multivariate Analysis
#'         by Example Using R (2nd ed.). Chapman and Hall/CRC.
#' }
#'
#' @field data Original data.frame of categorical variables
#' @field disj Disjunctive table (indicator matrix of modalities)
#' @field method Clustering method: "dice" or "acm"
#' @field n_axes Number of MCA axes to retain (ACM method only)
#' @field acm MCA result object from ade4::dudi.coa()
#' @field eig_raw Raw eigenvalues from MCA
#' @field eig_benzecri Benzecri-corrected eigenvalues
#' @field eig_greenacre Greenacre-corrected eigenvalues
#' @field ind_coords Individual coordinates on MCA axes
#' @field mod_coords Modality coordinates on MCA axes
#' @field dist_mat Distance matrix between modalities
#' @field hclust Hierarchical clustering object (hclust)
#' @field k Number of clusters
#' @field mod_clusters Cluster assignments for each modality
#'
#' @import R6
#' @import ade4
#' @importFrom stats hclust cutree dist cor
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics plot barplot abline text points arrows lines legend rect
#' @importFrom stats rect.hclust
#' @export
ClustModalities <- R6::R6Class(
  "ClustModalities",

  public = list(

    # ==========================================================================
    # PUBLIC FIELDS
    # ==========================================================================

    data = NULL,
    disj = NULL,
    method = NULL,

    # MCA-specific fields
    n_axes = NULL,
    acm = NULL,
    eig_raw = NULL,
    eig_benzecri = NULL,
    eig_greenacre = NULL,
    ind_coords = NULL,
    mod_coords = NULL,

    # Clustering fields
    dist_mat = NULL,
    hclust = NULL,
    k = NULL,
    mod_clusters = NULL,

    # ==========================================================================
    # CONSTRUCTOR
    # ==========================================================================

    #' @description Create a new ClustModalities object
    #' @param method Clustering method: "dice" or "acm" (default: "dice")
    #' @param n_axes Number of MCA axes to retain (default: NULL = automatic)
    #' @return A new `ClustModalities` object
    initialize = function(method = c("dice", "acm"), n_axes = NULL) {
      self$method <- match.arg(method)
      self$n_axes <- n_axes
    },

    # ==========================================================================
    # DATA VALIDATION
    # ==========================================================================

    #' @description Validate input data (convert to factors)
    #' @param X Data frame with categorical variables
    #' @return Validated data.frame with factor columns
    check_data = function(X) {
      if (is.matrix(X)) X <- as.data.frame(X)
      if (!is.data.frame(X))
        stop("'X' must be a data.frame or matrix")
      if (ncol(X) < 1)
        stop("'X' must contain at least 1 categorical variable")
      if (anyNA(X))
        stop("Data must not contain NA values")

      # Convert all columns to factors
      X[] <- lapply(X, function(col) if (!is.factor(col)) factor(col) else col)
      X
    },

    # ==========================================================================
    # DISTANCE COMPUTATION (DICE)
    # ==========================================================================

    #' @description Compute Dice² distance matrix
    #' @param disj_mat Disjunctive table (indicator matrix)
    #' @return Distance object (dist)
    compute_dice_distances = function(disj_mat) {
      disj_mat <- as.matrix(disj_mat)
      Q <- ncol(disj_mat)
      d2 <- matrix(0, Q, Q)
      colnames(d2) <- rownames(d2) <- colnames(disj_mat)

      for (j in seq_len(Q)) {
        for (j2 in j:Q) {
          diff_vec <- disj_mat[, j] - disj_mat[, j2]
          val <- 0.5 * sum(diff_vec * diff_vec)
          d2[j, j2] <- d2[j2, j] <- val
        }
      }
      as.dist(sqrt(d2))
    },

    # ==========================================================================
    # FIT - Main clustering method
    # ==========================================================================

    #' @description Fit the clustering model on categorical data
    #' @param X Data frame with categorical variables
    #' @param k Number of clusters (optional, can be determined later)
    #' @return Self (invisibly) for method chaining
    #' @examples
    #' \dontrun{
    #' data(vote)
    #' cm <- ClustModalities$new(method = "acm")
    #' cm$fit(vote, k = 3)
    #' }
    fit = function(X, k = NULL) {
      X <- self$check_data(X)
      self$data <- X
      self$k <- k
      self$disj <- ade4::acm.disjonctif(X)

      # ==================== DICE METHOD ====================
      if (self$method == "dice") {

        # Compute Dice² distance and perform hierarchical clustering
        self$dist_mat <- self$compute_dice_distances(self$disj)

        # Ensure labels are properly aligned
        attr(self$dist_mat, "Labels") <- colnames(self$disj)

        self$hclust <- stats::hclust(self$dist_mat, method = "ward.D2")

        # ==================== ACM METHOD ====================
      } else {

        nf <- if (is.null(self$n_axes)) 2 else min(self$n_axes, 2)

        # Perform MCA
        acm_res <- ade4::dudi.coa(self$disj, scannf = FALSE, nf = nf)
        self$acm <- acm_res

        # Eigenvalues (raw and corrected)
        self$eig_raw <- as.numeric(acm_res$eig)
        Q <- ncol(self$disj)
        K <- ncol(self$data)

        # Benzecri correction
        eig_b <- (Q/(Q-1)) * (self$eig_raw - 1/Q)
        eig_b[eig_b < 0] <- 0
        self$eig_benzecri <- eig_b

        # Greenacre correction
        self$eig_greenacre <- eig_b * (K/Q)

        # Modality coordinates (scaled by sqrt(eigenvalues))
        coords <- as.matrix(acm_res$co[, 1:nf])
        storage.mode(coords) <- "double"

        eig_nf <- as.numeric(acm_res$eig[1:nf])
        coords_scaled <- sweep(coords, 2, sqrt(eig_nf), FUN = "*")
        colnames(coords_scaled) <- paste0("Dim", 1:nf)
        self$mod_coords <- coords_scaled

        # Distance matrix on scaled coordinates (no weighting)
        self$dist_mat <- dist(coords_scaled)
        attr(self$dist_mat, "Labels") <- rownames(coords_scaled)
        self$hclust <- hclust(self$dist_mat, method = "ward.D")

        # Individual coordinates
        ind <- as.matrix(acm_res$li[, 1:nf])
        ind_scaled <- sweep(ind, 2, sqrt(eig_nf), FUN = "*")
        colnames(ind_scaled) <- paste0("Dim", 1:nf)
        self$ind_coords <- ind_scaled
      }

      # Cut dendrogram if k is specified
      if (!is.null(self$k))
        self$mod_clusters <- cutree(self$hclust, k = self$k)

      invisible(self)
    },

    # ==========================================================================
    # ELBOW METHOD
    # ==========================================================================

    #' @description Compute elbow curve for optimal k selection
    #' @param k_max Maximum number of clusters to test (default: NULL = 10)
    #' @return Data frame with k values and aggregation heights
    compute_elbow = function(k_max = NULL) {
      if (is.null(self$hclust))
        stop("fit() must be executed before compute_elbow()")

      h <- self$hclust$height
      n_mod <- length(self$hclust$order)

      if (is.null(k_max)) k_max <- min(10, n_mod)

      # Heights for k = 1..n_mod
      h_rev <- rev(h)
      heights_k <- c(h_rev, 0)

      k_vals <- 1:n_mod
      df <- data.frame(
        k = k_vals,
        height = heights_k
      )

      df[df$k <= k_max, , drop = FALSE]
    },

    #' @description Plot elbow curve
    #' @param k_max Maximum number of clusters to display
    #' @return Data frame (invisibly)
    plot_elbow = function(k_max = NULL) {
      df <- self$compute_elbow(k_max = k_max)

      graphics::plot(df$k, df$height, type = "b", pch = 19,
                     xlab = "Number of clusters k",
                     ylab = "Aggregation height",
                     main = "Elbow Curve - Aggregation Levels")
      invisible(df)
    },

    #' @description Elbow method with automatic detection of optimal k
    #' @param k_max Maximum k to test (default: 10)
    #' @param plot Logical, whether to display the plot (default: TRUE)
    #' @return List with optimal_k, results, and plot function
    elbow = function(k_max = 10, plot = TRUE) {
      if (is.null(self$data)) {
        stop("No data available. Use fit() first.")
      }

      # Call standalone elbow function
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

    # ==========================================================================
    # DENDROGRAM PLOT
    # ==========================================================================

    #' @description Plot dendrogram with optional cluster coloring
    #' @param k Number of clusters to highlight (default: NULL = use self$k)
    #' @param ... Additional arguments passed to plot()
    #' @return NULL (invisibly)
    #' @examples
    #' \dontrun{
    #' data(vote)
    #' cm <- ClustModalities$new(method = "acm")
    #' cm$fit(vote, k = 3)
    #' cm$plot_dendrogram(k = 3)
    #' }
    plot_dendrogram = function(k = NULL, ...) {
      if (is.null(self$hclust))
        stop("fit() must be executed before plot_dendrogram()")

      plot(self$hclust,
           main = paste("Dendrogram of Modalities -", toupper(self$method)),
           xlab = "", sub = "")

      kk <- if (!is.null(k)) k else self$k
      if (!is.null(kk)) {
        dark_cols <- grDevices::colorRampPalette(c(
          "#1f77b4", "#ff7f0e", "#2ca02c",
          "#d62728", "#9467bd", "#8c564b",
          "#e377c2", "#7f7f7f", "#bcbd22",
          "#17becf"
        ))(kk)

        rect.hclust(self$hclust, k = kk, border = dark_cols)
      }
    },

    # ==========================================================================
    # FACTORIAL MAP (ACM only)
    # ==========================================================================

    #' @description Plot factorial map of modalities (ACM method only)
    #' @param dims Dimensions to plot (default: c(1, 2))
    #' @param k Number of clusters to color (default: NULL = use self$k)
    #' @return NULL (invisibly)
    #' @examples
    #' \dontrun{
    #' data(vote)
    #' cm <- ClustModalities$new(method = "acm")
    #' cm$fit(vote, k = 3)
    #' cm$plot_factorial_map(dims = c(1, 2), k = 3)
    #' }
    plot_factorial_map = function(dims = c(1, 2), k = NULL) {
      if (self$method != "acm")
        stop("plot_factorial_map() is only available for ACM method")
      if (is.null(self$mod_coords))
        stop("fit() must be executed first")

      coords <- self$mod_coords[, dims]
      kk <- if (!is.null(k)) k else self$k

      # Color by cluster if available
      if (!is.null(kk) && !is.null(self$mod_clusters)) {
        dark_cols <- grDevices::colorRampPalette(c(
          "#1f77b4", "#ff7f0e", "#2ca02c",
          "#d62728", "#9467bd", "#8c564b",
          "#e377c2", "#7f7f7f", "#bcbd22",
          "#17becf"
        ))(kk)
        cols <- dark_cols[self$mod_clusters]
      } else {
        cols <- "black"
      }

      # Calculate % inertia explained
      eig <- self$eig_raw[dims]
      perc <- 100 * eig / sum(self$eig_raw)

      plot(coords[, 1], coords[, 2], pch = 19, col = cols,
           xlab = paste0("Dim", dims[1], " (", round(perc[1], 2), "%)"),
           ylab = paste0("Dim", dims[2], " (", round(perc[2], 2), "%)"),
           main = "Factorial Map - Modalities")

      text(coords[, 1], coords[, 2],
           labels = rownames(coords),
           pos = 3, cex = 0.8, font = 2, col = cols)

      abline(h = 0, v = 0, lty = 3)

      # Add legend if clusters are defined
      if (!is.null(kk) && !is.null(self$mod_clusters)) {
        dark_cols <- grDevices::colorRampPalette(c(
          "#1f77b4", "#ff7f0e", "#2ca02c",
          "#d62728", "#9467bd", "#8c564b",
          "#e377c2", "#7f7f7f", "#bcbd22",
          "#17becf"
        ))(kk)

        legend("topright",
               legend = paste("Cluster", 1:kk),
               col = dark_cols,
               pch = 19,
               pt.cex = 1.3,
               bty = "n")
      }
    },

    # ==========================================================================
    # SCREE PLOT (ACM only)
    # ==========================================================================

    #' @description Plot scree plot of eigenvalues (ACM method only)
    #' @param cumulative Logical, whether to plot cumulative inertia (default: FALSE)
    #' @return Percentage of inertia (invisibly)
    #' @examples
    #' \dontrun{
    #' data(vote)
    #' cm <- ClustModalities$new(method = "acm")
    #' cm$fit(vote, k = 3)
    #' cm$plot_scree()
    #' cm$plot_scree(cumulative = TRUE)
    #' }
    plot_scree = function(cumulative = FALSE) {
      if (self$method != "acm")
        stop("plot_scree() is only available for ACM method")
      if (is.null(self$eig_raw))
        stop("fit() must be executed first")

      eig <- self$eig_raw
      perc <- 100 * eig / sum(eig)

      if (!cumulative) {
        plot(seq_along(perc), perc, type = "b", pch = 19,
             xlab = "Dimension", ylab = "% Inertia Explained",
             main = "Scree Plot (MCA)")
      } else {
        plot(seq_along(perc), cumsum(perc), type = "b", pch = 19,
             xlab = "Dimension", ylab = "% Cumulative Inertia",
             main = "Cumulative Inertia (MCA)")
      }
      abline(h = 0, lty = 3)
      invisible(perc)
    },

    # ==========================================================================
    # CONTRIBUTIONS (ACM only)
    # ==========================================================================

    #' @description Plot modality contributions to a dimension (ACM method only)
    #' @param dim Dimension number (default: 1)
    #' @param top Number of top contributors to display (default: NULL = all)
    #' @return Contribution values (invisibly)
    #' @examples
    #' \dontrun{
    #' data(vote)
    #' cm <- ClustModalities$new(method = "acm")
    #' cm$fit(vote, k = 3)
    #' cm$plot_contrib(dim = 1, top = 10)
    #' }
    plot_contrib = function(dim = 1, top = NULL) {
      if (self$method != "acm")
        stop("plot_contrib() is only available for ACM method")

      mass <- as.numeric(self$acm$cw)
      coords <- self$mod_coords[, dim]
      lambda <- as.numeric(self$acm$eig[dim])

      contrib <- 100 * (mass * coords^2) / lambda
      contrib <- sort(contrib, decreasing = TRUE)

      if (!is.null(top))
        contrib <- contrib[1:min(top, length(contrib))]

      barplot(contrib, las = 2, cex.names = 0.7,
              main = paste("Contributions (%) - Axis", dim),
              ylab = "Contribution (%)")

      invisible(contrib)
    },

    # ==========================================================================
    # ILLUSTRATIVE NUMERIC - Quantitative variables (ACM only)
    # ==========================================================================

    #' @description Project quantitative illustrative variables (correlation circle)
    #' @param X_quant Data frame with numeric variables
    #' @param plot Logical, whether to display the plot (default: TRUE)
    #' @return Data frame with correlations
    #' @examples
    #' \dontrun{
    #' data(vote)
    #' cm <- ClustModalities$new(method = "acm")
    #' cm$fit(vote, k = 3)
    #' quant_vars <- data.frame(x = rnorm(nrow(vote)), y = rnorm(nrow(vote)))
    #' cors <- cm$illustrative_numeric(quant_vars, plot = TRUE)
    #' }
    illustrative_numeric = function(X_quant, plot = TRUE) {
      if (self$method != "acm")
        stop("illustrative_numeric() is only available for method='acm'")
      if (is.null(self$ind_coords))
        stop("fit() must be executed before illustrative_numeric()")

      if (!is.data.frame(X_quant))
        stop("X_quant must be a data.frame of numeric variables")
      if (nrow(X_quant) != nrow(self$data))
        stop("X_quant must have the same number of observations")
      if (!all(sapply(X_quant, is.numeric)))
        stop("All columns of X_quant must be numeric")

      # Correlations with MCA dimensions
      CF <- self$ind_coords

      cors <- sapply(X_quant, function(v) {
        c(cor(v, CF[, 1]), cor(v, CF[, 2]))
      })
      cors <- t(cors)
      colnames(cors) <- c("cor.Dim1", "cor.Dim2")

      if (plot) {
        graphics::plot(cors[, 1], cors[, 2], pch = 19,
                       xlim = c(-1, 1), ylim = c(-1, 1),
                       xlab = "Correlation Dim1", ylab = "Correlation Dim2",
                       main = "Correlation Circle - Quantitative Illustrative Variables")

        # Draw unit circle
        t <- seq(0, 2*pi, length.out = 200)
        lines(cos(t), sin(t), col = "grey70")

        abline(h = 0, v = 0, lty = 3)
        text(cors[, 1], cors[, 2], labels = rownames(cors), pos = 3)
      }

      rownames(cors) <- colnames(X_quant)
      return(as.data.frame(cors))
    },

    # ==========================================================================
    # ILLUSTRATIVE - Qualitative illustrative variables
    # ==========================================================================

    #' @description Project qualitative illustrative variables onto clusters
    #' @param X_illust Data frame with categorical illustrative variables
    #' @param plot Logical, whether to display the plot (default: TRUE)
    #' @return List containing:
    #' * `table` - Data frame with cluster assignments and distances
    #' * `plot` - Function to generate the visualization
    #' @examples
    #' \dontrun{
    #' data(vote)
    #' cm <- ClustModalities$new(method = "acm")
    #' cm$fit(vote, k = 3)
    #' illust_vars <- data.frame(extra = factor(sample(c("A", "B"), nrow(vote), replace = TRUE)))
    #' illust_results <- cm$illustrative(illust_vars, plot = TRUE)
    #' }
    illustrative = function(X_illust, plot = TRUE) {
      if (is.null(self$mod_clusters))
        stop("fit() with k must be executed before illustrative()")

      # Filter out numeric variables
      X_illust <- X_illust[, !sapply(X_illust, is.numeric), drop = FALSE]

      if (ncol(X_illust) == 0) {
        stop("No categorical illustrative variables. Use 'illustrative_numeric()' for quantitative variables.")
      }

      X_illust <- self$check_data(X_illust)

      if (nrow(X_illust) != nrow(self$data))
        stop("X_illust must have the same number of observations as training data")

      # Create disjunctive table for illustrative variables
      disj_illust <- ade4::acm.disjonctif(X_illust)

      K <- self$k
      n_mod_illust <- ncol(disj_illust)

      # ==================== DICE METHOD ====================
      if (self$method == "dice") {

        # Dice² distance function
        dice_dist <- function(m1, m2) {
          0.5 * sum((m1 - m2)^2)
        }

        # For each illustrative modality, calculate average distance to each cluster
        result_list <- list()

        for (m in colnames(disj_illust)) {
          vec_m <- disj_illust[, m]

          # Average distance to each cluster
          mean_dist <- numeric(K)

          for (k in 1:K) {
            # Modalities in cluster k
            members_k <- names(self$mod_clusters)[self$mod_clusters == k]

            # Calculate distances
            dists <- sapply(members_k, function(member) {
              dice_dist(vec_m, self$disj[, member])
            })

            mean_dist[k] <- mean(dists)
          }

          # Assigned cluster (minimum distance)
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

        # ==================== ACM METHOD ====================
      } else {

        # Project illustrative modalities in MCA space
        L <- ncol(self$ind_coords)
        coords_illust <- matrix(0, nrow = n_mod_illust, ncol = L,
                                dimnames = list(colnames(disj_illust),
                                                colnames(self$ind_coords)))

        for (m in colnames(disj_illust)) {
          idx <- which(disj_illust[, m] == 1)
          if (length(idx) > 0)
            coords_illust[m, ] <- colMeans(self$ind_coords[idx, , drop = FALSE])
        }

        # Calculate cluster barycenters
        centers <- rowsum(self$mod_coords, self$mod_clusters) /
          as.numeric(table(self$mod_clusters))

        # For each illustrative modality, calculate distance to each barycenter
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

      # Visualization function
      plot_func <- function() {

        if (self$method == "acm" && !is.null(self$mod_coords)) {

          coords_active <- self$mod_coords[, 1:2]

          xlim <- range(coords_active[, 1]) * 1.2
          ylim <- range(coords_active[, 2]) * 1.2

          # Cluster colors
          dark_cols <- grDevices::colorRampPalette(c(
            "#1f77b4", "#ff7f0e", "#2ca02c",
            "#d62728", "#9467bd", "#8c564b",
            "#e377c2", "#7f7f7f", "#bcbd22",
            "#17becf"
          ))(K)

          cols <- dark_cols[self$mod_clusters]

          # Plot active modalities
          plot(coords_active[, 1], coords_active[, 2],
               pch = 19, col = cols,
               cex = 1.3,
               xlab = "Dim 1", ylab = "Dim 2",
               xlim = xlim, ylim = ylim,
               main = "Active and Illustrative Modalities")

          text(coords_active[, 1], coords_active[, 2],
               labels = rownames(coords_active),
               pos = 3, cex = 0.8, col = cols)

          # Plot illustrative modalities
          if (exists("coords_illust")) {

            # Get assigned clusters
            cluster_assign <- result_table$cluster_assigned
            cols_illu <- dark_cols[cluster_assign]

            # Stars for illustrative
            points(coords_illust[, 1], coords_illust[, 2],
                   pch = 8, col = cols_illu, cex = 1.8, lwd = 2)

            # Labels
            text(coords_illust[, 1], coords_illust[, 2],
                 labels = rownames(coords_illust),
                 pos = 3, cex = 0.9, col = cols_illu, font = 2)
          }

          abline(h = 0, v = 0, lty = 3, col = "grey70")

          legend("topright",
                 legend = c(paste("Cluster", 1:K), "Illustrative"),
                 col = c(dark_cols, "black"),
                 pch = c(rep(19, K), 8),
                 pt.cex = c(rep(1.5, K), 2),
                 bty = "n")

        } else {
          # For DICE, barplot of distances
          par(mfrow = c(min(2, ceiling(nrow(result_table)/2)), 2))

          for (i in 1:min(nrow(result_table), 4)) {
            dists <- as.numeric(result_table[i, 3:(2+K)])
            assigned <- result_table$cluster_assigned[i]

            barplot(dists,
                    names.arg = paste0("C", 1:K),
                    main = paste("Modality:", result_table$modality[i]),
                    ylab = "Average Distance",
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

    # ==========================================================================
    # CLUSTER TABLE
    # ==========================================================================

    #' @description Get cluster assignments as a data frame
    #' @param k Number of clusters (default: NULL = use self$k)
    #' @return Data frame with modalities and cluster assignments
    cluster_table = function(k = NULL) {
      if (is.null(self$hclust))
        stop("fit() must be executed before cluster_table()")

      kk <- if (!is.null(k)) k else self$k
      if (is.null(kk))
        stop("k is missing")

      cl <- cutree(self$hclust, k = kk)
      out <- data.frame(
        cluster = cl,
        modality = names(cl),
        stringsAsFactors = FALSE
      )
      out[order(out$cluster, out$modality), ]
    },

    # ==========================================================================
    # PREDICT - Assign new modalities to clusters
    # ==========================================================================

    #' @description Assign new categorical variables to existing clusters
    #' @param X_new Data frame with new categorical variables
    #' @return Data frame with cluster assignments and distances
    #' @examples
    #' \dontrun{
    #' data(vote)
    #' cm <- ClustModalities$new(method = "acm")
    #' cm$fit(vote, k = 3)
    #' new_data <- vote[1:5, ]
    #' predictions <- cm$predict(new_data)
    #' }
    predict = function(X_new) {
      if (is.null(self$hclust))
        stop("fit() must be executed first")
      if (is.null(self$k))
        stop("fit(..., k=) is required")

      if (is.matrix(X_new)) X_new <- as.data.frame(X_new)
      X_new <- self$check_data(X_new)

      if (nrow(X_new) != nrow(self$data))
        stop("Incompatible number of observations")

      disj_new <- ade4::acm.disjonctif(X_new)

      # ==================== DICE METHOD ====================
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

      # ==================== ACM METHOD ====================
      L <- ncol(self$ind_coords)
      coords_new <- matrix(0, nrow = ncol(disj_new), ncol = L,
                           dimnames = list(colnames(disj_new),
                                           colnames(self$ind_coords)))

      for (m in colnames(disj_new)) {
        idx <- which(disj_new[, m] == 1)
        if (length(idx) > 0)
          coords_new[m, ] <- colMeans(self$ind_coords[idx, , drop = FALSE])
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
      df[order(df$cluster, df$distance), ]
    },

    # ==========================================================================
    # PRINT - Display summary
    # ==========================================================================

    #' @description Print model summary
    #' @param ... Additional arguments (ignored)
    #' @return Self (invisibly)
    print = function(...) {
      cat("========================================\n")
      cat("  CLUSTERING OF QUALITATIVE MODALITIES\n")
      cat("========================================\n")
      cat(sprintf("Method: %s\n", toupper(self$method)))

      if (!is.null(self$data)) {
        cat(sprintf("\nData:\n"))
        cat(sprintf("  - Observations              : %d\n", nrow(self$data)))
        cat(sprintf("  - Categorical variables     : %d\n", ncol(self$data)))
        if (!is.null(self$disj)) {
          cat(sprintf("  - Total modalities          : %d\n", ncol(self$disj)))
        }
      }

      if (!is.null(self$k)) {
        cat(sprintf("\nClustering:\n"))
        cat(sprintf("  - Number of clusters (k)    : %d\n", self$k))
        if (!is.null(self$mod_clusters)) {
          tbl <- table(self$mod_clusters)
          cat(sprintf("\nCluster sizes:\n"))
          for (i in 1:length(tbl)) {
            cat(sprintf("  - Cluster %d                 : %d modalities\n", i, tbl[i]))
          }
        }
      }

      if (self$method == "acm" && !is.null(self$eig_raw)) {
        cat(sprintf("\nMCA:\n"))
        cat(sprintf("  - Total inertia             : %.4f\n", sum(self$eig_raw)))
        cat(sprintf("  - Inertia Dim1              : %.2f%%\n",
                    100 * self$eig_raw[1] / sum(self$eig_raw)))
        cat(sprintf("  - Inertia Dim2              : %.2f%%\n",
                    100 * self$eig_raw[2] / sum(self$eig_raw)))
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
    #' * `basic_stats` - Basic dataset statistics
    #' * `acm_stats` - MCA eigenvalue statistics (ACM method only)
    #' * `cluster_stats` - Per-cluster statistics
    #' * `cluster_composition` - Detailed cluster composition
    #' * `method` - Clustering method used
    summary = function(print_output = TRUE) {
      if (is.null(self$data))
        stop("fit() must be executed before summary()")

      # Basic statistics
      basic_stats <- data.frame(
        metric = c("Number of observations", "Number of variables", "Number of modalities"),
        value = c(nrow(self$data), ncol(self$data),
                  if (!is.null(self$disj)) ncol(self$disj) else NA),
        stringsAsFactors = FALSE
      )

      # MCA statistics if applicable
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

      # Cluster statistics if k is defined
      cluster_stats <- NULL
      cluster_composition <- NULL

      if (!is.null(self$mod_clusters)) {
        # Cluster sizes
        cluster_sizes <- as.numeric(table(self$mod_clusters))
        cluster_stats <- data.frame(
          cluster = 1:self$k,
          n_modalities = cluster_sizes,
          pct_total = round(100 * cluster_sizes / sum(cluster_sizes), 2),
          stringsAsFactors = FALSE
        )

        # Detailed composition
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
        cat("  BASIC STATISTICS\n")
        cat("========================================\n")
        print(basic_stats, row.names = FALSE)

        if (!is.null(acm_stats)) {
          cat("\n========================================\n")
          cat("  MCA EIGENVALUES\n")
          cat("========================================\n")
          print(acm_stats, row.names = FALSE)
        }

        if (!is.null(cluster_stats)) {
          cat("\n========================================\n")
          cat("  CLUSTER STATISTICS\n")
          cat("========================================\n")
          print(cluster_stats, row.names = FALSE)

          cat("\n========================================\n")
          cat("  CLUSTER COMPOSITION (top 30)\n")
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

    # ==========================================================================
    # GET_CLUSTERS_TABLE
    # ==========================================================================

    #' @description Get cluster assignments as a data frame
    #' @return Data frame with modalities and cluster assignments
    get_clusters_table = function() {
      if (is.null(self$mod_clusters))
        stop("fit() must be executed with k before get_clusters_table()")

      df <- data.frame(
        modality = names(self$mod_clusters),
        cluster = unname(self$mod_clusters),
        stringsAsFactors = FALSE
      )

      df[order(df$cluster, df$modality), ]
    }
  )
)
