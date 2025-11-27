#' ClusterEngine for Managing Clustering Algorithms
#'
#' This R6 class provides a common wrapper around multiple clustering algorithms.
#' It stores the input dataset, the chosen method, and the resulting fitted model.
#'
#' @field data A data frame of variables to cluster.
#' @field method The clustering algorithm to use: "varclus", "kmeans", or "acm_cah".
#' @field n_clusters Number of clusters (NULL for automatic selection if applicable).
#' @field model The fitted clustering model object.
#'
#' @export
ClusterEngine <- R6::R6Class(

  "ClusterEngine",

  public = list(

    # ------------------------------------------------------------
    # Fields
    # ------------------------------------------------------------
    data = NULL,
    method = NULL,
    n_clusters = NULL,
    model = NULL,

    # ------------------------------------------------------------
    # Constructor
    # ------------------------------------------------------------
    #' @description Initialize a new ClusterEngine
    #' @param data A data frame containing the variables to cluster
    #' @param method Character string specifying the clustering algorithm
    #' @param n_clusters Number of clusters (default NULL for auto)
    initialize = function(data, method, n_clusters = NULL) {
      self$data <- data
      self$method <- method
      self$n_clusters <- n_clusters
    },


    # ------------------------------------------------------------
    # Fit the selected clustering algorithm
    # ------------------------------------------------------------
    #' @description Fit the selected clustering algorithm
    #' @return The fitted model object
    fit = function() {

      # ---- VarClus ------------------------------------------------
      if (self$method == "varclus") {
        vc <- VarClus$new(n_clusters = self$n_clusters)
        vc$fit(self$data)
        self$model <- vc
      }

      # ---- K-means ------------------------------------------------
      # FIX: Changed from kmeans to KMeansVariablesQuant
      else if (self$method == "kmeans") {
        km <- KMeansVariablesQuant$new(k = self$n_clusters)
        km$fit(self$data)
        self$model <- km
      }

      # ---- ACM + CAH ----------------------------------------------
      # FIX: Changed from acm_cah to ClustModalities
      else if (self$method == "acm_cah") {
        hc <- ClustModalities$new()
        hc$fit(self$data)
        self$model <- hc
      }

      else {
        stop(paste("Unknown method:", self$method))
      }

      return(self$model)
    },


    # ------------------------------------------------------------
    # Predict Method (Illustrative variables)
    # ------------------------------------------------------------
    #' @description Predict new variable's class using the fitted model
    #' @param new_data A data frame of new variable to predict
    #' @return Predicted cluster assignments or illustrative analysis
    predict = function(new_data) {
      if (is.null(self$model))
        stop("Model not fitted yet.")

      # Pour VarClus : utiliser illustrative si disponible (analyse complète)
      if ("illustrative" %in% names(self$model)) {
        return(self$model$illustrative(new_data))
      }

      # Pour KMeans et ACM-CAH : utiliser predict (affectation simple)
      if ("predict" %in% names(self$model)) {
        return(self$model$predict(new_data))
      }

      stop("This model does not support prediction or illustrative variables.")
    },


    # ------------------------------------------------------------
    # Print method
    # ------------------------------------------------------------
    #' @description Print model brief summary
    #' @return Output of the model's print method
    print = function() {
      if (is.null(self$model))
        stop("Model not fitted yet.")

      if ("print" %in% names(self$model))
        return(self$model$print())

      return(self$model)
    },

    # ------------------------------------------------------------
    # Summary method
    # ------------------------------------------------------------
    #' @description Display details of the fitted clustering model results
    #' @return Output of the model's summary results
    summary = function() {
      if (is.null(self$model))
        stop("Model not fitted yet.")

      if ("summary" %in% names(self$model))
        return(self$model$summary())

      return(self$model)
    },

    elbow = function(k_range = 2:10, plot = TRUE) {
      if (is.null(self$data))
        stop("Data not provided.")

      if (self$method == "kmeans") {
        result <- kmeans_elbow(
          X_num = self$data,
          k_range = k_range,
          seed = NULL
        )
      } else if (self$method == "varclus") {
        result <- varclus_elbow(
          X_num = self$data,
          similarity = "pearson"
        )
      } else if (self$method == "acm_cah") {
        result <- acm_cah_elbow(
          X_quali = self$data,
          method = "acm",
          k_max = max(k_range)
        )
      } else {
        stop("Elbow not implemented for this method")
      }

      if (plot) {
        result$plot()
      }

      invisible(result)
    }

  )
)
