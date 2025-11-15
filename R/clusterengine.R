#' ClusterEngine for Managing Clustering Algorithms
#'
#' An R6 class that provides a unified interface for performing clustering on a dataset.
#' It stores the dataset and the result of the selected clustering algorithm.
#'
#' @export
ClusterEngine <- R6::R6Class(
  "ClusterEngine",
  public = list(
    data = NULL,
    method = NULL,
    n_clusters = NULL,
    model = NULL,

    initialize = function(data, method, n_clusters = NULL) {
      self$data <- data
      self$method <- method
      self$n_clusters <- n_clusters
    },

    fit = function() {

      if (self$method == "varclus") {
        vc <- VarClus$new()
        vc$fit(self$data)
        self$model <- vc
      }

      if (self$method == "kmeans") {
        km <- kmeansvar$new(n_clusters = self$n_clusters)
        km$fit(self$data)
        self$model <- km
      }

      if (self$method == "acm_cah") {
        # data must be split into quantitative & qualitative
        X.quanti <- self$data[, sapply(self$data, is.numeric), drop = FALSE]
        X.quali  <- self$data[, sapply(self$data, function(x) is.factor(x) || is.character(x)), drop = FALSE]

        hc <- hclustvar(X.quanti, X.quali)
        self$model <- hc
      }

      return(self$model)
    },

    predict = function(new_data) {
      if (is.null(self$model)) stop("Model not fitted yet")
      if ("predict" %in% names(self$model)) {
        return(self$model$predict(new_data))
      }
      stop("This model has no predict method.")
    },

    summary = function() {
      if (is.null(self$model)) stop("Model not fitted yet")
      if ("summary" %in% names(self$model)) {
        return(self$model$summary())
      }
      return(self$model)
    }
  )
)

