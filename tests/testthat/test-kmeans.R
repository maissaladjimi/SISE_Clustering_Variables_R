# ==============================================================================
# tests/testthat/test-kmeans.R
# Tests for KMeansVariablesQuant class
# ==============================================================================

test_that("KMeansVariablesQuant - Initialization", {
  # Valid initialization
  km <- KMeansVariablesQuant$new(k = 3, seed = 42)
  
  expect_s3_class(km, "KMeansVariablesQuant")
  expect_equal(km$k, 3)
  expect_equal(km$seed, 42)
  expect_null(km$clusters)
  expect_null(km$X_scaled)
})

test_that("KMeansVariablesQuant - fit() with structured data", {
  data_test <- generate_grouped_data(n = 100, seed = 123)
  
  km <- KMeansVariablesQuant$new(k = 3, n_init = 20, seed = 42)
  km$fit(data_test)
  
  # Check basic outputs
  expect_false(is.null(km$clusters))
  expect_equal(length(km$clusters), 8)  # 8 variables
  expect_equal(length(unique(km$clusters)), 3)  # 3 clusters
  expect_false(is.null(km$inertia_total))
  expect_true(km$inertia_total > 0)
  
  # Check matrix dimensions
  expect_equal(nrow(km$X_scaled), 100)  # n observations
  expect_equal(ncol(km$X_scaled), 8)    # p variables
  expect_equal(nrow(km$centers), 100)   # n x k
  expect_equal(ncol(km$centers), 3)
  expect_equal(nrow(km$r2_matrix), 8)   # p x k
  expect_equal(ncol(km$r2_matrix), 3)
})

test_that("KMeansVariablesQuant - fit() with crime dataset", {
  data(crime)
  
  km <- KMeansVariablesQuant$new(k = 3, seed = 42)
  km$fit(crime)
  
  expect_equal(length(km$clusters), ncol(crime))
  expect_equal(length(unique(km$clusters)), 3)
  expect_true(km$inertia_total > 0)
})

test_that("KMeansVariablesQuant - Inertia strictly increasing with k", {
  data_test <- data.frame(
    var1 = rnorm(100), var2 = rnorm(100), var3 = rnorm(100),
    var4 = rnorm(100), var5 = rnorm(100), var6 = rnorm(100)
  )
  
  inertias <- numeric()
  for (k in 2:5) {
    km <- KMeansVariablesQuant$new(k = k, n_init = 10, seed = 42)
    km$fit(data_test)
    inertias <- c(inertias, km$inertia_total)
  }
  
  # Check strict increase
  expect_true(check_inertia_increasing(inertias))
})

test_that("KMeansVariablesQuant - Variance explained within 0-100%", {
  data_test <- data.frame(
    var1 = rnorm(80), var2 = rnorm(80), var3 = rnorm(80),
    var4 = rnorm(80), var5 = rnorm(80)
  )
  
  km <- KMeansVariablesQuant$new(k = 3, n_init = 10, seed = 42)
  km$fit(data_test)
  
  results <- km$summary(print_output = FALSE)
  var_expl <- results$global_stats$variance_explained_pct
  
  expect_true(check_variance_valid(var_expl))
  expect_gte(var_expl, 0)
  expect_lte(var_expl, 100)
})

test_that("KMeansVariablesQuant - predict() with same number of variables", {
  data_train <- data.frame(
    var1 = rnorm(50), var2 = rnorm(50), var3 = rnorm(50),
    var4 = rnorm(50), var5 = rnorm(50)
  )
  
  km <- KMeansVariablesQuant$new(k = 2, n_init = 10, seed = 42)
  km$fit(data_train)
  
  # Predict with same number of variables
  data_new <- data.frame(
    new1 = rnorm(50), new2 = rnorm(50), new3 = rnorm(50),
    new4 = rnorm(50), new5 = rnorm(50)
  )
  
  pred <- suppressWarnings(km$predict(data_new))
  
  expect_s3_class(pred, "data.frame")
  expect_equal(nrow(pred), ncol(data_new))  # 1 row per variable
  expect_true("predicted_cluster" %in% colnames(pred))
  expect_true("r2" %in% colnames(pred))
})

test_that("KMeansVariablesQuant - predict() with different number of variables", {
  data_train <- data.frame(
    var1 = rnorm(50), var2 = rnorm(50), var3 = rnorm(50),
    var4 = rnorm(50), var5 = rnorm(50)
  )
  
  km <- KMeansVariablesQuant$new(k = 2, n_init = 10, seed = 42)
  km$fit(data_train)
  
  # More variables
  data_more <- data.frame(
    new1 = rnorm(50), new2 = rnorm(50), new3 = rnorm(50),
    new4 = rnorm(50), new5 = rnorm(50), new6 = rnorm(50)
  )
  pred_more <- suppressWarnings(km$predict(data_more))
  expect_equal(nrow(pred_more), 6)
  
  # Fewer variables
  data_less <- data.frame(new1 = rnorm(50), new2 = rnorm(50))
  pred_less <- suppressWarnings(km$predict(data_less))
  expect_equal(nrow(pred_less), 2)
  
  # Single variable
  data_single <- data.frame(new1 = rnorm(50))
  pred_single <- suppressWarnings(km$predict(data_single))
  expect_equal(nrow(pred_single), 1)
})

test_that("KMeansVariablesQuant - illustrative() method", {
  data_train <- data.frame(
    var1 = rnorm(60), var2 = rnorm(60), var3 = rnorm(60),
    var4 = rnorm(60), var5 = rnorm(60)
  )
  
  km <- KMeansVariablesQuant$new(k = 2, n_init = 10, seed = 42)
  km$fit(data_train)
  
  illust_vars <- data.frame(
    IllustVar1 = rnorm(60),
    IllustVar2 = rnorm(60)
  )
  
  result <- suppressWarnings(km$illustrative(illust_vars, plot = FALSE))
  
  expect_type(result, "list")
  expect_true("table" %in% names(result))
  expect_s3_class(result$table, "data.frame")
})

test_that("KMeansVariablesQuant - summary() returns correct structure", {
  data_test <- data.frame(
    var1 = rnorm(60), var2 = rnorm(60), var3 = rnorm(60), var4 = rnorm(60)
  )
  
  km <- KMeansVariablesQuant$new(k = 2, seed = 42)
  km$fit(data_test)
  
  results <- km$summary(print_output = FALSE)
  
  expect_type(results, "list")
  expect_true("global_stats" %in% names(results))
  expect_true("cluster_summary" %in% names(results))
  expect_true("cluster_members" %in% names(results))
  expect_true("cor_latent" %in% names(results))
  expect_true("r2_matrix" %in% names(results))
})

test_that("KMeansVariablesQuant - print() method works", {
  data_test <- data.frame(var1 = rnorm(50), var2 = rnorm(50), var3 = rnorm(50))
  
  km <- KMeansVariablesQuant$new(k = 2, seed = 42)
  km$fit(data_test)
  
  expect_output(km$print(), "K-MEANS")
})

test_that("KMeansVariablesQuant - get_clusters_table() method", {
  data_test <- data.frame(var1 = rnorm(50), var2 = rnorm(50), var3 = rnorm(50))
  
  km <- KMeansVariablesQuant$new(k = 2, seed = 42)
  km$fit(data_test)
  
  clusters_df <- km$get_clusters_table()
  
  expect_s3_class(clusters_df, "data.frame")
  expect_equal(nrow(clusters_df), 3)  # 3 variables
  expect_true("variable" %in% colnames(clusters_df))
  expect_true("cluster" %in% colnames(clusters_df))
})

test_that("KMeansVariablesQuant - Visualizations execute without error", {
  data_test <- data.frame(
    var1 = rnorm(60), var2 = rnorm(60), var3 = rnorm(60),
    var4 = rnorm(60), var5 = rnorm(60)
  )
  
  km <- KMeansVariablesQuant$new(k = 2, n_init = 10, seed = 42)
  km$fit(data_test)
  
  # Test plots don't error
  pdf(file = tempfile(fileext = ".pdf"))
  expect_silent(km$plot_correlation_circle())
  expect_silent(km$plot_biplot())
  dev.off()
})

test_that("KMeansVariablesQuant - plot_elbow() method", {
  data_test <- generate_grouped_data(n = 80, seed = 999)
  
  km <- KMeansVariablesQuant$new(k = 3, seed = 42)
  km$fit(data_test)
  
  result <- km$plot_elbow(k_range = 2:5, plot = FALSE)
  
  expect_type(result, "list")
  expect_true("optimal_k" %in% names(result))
  expect_true("results" %in% names(result))
  expect_gte(result$optimal_k, 2)
  expect_lte(result$optimal_k, 5)
})

test_that("KMeansVariablesQuant - Stability across initializations", {
  # Data with clear structure
  set.seed(555)
  n <- 80
  X1 <- rnorm(n)
  X2 <- rnorm(n)
  
  data_test <- data.frame(
    var1 = X1 + rnorm(n, 0, 0.2),
    var2 = X1 + rnorm(n, 0, 0.2),
    var3 = X2 + rnorm(n, 0, 0.2),
    var4 = X2 + rnorm(n, 0, 0.2)
  )
  
  # Multiple runs
  inertias <- numeric(5)
  for (i in 1:5) {
    km <- KMeansVariablesQuant$new(k = 2, n_init = 20, seed = i * 100)
    km$fit(data_test)
    inertias[i] <- km$inertia_total
  }
  
  # Check stability (low coefficient of variation)
  cv <- sd(inertias) / mean(inertias)
  expect_lt(cv, 0.01)  # CV < 1%
})

test_that("KMeansVariablesQuant - Error: k too large", {
  data_test <- data.frame(var1 = rnorm(50), var2 = rnorm(50))
  km <- KMeansVariablesQuant$new(k = 5, seed = 42)
  
  expect_error(km$fit(data_test), "must be less than")
})

test_that("KMeansVariablesQuant - Error: Non-numeric data", {
  data_bad <- data.frame(var1 = letters[1:10], var2 = 1:10)
  km <- KMeansVariablesQuant$new(k = 2)
  
  expect_error(km$fit(data_bad))
})

test_that("KMeansVariablesQuant - Error: predict() without fit()", {
  km <- KMeansVariablesQuant$new(k = 2)
  
  expect_error(km$predict(data.frame(x = 1:10)), "must be called before")
})

test_that("KMeansVariablesQuant - Edge case: Minimum 2 variables", {
  data_min <- data.frame(var1 = rnorm(30), var2 = rnorm(30))
  km <- KMeansVariablesQuant$new(k = 2, seed = 42)
  
  expect_silent(km$fit(data_min))
  expect_false(is.null(km$clusters))
  expect_equal(length(km$clusters), 2)
})

test_that("KMeansVariablesQuant - Edge case: k = 2 (minimum)", {
  data_test <- data.frame(var1 = rnorm(30), var2 = rnorm(30), var3 = rnorm(30))
  km <- KMeansVariablesQuant$new(k = 2, seed = 42)
  
  km$fit(data_test)
  expect_equal(length(unique(km$clusters)), 2)
})

test_that("KMeansVariablesQuant - Real data: crime dataset", {
  data(crime)
  
  km <- KMeansVariablesQuant$new(k = 3, n_init = 20, seed = 123)
  km$fit(crime)
  
  expect_equal(length(km$clusters), ncol(crime))
  
  results <- km$summary(print_output = FALSE)
  var_expl <- results$global_stats$variance_explained_pct
  expect_true(check_variance_valid(var_expl))
})
