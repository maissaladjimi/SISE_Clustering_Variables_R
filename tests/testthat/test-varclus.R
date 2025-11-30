# ==============================================================================
# tests/testthat/test-varclus.R
# Tests for VarClus class - ADAPTED TO REAL CLASS STRUCTURE
# ==============================================================================

test_that("VarClus - Initialization", {
  vc <- VarClus$new(similarity = "pearson", n_clusters = 3)

  expect_s3_class(vc, "VarClus")
  expect_equal(vc$similarity, "pearson")
  expect_equal(vc$n_clusters, 3)
  expect_null(vc$data)
  expect_null(vc$clusters)
})

test_that("VarClus - Initialization with spearman", {
  vc <- VarClus$new(similarity = "spearman")

  expect_equal(vc$similarity, "spearman")
})

test_that("VarClus - fit() with simple data", {
  set.seed(123)
  data_test <- data.frame(
    var1 = rnorm(100), var2 = rnorm(100), var3 = rnorm(100),
    var4 = rnorm(100), var5 = rnorm(100), var6 = rnorm(100)
  )

  vc <- VarClus$new(similarity = "pearson", n_clusters = 3)
  vc$fit(data_test)

  # Check basic outputs - clusters is a data.frame with 2 columns
  expect_false(is.null(vc$clusters))
  expect_s3_class(vc$clusters, "data.frame")
  expect_equal(nrow(vc$clusters), 6)  # 6 variables
  expect_true("cluster" %in% colnames(vc$clusters))
  expect_false(is.null(vc$model))
  expect_false(is.null(vc$model$hclust))
  expect_false(is.null(vc$model$sim))
})

test_that("VarClus - fit() with crime dataset", {
  data(crime)

  vc <- VarClus$new(similarity = "pearson", n_clusters = 4)
  vc$fit(crime)

  expect_equal(nrow(vc$clusters), ncol(crime))
  expect_equal(length(unique(vc$clusters$cluster)), 4)
  expect_false(is.null(vc$model$hclust))
})

test_that("VarClus - fit() with uscrime dataset", {
  data(uscrime)

  vc <- VarClus$new(similarity = "pearson", n_clusters = 3)
  vc$fit(uscrime)

  expect_equal(nrow(vc$clusters), ncol(uscrime))
  expect_equal(length(unique(vc$clusters$cluster)), 3)
})

test_that("VarClus - fit() without specifying n_clusters (automatic)", {
  data_test <- generate_grouped_data(n = 80, seed = 456)

  vc <- VarClus$new(similarity = "pearson")
  vc$fit(data_test)

  expect_false(is.null(vc$clusters))
  expect_true(length(unique(vc$clusters$cluster)) >= 2)
})

test_that("VarClus - Refit with different n_clusters", {
  data_test <- generate_grouped_data(n = 80, seed = 789)

  vc <- VarClus$new(similarity = "pearson", n_clusters = 3)
  vc$fit(data_test)

  original_k <- vc$n_clusters

  # Change n_clusters and refit
  vc$n_clusters <- 4
  vc$fit(data_test)

  expect_equal(vc$n_clusters, 4)
  expect_true(vc$n_clusters != original_k)
  expect_equal(length(unique(vc$clusters$cluster)), 4)
})

test_that("VarClus - predict() single variable", {
  data_train <- data.frame(
    var1 = rnorm(100), var2 = rnorm(100), var3 = rnorm(100),
    var4 = rnorm(100), var5 = rnorm(100)
  )

  vc <- VarClus$new(similarity = "pearson", n_clusters = 2)
  vc$fit(data_train)

  new_var <- rnorm(100)

  pred <- vc$predict(new_var)

  expect_type(pred, "list")
  expect_true("predicted_cluster" %in% names(pred))
  expect_true("cluster_similarity" %in% names(pred))
  expect_true("var_corr" %in% names(pred))
  expect_gte(pred$predicted_cluster, 1)
  expect_lte(pred$predicted_cluster, 2)
})

test_that("VarClus - predict() requires numeric vector", {
  data_train <- data.frame(
    var1 = rnorm(100), var2 = rnorm(100), var3 = rnorm(100),
    var4 = rnorm(100)
  )

  vc <- VarClus$new(similarity = "pearson", n_clusters = 2)
  vc$fit(data_train)

  # predict() expects a numeric vector, not a data.frame
  new_var1 <- rnorm(100)
  new_var2 <- rnorm(100)

  pred1 <- vc$predict(new_var1)
  pred2 <- vc$predict(new_var2)

  expect_type(pred1, "list")
  expect_type(pred2, "list")
})

test_that("VarClus - illustrative() method", {
  data_train <- data.frame(
    var1 = rnorm(80), var2 = rnorm(80), var3 = rnorm(80),
    var4 = rnorm(80), var5 = rnorm(80)
  )

  vc <- VarClus$new(similarity = "pearson", n_clusters = 2)
  vc$fit(data_train)

  illust_vars <- data.frame(
    illust1 = rnorm(80),
    illust2 = rnorm(80)
  )

  result <- vc$illustrative(illust_vars)

  expect_type(result, "list")
  expect_true("table" %in% names(result))
  expect_s3_class(result$table, "data.frame")
})

test_that("VarClus - summary() returns correct structure", {
  data_test <- data.frame(
    var1 = rnorm(60), var2 = rnorm(60), var3 = rnorm(60),
    var4 = rnorm(60)
  )

  vc <- VarClus$new(similarity = "pearson", n_clusters = 2)
  vc$fit(data_test)

  results <- vc$summary(print_output = FALSE)

  expect_type(results, "list")
  expect_true("global_stats" %in% names(results))
  expect_true("cluster_summary" %in% names(results))
  expect_true("cluster_quality" %in% names(results))
  expect_true("R2_details" %in% names(results))
  expect_true("similarity_matrix" %in% names(results))
})

test_that("VarClus - print() method works", {
  data_test <- data.frame(var1 = rnorm(50), var2 = rnorm(50), var3 = rnorm(50))

  vc <- VarClus$new(similarity = "pearson", n_clusters = 2)
  vc$fit(data_test)

  expect_output(vc$print(), "VARCLUS")
})

test_that("VarClus - get_clusters_table() method", {
  data_test <- data.frame(var1 = rnorm(50), var2 = rnorm(50), var3 = rnorm(50))

  vc <- VarClus$new(similarity = "pearson", n_clusters = 2)
  vc$fit(data_test)

  clusters_df <- vc$get_clusters_table()

  expect_s3_class(clusters_df, "data.frame")
  expect_equal(nrow(clusters_df), 3)
  expect_true("variable" %in% colnames(clusters_df))
  expect_true("cluster" %in% colnames(clusters_df))
})

test_that("VarClus - Visualizations execute without error", {
  data_test <- data.frame(
    var1 = rnorm(60), var2 = rnorm(60), var3 = rnorm(60),
    var4 = rnorm(60), var5 = rnorm(60)
  )

  vc <- VarClus$new(similarity = "pearson", n_clusters = 2)
  vc$fit(data_test)

  pdf(file = tempfile(fileext = ".pdf"))

  dend <- vc$get_dendrogram()
  expect_type(dend, "closure")
  dend()

  heatmap <- vc$get_heatmap()
  expect_type(heatmap, "closure")
  heatmap()

  dev.off()
})

test_that("VarClus - Similarity matrix properties", {
  data_test <- data.frame(
    var1 = rnorm(80), var2 = rnorm(80), var3 = rnorm(80),
    var4 = rnorm(80)
  )

  vc <- VarClus$new(similarity = "pearson", n_clusters = 2)
  vc$fit(data_test)

  # model$sim contains the similarity matrix
  sim_mat <- vc$model$sim

  expect_false(is.null(sim_mat))
  expect_equal(nrow(sim_mat), 4)
  expect_equal(ncol(sim_mat), 4)

  # Should be symmetric
  expect_equal(sim_mat, t(sim_mat))

  # Diagonal should be 1
  diag_vals <- diag(sim_mat)
  expect_true(all(abs(diag_vals - 1) < 1e-10))
})

test_that("VarClus - Spearman similarity", {
  set.seed(999)
  data_test <- data.frame(
    var1 = rnorm(60), var2 = rnorm(60), var3 = rnorm(60)
  )

  vc <- VarClus$new(similarity = "spearman", n_clusters = 2)
  vc$fit(data_test)

  expect_equal(vc$similarity, "spearman")
  expect_false(is.null(vc$clusters))
  expect_false(is.null(vc$model$sim))
})

test_that("VarClus - PCA within clusters", {
  data_test <- generate_grouped_data(n = 80, seed = 1111)

  vc <- VarClus$new(similarity = "pearson", n_clusters = 3)
  vc$fit(data_test)

  results <- vc$summary(print_output = FALSE)

  expect_true("cluster_summary" %in% names(results))
  cluster_summary <- results$cluster_summary

  # Check basic structure exists
  expect_s3_class(cluster_summary, "data.frame")
  expect_true(nrow(cluster_summary) > 0)
})

test_that("VarClus - R2 quality metrics", {
  data_test <- generate_grouped_data(n = 80, seed = 2222)

  vc <- VarClus$new(similarity = "pearson", n_clusters = 3)
  vc$fit(data_test)

  results <- vc$summary(print_output = FALSE)

  expect_true("cluster_quality" %in% names(results))
  r2_quality <- results$cluster_quality$mean_R2_own

  expect_true(all(r2_quality >= 0 & r2_quality <= 1))
})

test_that("VarClus - Error: Non-numeric data", {
  data_bad <- data.frame(var1 = letters[1:10], var2 = 1:10)
  vc <- VarClus$new(similarity = "pearson", n_clusters = 2)

  expect_error(vc$fit(data_bad))
})

test_that("VarClus - Error: predict() without fit()", {
  vc <- VarClus$new(similarity = "pearson", n_clusters = 2)

  expect_error(vc$predict(rnorm(10)), "must be fitted")
})

test_that("VarClus - Edge case: Minimum 2 variables", {
  data_min <- data.frame(var1 = rnorm(50), var2 = rnorm(50))
  vc <- VarClus$new(similarity = "pearson", n_clusters = 2)

  expect_silent(vc$fit(data_min))
  expect_false(is.null(vc$clusters))
  expect_equal(nrow(vc$clusters), 2)
})

test_that("VarClus - Edge case: k = 2 (minimum)", {
  data_test <- data.frame(
    var1 = rnorm(50), var2 = rnorm(50), var3 = rnorm(50)
  )
  vc <- VarClus$new(similarity = "pearson", n_clusters = 2)

  vc$fit(data_test)
  expect_equal(length(unique(vc$clusters$cluster)), 2)
})

test_that("VarClus - Real data: autos dataset (numeric part)", {
  data(autos)

  autos_num <- autos[, sapply(autos, is.numeric)]

  vc <- VarClus$new(similarity = "pearson", n_clusters = 3)
  vc$fit(autos_num)

  expect_equal(nrow(vc$clusters), ncol(autos_num))
  expect_equal(length(unique(vc$clusters$cluster)), 3)

  results <- vc$summary(print_output = FALSE)
  expect_type(results, "list")
})

test_that("VarClus - Real data: uscrime full workflow", {
  data(uscrime)

  vc <- VarClus$new(similarity = "pearson", n_clusters = 4)
  vc$fit(uscrime)

  expect_equal(nrow(vc$clusters), ncol(uscrime))

  results <- vc$summary(print_output = FALSE)
  expect_type(results, "list")

  new_var <- rnorm(nrow(uscrime))
  pred <- vc$predict(new_var)
  expect_type(pred, "list")

  clusters_df <- vc$get_clusters_table()
  expect_s3_class(clusters_df, "data.frame")
})

test_that("VarClus - Hierarchical structure preserved", {
  data_test <- generate_grouped_data(n = 100, seed = 3333)

  vc <- VarClus$new(similarity = "pearson", n_clusters = 3)
  vc$fit(data_test)

  # Check hclust object in model
  expect_s3_class(vc$model$hclust, "hclust")
  expect_false(is.null(vc$model$hclust$merge))
  expect_false(is.null(vc$model$hclust$height))
  expect_false(is.null(vc$model$hclust$order))

  # Heights should be increasing
  expect_true(all(diff(vc$model$hclust$height) >= 0))
})
