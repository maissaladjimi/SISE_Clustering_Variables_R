# ==============================================================================
# tests/testthat/test-acm_cah.R
# Tests for ClustModalities class (ACM and DICE methods)
# ==============================================================================

test_that("ClustModalities - Initialization ACM", {
  cm <- ClustModalities$new(method = "acm", n_axes = 5)

  expect_s3_class(cm, "ClustModalities")
  expect_equal(cm$method, "acm")
  expect_equal(cm$n_axes, 5)
  expect_null(cm$data)
  expect_null(cm$mod_clusters)
})

test_that("ClustModalities - Initialization DICE", {
  cm <- ClustModalities$new(method = "dice")  # Pas de hclust_method

  expect_s3_class(cm, "ClustModalities")
  expect_equal(cm$method, "dice")
})

test_that("ClustModalities ACM - fit() with simple data", {
  data_test <- generate_quali_data(n = 100, seed = 123)

  cm <- ClustModalities$new(method = "acm")
  cm$fit(data_test, k = 3)

  # Check basic outputs
  expect_false(is.null(cm$mod_clusters))
  expect_equal(cm$k, 3)
  expect_equal(length(unique(cm$mod_clusters)), 3)
  expect_false(is.null(cm$acm))
  expect_false(is.null(cm$mod_coords))
  expect_false(is.null(cm$hclust))
})

test_that("ClustModalities DICE - fit() with simple data", {
  data_test <- generate_quali_data(n = 100, seed = 123)

  cm <- ClustModalities$new(method = "dice")
  cm$fit(data_test, k = 3)

  expect_false(is.null(cm$mod_clusters))
  expect_equal(cm$k, 3)
  expect_equal(length(unique(cm$mod_clusters)), 3)
  expect_false(is.null(cm$hclust))
  expect_false(is.null(cm$dist_matrix))
})

test_that("ClustModalities ACM - fit() with vote dataset", {
  data(vote)

  cm <- ClustModalities$new(method = "acm")
  cm$fit(vote, k = 3)

  expect_equal(cm$k, 3)
  expect_false(is.null(cm$mod_clusters))
  expect_false(is.null(cm$acm))
})

test_that("ClustModalities DICE - fit() with vote dataset", {
  data(vote)

  cm <- ClustModalities$new(method = "dice")
  cm$fit(vote, k = 3)

  expect_equal(cm$k, 3)
  expect_false(is.null(cm$mod_clusters))
  expect_false(is.null(cm$dist_matrix))
})

test_that("ClustModalities ACM - Change k with new fit", {
  data_test <- generate_quali_data(n = 100, seed = 456)

  cm <- ClustModalities$new(method = "acm")
  cm$fit(data_test, k = 3)

  # Re-fit with different k
  cm$fit(data_test, k = 4)

  expect_equal(cm$k, 4)
  expect_equal(length(unique(cm$mod_clusters)), 4)
})

test_that("ClustModalities ACM - predict() on new observations", {
  data_train <- generate_quali_data(n = 100, seed = 789)

  cm <- ClustModalities$new(method = "acm")
  cm$fit(data_train, k = 3)

  # New observations with same structure
  data_new <- generate_quali_data(n = 10, seed = 999)

  predictions <- suppressWarnings(cm$predict(data_new))

  expect_s3_class(predictions, "data.frame")
  expect_equal(nrow(predictions), 10)  # 10 new observations
  expect_true("predicted_cluster" %in% colnames(predictions))
  expect_true("distance" %in% colnames(predictions))
})

test_that("ClustModalities ACM - illustrative() qualitative variables", {
  data_train <- generate_quali_data(n = 100, seed = 111)

  cm <- ClustModalities$new(method = "acm")
  cm$fit(data_train, k = 3)

  # Illustrative variable
  illust_var <- data.frame(
    extra = factor(sample(c("Type1", "Type2", "Type3"), 100, replace = TRUE))
  )

  result <- suppressWarnings(cm$illustrative(illust_var, plot = FALSE))

  expect_type(result, "list")
  expect_true("table" %in% names(result))
  expect_s3_class(result$table, "data.frame")
})

test_that("ClustModalities ACM - illustrative_numeric() quantitative variables", {
  data_train <- generate_quali_data(n = 100, seed = 222)

  cm <- ClustModalities$new(method = "acm")
  cm$fit(data_train, k = 3)

  # Quantitative illustrative variables
  quant_vars <- data.frame(
    num1 = rnorm(100),
    num2 = rnorm(100)
  )

  cors <- suppressWarnings(cm$illustrative_numeric(quant_vars, plot = FALSE))

  expect_type(cors, "double")
  expect_equal(length(cors), 2)  # 2 variables
})

test_that("ClustModalities ACM - summary() returns correct structure", {
  data_test <- generate_quali_data(n = 80, seed = 333)

  cm <- ClustModalities$new(method = "acm")
  cm$fit(data_test, k = 3)

  results <- cm$summary(print_output = FALSE)

  expect_type(results, "list")
  expect_true("basic_stats" %in% names(results))
  expect_true("cluster_stats" %in% names(results))
  expect_true("cluster_composition" %in% names(results))
})

test_that("ClustModalities ACM - print() method works", {
  data_test <- generate_quali_data(n = 50, seed = 444)

  cm <- ClustModalities$new(method = "acm")
  cm$fit(data_test, k = 2)

  expect_output(cm$print(), "QUALITATIVE")
})

test_that("ClustModalities ACM - Visualizations execute without error", {
  data_test <- generate_quali_data(n = 80, seed = 555)

  cm <- ClustModalities$new(method = "acm")
  cm$fit(data_test, k = 3)

  # Test plots don't error
  pdf(file = tempfile(fileext = ".pdf"))
  expect_silent(cm$plot_dendrogram(k = 3))
  expect_silent(cm$plot_factorial_map(dims = c(1, 2)))
  expect_silent(cm$plot_scree())
  dev.off()
})

test_that("ClustModalities ACM - plot_contrib() method", {
  data_test <- generate_quali_data(n = 80, seed = 666)

  cm <- ClustModalities$new(method = "acm")
  cm$fit(data_test, k = 3)

  pdf(file = tempfile(fileext = ".pdf"))
  expect_silent(cm$plot_contrib(dim = 1, top = 5))
  dev.off()
})

test_that("ClustModalities DICE - Different hclust methods", {
  data_test <- generate_quali_data(n = 80, seed = 777)

  methods <- c("complete", "average", "single")

  for (method in methods) {
    cm <- ClustModalities$new(method = "dice", hclust_method = method)
    cm$fit(data_test, k = 3)

    expect_equal(cm$hclust_method, method)
    expect_false(is.null(cm$mod_clusters))
  }
})

test_that("ClustModalities ACM - Eigenvalues and variance", {
  data_test <- generate_quali_data(n = 100, seed = 888)

  cm <- ClustModalities$new(method = "acm", n_axes = 5)
  cm$fit(data_test, k = 3)

  # Check eigenvalues exist
  expect_false(is.null(cm$acm$eig))

  # Eigenvalues should be positive
  expect_true(all(cm$acm$eig[, 1] > 0))
})

test_that("ClustModalities ACM - Stability with loisirs dataset", {
  data(loisirs)

  # Take sample for speed
  set.seed(999)
  sample_idx <- sample(nrow(loisirs), min(500, nrow(loisirs)))
  loisirs_sample <- loisirs[sample_idx, ]

  cm <- ClustModalities$new(method = "acm")
  cm$fit(loisirs_sample, k = 4)

  expect_equal(cm$k, 4)
  expect_false(is.null(cm$mod_clusters))
  expect_equal(length(unique(cm$mod_clusters)), 4)
})

test_that("ClustModalities - Error: Non-factor data", {
  data_bad <- data.frame(var1 = 1:10, var2 = 11:20)
  cm <- ClustModalities$new(method = "acm")

  expect_error(cm$fit(data_bad), "qualitative")
})

test_that("ClustModalities - Error: predict() without fit()", {
  cm <- ClustModalities$new(method = "acm")
  data_new <- generate_quali_data(n = 10)

  expect_error(cm$predict(data_new), "must be called before")
})

test_that("ClustModalities - Error: Invalid method", {
  expect_error(
    ClustModalities$new(method = "invalid"),
    "method"
  )
})

test_that("ClustModalities ACM - Edge case: Minimum 2 variables", {
  data_min <- data.frame(
    var1 = factor(sample(c("A", "B"), 50, replace = TRUE)),
    var2 = factor(sample(c("X", "Y"), 50, replace = TRUE))
  )

  cm <- ClustModalities$new(method = "acm")
  expect_silent(cm$fit(data_min, k = 2))
  expect_false(is.null(cm$mod_clusters))
})

test_that("ClustModalities DICE - Edge case: k = 2", {
  data_test <- generate_quali_data(n = 60, seed = 1111)

  cm <- ClustModalities$new(method = "dice")
  cm$fit(data_test, k = 2)

  expect_equal(length(unique(cm$mod_clusters)), 2)
})

test_that("ClustModalities ACM - MCA coordinates exist", {
  data_test <- generate_quali_data(n = 80, seed = 1234)

  cm <- ClustModalities$new(method = "acm")
  cm$fit(data_test, k = 3)

  # Check modality coordinates
  expect_false(is.null(cm$mod_coords))
  expect_equal(ncol(cm$mod_coords), cm$n_axes)

  # Check individual coordinates
  expect_false(is.null(cm$acm$ind$coord))
})

test_that("ClustModalities ACM - Heights for dendrogram", {
  data_test <- generate_quali_data(n = 80, seed = 5678)

  cm <- ClustModalities$new(method = "acm")
  cm$fit(data_test, k = 3)

  # Check heights exist
  expect_false(is.null(cm$hclust$height))
  expect_true(length(cm$hclust$height) > 0)
})

test_that("ClustModalities - Real data: vote dataset full workflow", {
  data(vote)

  # ACM method
  cm_acm <- ClustModalities$new(method = "acm")
  cm_acm$fit(vote, k = 3)

  expect_equal(cm_acm$k, 3)
  expect_false(is.null(cm_acm$mod_clusters))

  # Summary
  results <- cm_acm$summary(print_output = FALSE)
  expect_type(results, "list")

  # DICE method
  cm_dice <- ClustModalities$new(method = "dice")
  cm_dice$fit(vote, k = 3)

  expect_equal(cm_dice$k, 3)
  expect_false(is.null(cm_dice$mod_clusters))
})
