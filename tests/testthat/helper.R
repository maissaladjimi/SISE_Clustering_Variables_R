# ==============================================================================
# tests/testthat/helper.R
# Utility functions for testing
# ==============================================================================

# Generate test data with known structure
generate_grouped_data <- function(n = 100, seed = 123) {
  set.seed(seed)
  
  # Group 1: 3 correlated variables
  X1 <- rnorm(n)
  var1 <- X1 + rnorm(n, 0, 0.3)
  var2 <- X1 + rnorm(n, 0, 0.3)
  var3 <- X1 + rnorm(n, 0, 0.3)
  
  # Group 2: 2 correlated variables
  X2 <- rnorm(n)
  var4 <- X2 + rnorm(n, 0, 0.3)
  var5 <- X2 + rnorm(n, 0, 0.3)
  
  # Group 3: 3 correlated variables
  X3 <- rnorm(n)
  var6 <- X3 + rnorm(n, 0, 0.3)
  var7 <- X3 + rnorm(n, 0, 0.3)
  var8 <- X3 + rnorm(n, 0, 0.3)
  
  data.frame(var1, var2, var3, var4, var5, var6, var7, var8)
}

# Generate qualitative test data
generate_quali_data <- function(n = 100, seed = 123) {
  set.seed(seed)
  
  data.frame(
    var1 = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    var2 = factor(sample(c("X", "Y", "Z"), n, replace = TRUE)),
    var3 = factor(sample(c("1", "2", "3", "4"), n, replace = TRUE)),
    var4 = factor(sample(c("Red", "Blue", "Green"), n, replace = TRUE))
  )
}

# Check if variance explained is valid (between 0-100%)
check_variance_valid <- function(var_expl) {
  all(var_expl >= 0 & var_expl <= 100)
}

# Check if inertia is strictly increasing
check_inertia_increasing <- function(inertias) {
  diffs <- diff(inertias)
  all(diffs > 0)
}
