
#-------------
# Extract numeric variables
#-------------
get_numeric_vars = function(data) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("Data must be a data frame or numeric matrix.")
  }

  # Keep only numeric columns
  num_data <- data[, sapply(data, is.numeric), drop = FALSE]

  # Remove rows with NA
  num_data <- na.omit(num_data)
  num_data <- as.matrix(num_data)
  colnames(num_data) <- colnames(data)[sapply(data, is.numeric)]

  return(num_data)
}

