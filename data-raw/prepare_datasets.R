# ==============================================================================
# data-raw/prepare_datasets.R
# Preparation of datasets bundled with the ClusteringVariables package
# ==============================================================================

library(usethis)

cat("\n=== Starting dataset preparation ===\n\n")

# ==============================================================================
# HELPER FUNCTION: Auto-detect separator
# ==============================================================================

auto_read_table <- function(file, dec = ",") {
  # Read first line to detect separator
  first_line <- readLines(file, n = 2)
  header_line <- first_line[1]

  # Count potential separators
  n_semicolon <- lengths(regmatches(header_line, gregexpr(";", header_line)))
  n_comma <- lengths(regmatches(header_line, gregexpr(",", header_line)))
  n_tab <- lengths(regmatches(header_line, gregexpr("\t", header_line)))

  # Determine most likely separator
  counts <- c(semicolon = n_semicolon, comma = n_comma, tab = n_tab)
  sep_map <- c(semicolon = ";", comma = ",", tab = "\t")

  # If using comma as decimal, can't use comma as separator
  if (dec == ",") {
    counts["comma"] <- 0
  }

  best_sep <- sep_map[which.max(counts)]

  # Read file with detected separator
  if (grepl("\\.csv$", file, ignore.case = TRUE)) {
    df <- read.csv(file, stringsAsFactors = FALSE, row.names = NULL,
                   sep = best_sep, dec = dec, check.names = FALSE)
  } else {
    df <- read.table(file, header = TRUE, stringsAsFactors = FALSE,
                     row.names = NULL, sep = best_sep, dec = dec,
                     check.names = FALSE, quote = "")
  }

  return(df)
}

# ==============================================================================
# DATASET 1: CRIME (Quantitative) → KMeansVariablesQuant
# ==============================================================================

cat("Processing: crime...\n")

crime <- auto_read_table("data-raw/crime_dataset_from_DASL.csv", dec = ",")

# Remove first column if it's an index
if (ncol(crime) > 0) {
  first_col <- tolower(names(crime)[1])
  if (first_col %in% c("x", "v1", "") || grepl("^x\\.", first_col) || first_col == "1") {
    crime <- crime[, -1, drop = FALSE]
  }
}

# Force all to numeric
for (col in names(crime)) {
  crime[[col]] <- suppressWarnings(as.numeric(as.character(crime[[col]])))
}

# Remove all-NA columns
crime <- crime[, colSums(is.na(crime)) < nrow(crime), drop = FALSE]

# Remove rows with NA
crime <- na.omit(crime)
crime <- as.data.frame(crime)

cat(sprintf("  ✓ crime: %d obs x %d vars\n", nrow(crime), ncol(crime)))


# ==============================================================================
# DATASET 2: USCRIME (Quantitative) → VarClus
# ==============================================================================

cat("Processing: uscrime...\n")

uscrime <- auto_read_table("data-raw/uscrime.txt", dec = ".")  # Point décimal (US format)

# Remove first column if it's an index
if (ncol(uscrime) > 0) {
  first_col <- tolower(names(uscrime)[1])
  if (first_col %in% c("x", "v1", "") || grepl("^x\\.", first_col) || first_col == "1") {
    uscrime <- uscrime[, -1, drop = FALSE]
  }
}

# Keep only numeric columns
numeric_cols <- sapply(uscrime, is.numeric)
if (sum(numeric_cols) > 0) {
  uscrime <- uscrime[, numeric_cols, drop = FALSE]
}

# Remove rows with NA
uscrime <- na.omit(uscrime)
uscrime <- as.data.frame(uscrime)

cat(sprintf("  ✓ uscrime: %d obs x %d vars\n", nrow(uscrime), ncol(uscrime)))


# ==============================================================================
# DATASET 3: AUTOS (Mixed) → VarClus (numeric part)
# ==============================================================================

cat("Processing: autos...\n")

autos <- auto_read_table("data-raw/autos.txt", dec = ",")

# Remove first column if it's an index
if (ncol(autos) > 0) {
  first_col <- tolower(names(autos)[1])
  if (first_col %in% c("x", "v1", "") || grepl("^x\\.", first_col) || first_col == "1") {
    autos <- autos[, -1, drop = FALSE]
  }
}

# Convert character to factor (if not convertible to numeric)
for (col in names(autos)) {
  if (is.character(autos[[col]])) {
    test_num <- suppressWarnings(as.numeric(autos[[col]]))
    if (all(is.na(test_num))) {
      # Pure character -> factor
      autos[[col]] <- as.factor(autos[[col]])
    } else {
      # Can be numeric -> convert
      autos[[col]] <- test_num
    }
  }
}

# Remove rows with NA
autos <- na.omit(autos)
autos <- as.data.frame(autos)

n_num <- sum(sapply(autos, is.numeric))
n_fac <- sum(sapply(autos, is.factor))
cat(sprintf("  ✓ autos: %d obs x %d vars (%d numeric, %d factor)\n",
            nrow(autos), ncol(autos), n_num, n_fac))


# ==============================================================================
# DATASET 4: AUTOS2005 (Mixed) → KMeansVariablesQuant (numeric part)
# ==============================================================================

cat("Processing: autos2005...\n")

autos2005 <- auto_read_table("data-raw/AUTOS2005.txt", dec = ",")

# Remove first column if it's an index
if (ncol(autos2005) > 0) {
  first_col <- tolower(names(autos2005)[1])
  if (first_col %in% c("x", "v1", "") || grepl("^x\\.", first_col) || first_col == "1") {
    autos2005 <- autos2005[, -1, drop = FALSE]
  }
}

# Convert character to factor (if not convertible to numeric)
for (col in names(autos2005)) {
  if (is.character(autos2005[[col]])) {
    test_num <- suppressWarnings(as.numeric(autos2005[[col]]))
    if (all(is.na(test_num))) {
      autos2005[[col]] <- as.factor(autos2005[[col]])
    } else {
      autos2005[[col]] <- test_num
    }
  }
}

# Remove rows with NA
autos2005 <- na.omit(autos2005)
autos2005 <- as.data.frame(autos2005)

n_num <- sum(sapply(autos2005, is.numeric))
n_fac <- sum(sapply(autos2005, is.factor))
cat(sprintf("  ✓ autos2005: %d obs x %d vars (%d numeric, %d factor)\n",
            nrow(autos2005), ncol(autos2005), n_num, n_fac))


# ==============================================================================
# DATASET 5: LOISIRS (Qualitative) → ClustModalities (ACM-CAH)
# ==============================================================================

cat("Processing: loisirs...\n")

loisirs <- auto_read_table("data-raw/loisirs.txt", dec = ",")

# Remove first column if it's an index
if (ncol(loisirs) > 0) {
  first_col <- tolower(names(loisirs)[1])
  if (first_col %in% c("x", "v1", "") || grepl("^x\\.", first_col) || first_col == "1") {
    loisirs <- loisirs[, -1, drop = FALSE]
  }
}

# Convert to factors EXCEPT last column (which is numeric)
for (i in seq_along(names(loisirs))) {
  col <- names(loisirs)[i]
  if (i == length(names(loisirs))) {
    # Last column: keep as numeric
    if (!is.numeric(loisirs[[col]])) {
      loisirs[[col]] <- suppressWarnings(as.numeric(as.character(loisirs[[col]])))
    }
  } else {
    # Other columns: convert to factor
    if (!is.factor(loisirs[[col]])) {
      loisirs[[col]] <- as.factor(loisirs[[col]])
    }
  }
}

# Remove rows with NA
loisirs <- na.omit(loisirs)
loisirs <- as.data.frame(loisirs)

cat(sprintf("  ✓ loisirs: %d obs x %d vars\n", nrow(loisirs), ncol(loisirs)))


# ==============================================================================
# DATASET 6: VOTE (Qualitative) → ClustModalities (ACM-CAH)
# ==============================================================================

cat("Processing: vote...\n")

vote <- auto_read_table("data-raw/vote_catvarclus.csv", dec = ",")

# Remove first column if it's an index
if (ncol(vote) > 0) {
  first_col <- tolower(names(vote)[1])
  if (first_col %in% c("x", "v1", "") || grepl("^x\\.", first_col) || first_col == "1") {
    vote <- vote[, -1, drop = FALSE]
  }
}

# Convert all to factors
for (col in names(vote)) {
  if (!is.factor(vote[[col]])) {
    vote[[col]] <- as.factor(vote[[col]])
  }
}

# Remove rows with NA
vote <- na.omit(vote)
vote <- as.data.frame(vote)

cat(sprintf("  ✓ vote: %d obs x %d vars\n", nrow(vote), ncol(vote)))


# ==============================================================================
# SAVE ALL DATASETS
# ==============================================================================

cat("\n=== Saving datasets to data/ ===\n\n")

usethis::use_data(
  crime,
  uscrime,
  autos,
  autos2005,
  loisirs,
  vote,
  overwrite = TRUE
)

cat("✓ All datasets saved as .rda files in data/\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n=== Dataset Summary ===\n\n")

datasets <- list(
  crime = crime,
  uscrime = uscrime,
  autos = autos,
  autos2005 = autos2005,
  loisirs = loisirs,
  vote = vote
)

for (name in names(datasets)) {
  df <- datasets[[name]]
  n_numeric <- sum(sapply(df, is.numeric))
  n_factor <- sum(sapply(df, is.factor))

  cat(sprintf("%-12s : %4d obs × %2d vars  (%d num, %d cat)\n",
              name, nrow(df), ncol(df), n_numeric, n_factor))
}

cat("\n=== Usage ===\n\n")
cat("Datasets are now available:\n")
cat("  data(crime)\n")
cat("  data(vote)\n")
cat("  ?crime\n\n")

cat("Recommended usage:\n")
cat("  crime, autos2005  → KMeansVariablesQuant\n")
cat("  autos, uscrime    → VarClus\n")
cat("  loisirs, vote     → ClustModalities (ACM-CAH)\n\n")

cat("=== Done! ===\n\n")
