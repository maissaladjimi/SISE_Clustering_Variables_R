# 📊 ClusteringVariables: R Package for Variable Clustering & Analysis

> This project is conducted as part of the Data Science curriculum at the University of Lyon 2, Master 2 SISE. Its main objective is to develop an R package and a Shiny application capable of performing variable clustering on any given dataset. Users can install the R package directly from GitHub or access the Shiny application to explore and test the package's functionalities.


[![R Version](https://img.shields.io/badge/R-%E2%89%A5%204.0.0-blue)](https://www.r-project.org/)
[![Tests](https://img.shields.io/badge/tests-203%20passing-success)](#)
[![License](https://img.shields.io/badge/license-MIT-green)](LICENSE)

---
## 📑 Table of Contents

- [🔍 Overview](#-overview)
- [✨ Key Features](#-key-features)
- [📦 Installation](#-installation)
- [🚀 Quick Start Guide](#-quick-start-guide)
  - [K-Means for Quantitative Variables](#1-k-means-for-quantitative-variables)
  - [HAC for Qualitative Variables](#-2-mca-hac-for-qualitative-variables)
  - [VarClus for Quantitative Variables](#-3-varclus-for-hierarchical-clustering)
- [📊Datasets included](#-included-datasets)
- [🔧Advanced Features](#-advanced-features)
- [🎨 Shiny Application](#-shiny-application)
- [🧪Tests](#-tests)
- [📂 Package structure](#-package-structure)
- [❓ Getting Help](#-getting-help)
- [👥 Authors](#-authors)
- [📄 License](#-license)

---
## 🔍 Overview

When we talk about clustering, we're often considering grouping data points (individuals) into clusters. However, it is also valuable to study relationships between variables themselves; analyzing correlations and grouping them based on their shared characteristics. Clustering variables helps understand the underlying structure of your data by revealing which variables behave similarly, identifying redundant features, and enabling feature selection to reduce model complexity.

Therefore, we developed the `ClusteringVariables` package. It is an R package built with R6 classes that provides methods for clustering both quantitative and qualitative variables. The package offers 3 methods for classifying variables:

* 🔄 **KMeans** - Clusters quantitative variables using a reallocation algorithm that iteratively assigns variables to clusters
* 🌳 **VarClus** - Clusters quantitative variables using a divisive (top-down) hierarchical method that recursively splits variable groups
* 🔗 **HAC (Hierarchical Agglomerative Clustering)** - Designed for qualitative variables, it uses an agglomerative (bottom-up) hierarchical approach that progressively merges similar variables, we use their modalities to cluster the variables.

## ✨ Key Features

- 🎯 **Three Specialized Algorithms** - KMeans, VarClus, and HAC for different data types
- 📈 **Rich Visualizations** - Dendrograms, heatmaps, elbow plots, correlation circles, etc.
- 🔮 **Predictive Capability** - Assign new variables to existing clusters
- 🎨 **Interactive Shiny App** - User-friendly interface for data exploration
- 📦 **Clean API** - R6 class-based architecture with unified interface
- 🚀 **Automatic Selection** - Built-in methods for optimal cluster determination
- 📊 **Comprehensive Results** - Detailed summaries and cluster statistics

## 📦 Installation

### From GitHub

```r
# Install devtools if needed
if (!require("devtools")) install.packages("devtools")

# Install the package
devtools::install_github("maissaladjimi/SISE_Clustering_Variables_R")
```

### Local installation

```r
# From the package directory
devtools::install()
```

### Dependencies

The package requires:
- `R6`, `Hmisc`, `ade4`, `dendextend`, `plotly`, `shiny`
- For shiny applications: `shinyjs`, `shinythemes`

---

## 🎯 Algorithms

**ClusteringVariables** offers three complementary approaches for variable clustering :

| Method | Variable Type | Principle | Use |
|---------|------------------|----------|-------|
| **K-Means Variables** | Quantitative | Partitioning based on moving centroids | Groups of correlated variables |
| **MCA-HAC** | Qualitative | MCA + Hierarchical clustering | Similar modalities |
| **VarClus** | Quantitative | Hierarchical based on correlations | Tree-like structure |

---

## 🚀 Quick Start Guide

### 🔄1. K-Means for Quantitative Variables

**Objective** : Group numerical variables that are highly correlated.

#### Complete example with the `crime` dataset

```r
library(ClusteringVariables)

# loading data
data(crime)
head(crime, 3)
#   CrimeRate Male14-24 Southern Education Expend60 Expend59 Labor ...
# 1      79.1       151        1         91       58       56   510 ...
# 2     163.5       143        0        113      103       95   583 ...
# 3      57.8       142        1         89       45       44   533 ...

# Dimensions
dim(crime)
# [1] 47 14  # 47 États × 14 socio-economic variables
```

#### Clustering with K-Means

**Note** : `CrimeRate` is excluded from clustering to use as an illustrative variable.

```r
# Separate CrimeRate from the rest of the dataset
crime_vars <- crime[, -1]  # All except CrimeRate

# Initialize with 4 clusters (optimal according to elbow method)
km <- KMeansVariablesQuant$new(k = 4, n_init = 20, seed = 42)

# Fit the model
km$fit(crime_vars)

# Summary of results
km$summary()
# ========================================
#   K-MEANS CLUSTERING OF VARIABLES
# ========================================
# Number of variables: 13
# Number of clusters: 4
# Total inertia: 10.5785
# ...
```

## Interpretation

The results show 4 clusters of variables :

- **Cluster 1** : Expenditure variables (Expend60, Expend59)
- **Cluster 2** : Socio-demographic variables (Education, Southern, Male14-24, etc.)
- **Cluster 3** : Economic/employment variables
- **Cluster 4** : Population variables

Each cluster groups variables that are highly correlated with each other.

#### Visualizations

```r
# Correlation circle
km$plot_correlation_circle()

# Biplot of variables
km$plot_biplot()

# Elbow method to choose k
km$plot_elbow(k_range = 2:6)
# === K-Means Elbow Analysis ===
# Optimal k: 4
```

#### Prediction on New Variables

```r
# Predict the cluster of a new variable correlated with crime
new_var <- data.frame(
  Unemployment = rnorm(nrow(crime), mean = 6, sd = 2)
)

# Prediction
predictions <- km$predict(new_var)
# Warning: 1 variable(s) have R² < 30%: Unemployment
# These variables are poorly represented by existing clusters.

print(predictions)
#                 variable cluster r2_max distance
# Unemployment Unemployment       1  0.037   0.981

```

#### Illustrative Variables

```r
# Use CrimeRate as an illustrative variable
crime_rate_df <- data.frame(CrimeRate = crime$CrimeRate)

result <- km$illustrative(crime_rate_df, plot = TRUE)
print(result$table)
#           variable cluster r2_max distance
# CrimeRate CrimeRate       4  42.28   0.7597

# Interpretation : 
# - CrimeRate is best represented by Cluster 4 (R² = 42.28%)
# - distance = 0.76 indicates a moderate correlation with this cluster
# - Cluster 4 likely contains socio-economic variables related to crime
```

---

### 🔗 2. MCA-HAC for Qualitative Variables

**Objective** : Group modalities of qualitative variables.

#### Example with the `vote` dataset

```r
data(vote)
head(vote, 3)
#   affiliation budget physician salvador nicaraguan missile education
# 1  republican      n         y        y          n       n         y
# 2  republican      n         y        y          n       n         y
# 3    democrat      y   neither        y          n       n         n

# Structure
str(vote)
# 'data.frame': 435 obs. of 7 variables:
#  $ affiliation: Factor w/ 2 levels "democrat","republican": 2 2 1 ...
#  $ budget     : Factor w/ 3 levels "n","neither","y": 1 1 3 ...
```

#### Clustering with MCA

**Note** : The variable `affiliation` (political party) is excluded to use as an illustrative variable and for predictions.

```r
# Separate affiliation from the rest
vote_vars <- vote[, -1]  # All variables except affiliation

# MCA method (Multiple Correspondence Analysis)
cm <- ClustModalities$new(method = "acm", n_axes = 5)
cm$fit(vote_vars, k = 3)

# Summary
cm$summary()
# ========================================
# CLUSTERING OF QUALITATIVE MODALITIES
# ========================================
#   Method: ACM
#
# Data:
# - Observations              : 435
# - Categorical variables     : 6
# - Total modalities          : 18
#
# Clustering:
# - Number of clusters (k)    : 3
#
# Cluster sizes:
# - Cluster 1                 : 6 modalities
# - Cluster 2                 : 6 modalities
# - Cluster 3                 : 6 modalities
```

#### MCA Visualizations

```r
# Dendrogramme
cm$plot_dendrogram(k = 3)

# Factorial map
cm$plot_factorial_map(dims = c(1, 2))

# Scree plot
cm$plot_scree()

# Contribution of modalities
cm$plot_contrib(dim = 1, top = 10)
```

#### DICE Method (Alternative)

The DICE method uses the Dice similarity index instead of MCA :

```r
# Clustering based on the Dice index
cm_dice <- ClustModalities$new(method = "dice")
cm_dice$fit(vote_vars, k = 3)

cm_dice$summary()
# ========================================
#   QUALITATIVE VARIABLES CLUSTERING
# ========================================
# Method: DICE + CAH
# Number of modalities: 18
# Number of clusters: 3

# Dendrogramme
cm_dice$plot_dendrogram(k = 3)

# Comparison of the two methods
cat("\n=== Comparison MCA vs DICE ===\n")
print("MCA: Based on Multiple Correspondence Analysis")
print("DICE: Based on similarity index (simple, intuitive)")
```

#### Prediction on New Variables

```r
# Example: using 'affiliation' (political party) as a new variable
affiliation_df <- data.frame(affiliation = vote$affiliation)

predictions <- cm$predict(affiliation_df)
print(predictions)
#                                       modality cluster   distance
# affiliation.republican affiliation.republican       1 0.09164179
# affiliation.democrat     affiliation.democrat       3 0.19380920
```

**Note** : the predict() method of MCA-HAC requires the same number of observations as the training data. See the documentation for details.

#### Illustrative Variables

**Qualitative variable** : Use `affiliation` (political party) as illustrative

```r
# Affiliation as an illustrative variable (detailed version)
affiliation_df <- data.frame(affiliation = vote$affiliation)
result_parti <- cm$illustrative(affiliation_df, plot = TRUE)
print(result_parti$table)

# Interpretation :
# - The modalities "democrat" and "republican" are projected onto the clusters
# - We can see which cluster is most associated with each party
# - This illustrates voting profiles according to political affiliation
```

---

### 🌳 3. VarClus for Hierarchical Clustering

**Objective** : Hierarchical clustering of variables using similarity measures.

#### Exemple with the `uscrime` dataset

```r
data(uscrime)
head(uscrime, 3)
#      M So   Ed  Po1  Po2    LF  M.F Pop   NW    U1   U2 Wealth Ineq   Prob  Time Crime
# 1 15.1  1  9.1  5.8  5.6 0.510 95.0  33 30.1 0.108  4.1   3940 26.1 0.0846 26.20   791
# 2 14.3  0 11.3 10.3  9.5 0.583 101.2 13 10.2 0.096  3.6   5570 19.4 0.0296 25.30  1635
# 3 14.2  1  8.9  4.5  4.4 0.533 96.9  18 21.9 0.094  3.3   3180 25.0 0.0834 24.30   578

dim(uscrime)
# [1] 47 16  # 47 États × 16 variables socio-économiques
```

#### Clustering with VarClus

```r
# Initialization (Pearson similarity by default)
vc <- VarClus$new(similarity = "pearson", n_clusters = 4)

# Fit the model
vc$fit(uscrime)

# Detailed summary
vc$summary()
# ========================================
# VARCLUS - VARIABLE CLUSTERING
# ========================================
# Similarity: pearson

# Status: Model fitted

# Data:
# - Number of variables       : 16
# - Number of clusters        : 4

# Cluster sizes:
# - Cluster 1                 : 6 variables
# - Cluster 2                 : 4 variables
# - Cluster 3                 : 4 variables
# - Cluster 4                 : 2 variables
```

#### Visualizations

```r
# Hierarchical dendrogram
dend_func <- vc$get_dendrogram()
dend_func()

# Similarity heatmap
heatmap_func <- vc$get_heatmap()
heatmap_func()
```

#### Prediction

```r
# Predict the cluster of a new variable
new_var <- rnorm(nrow(uscrime))
prediction <- vc$predict(new_var)

print(prediction)
# $predicted_cluster
# [1] 4
# 
# $cluster_similarity
#    1     2     3     4 
# 0.143 0.095 0.222 0.409 
#
#  $var_corr
#     variable correlation
# U1       U1       0.327
# U2       U2       0.490
# ...
```

#### Illustrative Variables

```r
illust_vars <- data.frame(
  GDP = rnorm(nrow(uscrime), mean = 50000, sd = 10000),
  Temperature = rnorm(nrow(uscrime), mean = 15, sd = 5)
)

result <- vc$illustrative(illust_vars)
print(result$table)
```

#### Changing the Number of Clusters

```r
# Re-cluster with a different number of clusters
vc$n_clusters <- 5
vc$fit(uscrime)

vc$summary()
# Number of clusters: 5
```

#### Using Spearman Instead of Pearson

```r
vc_spearman <- VarClus$new(similarity = "spearman", n_clusters = 4)
vc_spearman$fit(uscrime)
```

---

## 📊 Included Datasets

The package includes 6 ready-to-use datasets:

| Dataset | Dimensions | Type | Description |
|---------|-----------|------|-------------|
| **crime** | 47 × 14 | Quantitative | US state crime statistics |
| **uscrime** | 47 × 16 | Quantitative | Socio-economic variables and crime |
| **autos** | 18 × 9 | Mixed | Vehicle characteristics (7 num, 2 cat) |
| **autos2005** | 38 × 13 | Mixed | Vehicles 2005 (9 num, 4 cat) |
| **loisirs** | 8403 × 23 | Qualitative | Leisure activity survey (1 num, 22 cat) |
| **vote** | 435 × 7 | Qualitative | 1984 US Congressional votes |

**Accessing the datasets:** :

```r
# List all datasets
data(package = "ClusteringVariables")

# Load a dataset
data(crime)
?crime  # View documentation
```

---

## 🔧 Advanced Features

### Automatic Elbow Method

The algorithms provide automatic detection of the optimal number of clusters (the suggested k may be inaccurate; it is recommended to refer to the elbow plot to choose k):

```r
# K-Means
data(crime)
crime_vars <- crime[, -1]  # Exclude CrimeRate

km <- KMeansVariablesQuant$new(k = 4)
km$fit(crime_vars)
elbow_result <- km$plot_elbow(k_range = 2:8)
print(elbow_result$optimal_k)  # Suggested optimal k: 4

```

### Retrieving Clusters

```r
# K-Means & VarClus
clusters_table <- km$get_clusters_table()
print(clusters_table)
#       variable cluster
# 10  Unemp14-24       1
# 11  Unemp35-39       1
# 6        Labor       2
# 7         Male       2
# 5     Expend59       3
# 4     Expend60       3
# 8      PopSize       3
# 3    Education       4

# MCA-HAC
clusters_table <- cm$get_clusters_table()  
```

### Clustering Quality

#### K-Means : Inertia and R²

```r
results <- km$summary(print_output = FALSE)

# Total inertia
print(results$global_quality)

# R² per variable
print(results$cluster)
#   Cluster Size Inertia Avg_R2.Var1 Avg_R2.Freq  
#1       1    2  1.7459           1      0.8730   
#2       2    2  1.5136           2      0.7568   
#3       3    3  2.3843           3      0.7948   
#4       4    6  4.4051           4      0.7342   
# ...

# Correlation between latent components
print(results$cor_latent)
```

#### VarClus : R² and PCA

```r
results <- vc$summary(print_output = FALSE)

# Cluster quality
print(results$cluster_quality)
#   cluster mean_R2_own
# 1       1      0.8234
# 2       2      0.7891
# ...

# R² details per variable
print(results$R2_details)
```
---
## 🎨 Shiny Application

An interactive web application is included to explore clustering methods without coding.
```r
# Navigate to the shiny directory
setwd("path/to/SISE_Clustering_Variables_R/inst/shiny")

# Run the application
shiny::runApp()
```

---

### 📸 Application Screenshots

#### 🏠 Home Page
Welcome interface introducing the three clustering methods (K-Means, VarClus, ACM-CAH) and their applications for variable clustering.

<img width="600" alt="image" src="https://github.com/user-attachments/assets/b0f2aa9d-2879-430d-857c-895bc7fcb284" />


#### 📁 Data Import
Upload your datasets (TXT,CSV, XLSX, XLS) with automatic preview, variable type detection, and data summary statistics.

<img width="600" alt="image" src="https://github.com/user-attachments/assets/8ca067bb-dee4-4ceb-870e-06da4c3b4646" />

> Each of the following algorithms has 4 sections in which you can navigate : Summary, Visualizations, Detailed Stats, Illustratrive Variables 
#### 📊 K-Means Clustering
Perform K-Means clustering on quantitative variables with interactive visualizations including correlation circles, biplots, and elbow plots for optimal k selection.

<img width="600" alt="image" src="https://github.com/user-attachments/assets/a3882d7e-0f40-40ec-ab5e-8388052d5397" />


#### 🎯 ACM-CAH Clustering
Cluster qualitative variables using Multiple Correspondence Analysis (or Dice) combined with Hierarchical Clustering. Visualize dendrograms, factorial maps, and contribution plots.

<img width="600" alt="image" src="https://github.com/user-attachments/assets/c793260f-5b98-4c49-b63f-ecab59fbfdd9" />



#### 🌳 VarClus Clustering
Hierarchical divisive clustering for quantitative variables with dendrograms, heatmaps, and automatic optimal cluster detection.

<img width="600" alt="image" src="https://github.com/user-attachments/assets/9626d258-8ad2-4143-b787-902372173758" />

---

### 🎥 Video Demo

Watch a complete demonstration of the application:

**[📺 Application Demo Video](https://youtu.be/4LUbIMIcrc8)**

 
---
## 🧪 Tests

The package includes **203 unit tests** covering all functionalities.

### Running the Tests

```r
# Run all tests
devtools::test()

# Run specific tests
testthat::test_file("tests/testthat/test-kmeans.R")
testthat::test_file("tests/testthat/test-acm_cah.R")
testthat::test_file("tests/testthat/test-varclus.R")
```

### Expected Results

```
✔ | 68 | acm_cah
✔ | 57 | kmeans
✔ | 78 | varclus
────────────────────────────────────
[ FAIL 0 | WARN 0 | SKIP 1 | PASS 203 ]
```

---

## 📂 Package structure 



```
ClusteringVariables/
├── R/                          # R source code
│   ├── clusterengine.R        # Unified wrapper class
│   ├── kmeans.R               # K-Means algorithm (R6)
│   ├── varclus.R              # VarClus algorithm (R6)
│   ├── acm_cah.R              # ACM-CAH algorithm (R6)
│   ├── n_clusters.R           # Elbow methods for optimal k
│   ├── utils.R                # Utility functions
│   ├── data.R                 # Dataset documentation
│   └── run_app.R              # Shiny app launcher
├── data/                       # Built-in datasets (.rda)
├── data-raw/                   # Raw data and preparation scripts
│   └── prepare_datasets.R
├── inst/                       # Installed files
│   └── shiny/                 # Shiny application
│       ├── ui.R               # User interface
│       ├── server.R           # Server logic
│       ├── modules/           # Modular components
│       │   ├── kmeans_module.R
│       │   ├── acm_cah_module.R
│       │   └── varclus_module.R
│       └── www/               # Static assets (images, CSS)
├── man/                        # Documentation (auto-generated)
├── tests/                      # Unit tests (testthat)
├── DESCRIPTION                 # Package metadata
├── NAMESPACE                   # Exports (auto-generated)
├── LICENSE                     # MIT License
└── README.md                   # This file
```

---

## ❓ Getting Help

### Function help 

```r
# Help for a class 
?KMeansVariablesQuant
?ClustModalities
?VarClus

# Help for a dataset
?crime
?vote
?uscrime
```

---

## 👥 Authors

- **Maissa Lajimi** 
- **Yassine Cheniour** 
- **Lamia Hatem** 

Master 2 Data Science (SISE), University of Lyon 2

## 📄 License

This project is licensed under the MIT License.
