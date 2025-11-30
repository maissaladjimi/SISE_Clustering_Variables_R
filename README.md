# 📊 ClusteringVariables: R Package for Variable Clustering & Analysis

> **This project is conducted as part of the Data Science curriculum at the University of Lyon 2, Master 2 SISE. Its main objective is to develop an R package and a Shiny application capable of performing variable clustering on any given dataset. Users can install the R package directly from GitHub or access the Shiny application to explore and test the package's functionalities.


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
  - [HAC for Qualitative Variables](#2-hac-for-qualitative-variables)
  - [VarClus for Quantitative Variables](#3-varclus-for-quantitative-variables)
- [📊Datasets included](#-datasets-included)
- [🔧Fonctionnalités avancées](#-fonctionnalités-avancées)
- [🎨 Shiny Application](#-)
- [🧪Tests](#-tests)
- [🛠️ Package structure](#-contributions)
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
# Installation de devtools si nécessaire
if (!require("devtools")) install.packages("devtools")

# Installation du package
devtools::install_github("maissaladjimi/SISE_Clustering_Variables_R", ref = "test_module")
```

### Installation locale

```r
# Depuis le répertoire du package
devtools::install()
```

### Dépendances

Le package nécessite :
- `R6`, `Hmisc`, `ade4`, `dendextend`, `plotly`, `shiny`
- Pour les applications Shiny : `shinyjs`, `shinythemes`

---

## 🎯 Algorithms

**ClusteringVariables** propose trois approches complémentaires pour le clustering de variables :

| Méthode | Type de variables | Principe | Usage |
|---------|------------------|----------|-------|
| **K-Means Variables** | Quantitatives | Partitionnement par centres mobiles | Groupes de variables corrélées |
| **ACM-CAH** | Qualitatives | ACM + Classification hiérarchique | Modalités similaires |
| **VarClus** | Quantitatives | Hiérarchique basé sur corrélations | Structure arborescente |

---

## 🚀 Guide de démarrage rapide

### 🔄1. K-Means pour variables quantitatives

**Objectif** : Regrouper des variables numériques fortement corrélées.

#### Exemple complet avec le dataset `crime`

```r
library(ClusteringVariables)

# Chargement des données
data(crime)
head(crime, 3)
#   CrimeRate Male14-24 Southern Education Expend60 Expend59 Labor ...
# 1      79.1       151        1         91       58       56   510 ...
# 2     163.5       143        0        113      103       95   583 ...
# 3      57.8       142        1         89       45       44   533 ...

# Dimensions
dim(crime)
# [1] 47 14  # 47 États × 14 variables socio-économiques
```

#### Clustering avec K-Means

**Note** : On exclut `CrimeRate` du clustering pour l'utiliser comme variable illustrative.

```r
# Séparer CrimeRate du reste
crime_vars <- crime[, -1]  # Toutes sauf CrimeRate

# Initialisation avec 4 clusters (optimal d'après l'elbow)
km <- KMeansVariablesQuant$new(k = 4, n_init = 20, seed = 42)

# Ajustement du modèle
km$fit(crime_vars)

# Résumé des résultats
km$summary()
# ========================================
#   K-MEANS CLUSTERING OF VARIABLES
# ========================================
# Number of variables: 13
# Number of clusters: 4
# Total inertia: 10.5785
# ...
```

## Interprétation

Les résultats montrent 4 clusters de variables :

- **Cluster 1** : Variables de dépenses (Expend60, Expend59)
- **Cluster 2** : Variables socio-démographiques (Education, Southern, Male14-24, etc.)
- **Cluster 3** : Variables économiques/emploi
- **Cluster 4** : Variables de population

Chaque cluster regroupe des variables fortement corrélées entre elles.

#### Visualisations

```r
# Cercle de corrélation
km$plot_correlation_circle()

# Biplot des variables
km$plot_biplot()

# Méthode du coude pour choisir k
km$plot_elbow(k_range = 2:6)
# === K-Means Elbow Analysis ===
# Optimal k: 4
```

#### Prédiction sur nouvelles variables

```r
# Prédire le cluster d'une nouvelle variable corrélée au crime
new_var <- data.frame(
  Unemployment = rnorm(nrow(crime), mean = 6, sd = 2)
)

# Prédiction
predictions <- km$predict(new_var)
# Warning: 1 variable(s) have R² < 30%: Unemployment
# These variables are poorly represented by existing clusters.

print(predictions)
#                 variable cluster r2_max distance
# Unemployment Unemployment       1  0.037   0.981

```

#### Variables illustratives

```r
# Utiliser CrimeRate comme variable illustrative
crime_rate_df <- data.frame(CrimeRate = crime$CrimeRate)

result <- km$illustrative(crime_rate_df, plot = TRUE)
print(result$table)
#           variable cluster r2_max distance
# CrimeRate CrimeRate       4  42.28   0.7597

# Interprétation : 
# - CrimeRate est le mieux représenté par le Cluster 4 (R² = 42.28%)
# - distance = 0.76 indique une corrélation modérée avec ce cluster
# - Le Cluster 4 contient probablement des variables socio-économiques liées au crime
```

---

### 🔗 2. ACM-CAH pour variables qualitatives

**Objectif** : Regrouper des modalités de variables qualitatives.

#### Exemple avec le dataset `vote`

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

#### Clustering avec ACM

**Note** : On exclut la variable `affiliation` (parti politique) pour l'utiliser comme variable illustrative et pour les prédictions.

```r
# Séparer affiliation du reste
vote_vars <- vote[, -1]  # Toutes les variables sauf affiliation

# Méthode ACM (Analyse des Correspondances Multiples)
cm <- ClustModalities$new(method = "acm", n_axes = 5)
cm$fit(vote_vars, k = 3)

# Résumé
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

#### Visualisations ACM

```r
# Dendrogramme
cm$plot_dendrogram(k = 3)

# Plan factoriel
cm$plot_factorial_map(dims = c(1, 2))

# Éboulis des valeurs propres
cm$plot_scree()

# Contribution des modalités
cm$plot_contrib(dim = 1, top = 10)
```

#### Méthode DICE (alternative)

La méthode DICE utilise l'indice de similarité de Dice au lieu de l'ACM :

```r
# Clustering basé sur l'indice de DICE
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

# Comparaison des deux méthodes
cat("\n=== Comparaison ACM vs DICE ===\n")
print("ACM : Basé sur l'analyse factorielle des correspondances")
print("DICE : Basé sur l'indice de similarité (simple, intuitif)")
```

#### Prédiction sur nouvelles observations

```r
# Exemple : utiliser 'affiliation' (parti politique) comme nouvelle variable
affiliation_df <- data.frame(affiliation = vote$affiliation)

predictions <- cm$predict(affiliation_df)
print(predictions)
#                                       modality cluster   distance
# affiliation.republican affiliation.republican       1 0.09164179
# affiliation.democrat     affiliation.democrat       3 0.19380920
```

**Note** : La méthode predict() de ACM-CAH nécessite le même nombre d'observations que l'apprentissage. Voir la documentation pour plus de détails.

#### Variables illustratives

**Variable qualitative** : Utiliser `affiliation` (parti politique) comme illustrative

```r
# Affiliation comme variable illustrative (version détaillée)
affiliation_df <- data.frame(affiliation = vote$affiliation)
result_parti <- cm$illustrative(affiliation_df, plot = TRUE)
print(result_parti$table)

# Interprétation :
# - Les modalités "democrat" et "republican" sont projetées sur les clusters
# - On voit quel cluster est le plus associé à chaque parti
# - Cela illustre les profils de vote selon l'affiliation politique
```

---

### 🌳 3. VarClus pour clustering hiérarchique

**Objectif** : Clustering hiérarchique de variables avec mesures de similarité.

#### Exemple avec le dataset `uscrime`

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

#### Clustering avec VarClus

```r
# Initialisation (similarité Pearson par défaut)
vc <- VarClus$new(similarity = "pearson", n_clusters = 4)

# Ajustement
vc$fit(uscrime)

# Résumé détaillé
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

#### Visualisations

```r
# Dendrogramme hiérarchique
dend_func <- vc$get_dendrogram()
dend_func()

# Heatmap de similarité
heatmap_func <- vc$get_heatmap()
heatmap_func()
```

#### Prédiction

```r
# Prédire le cluster d'une nouvelle variable
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

#### Variables illustratives

```r
illust_vars <- data.frame(
  GDP = rnorm(nrow(uscrime), mean = 50000, sd = 10000),
  Temperature = rnorm(nrow(uscrime), mean = 15, sd = 5)
)

result <- vc$illustrative(illust_vars)
print(result$table)
```

#### Changer le nombre de clusters

```r
# Re-découper avec un nombre différent de clusters
vc$n_clusters <- 5
vc$fit(uscrime)

vc$summary()
# Nombre de clusters: 5
```

#### Utiliser Spearman au lieu de Pearson

```r
vc_spearman <- VarClus$new(similarity = "spearman", n_clusters = 4)
vc_spearman$fit(uscrime)
```

---

## 📊 Datasets inclus

Le package inclut 6 datasets prêts à l'emploi :

| Dataset | Dimensions | Type | Description |
|---------|-----------|------|-------------|
| **crime** | 47 × 14 | Quantitatif | Statistiques de criminalité par État US |
| **uscrime** | 47 × 16 | Quantitatif | Variables socio-économiques et criminalité |
| **autos** | 18 × 9 | Mixte | Caractéristiques de véhicules (7 num, 2 cat) |
| **autos2005** | 38 × 13 | Mixte | Véhicules 2005 (9 num, 4 cat) |
| **loisirs** | 8403 × 23 | Qualitatif | Enquête pratiques de loisirs (1 num, 22 cat) |
| **vote** | 435 × 7 | Qualitatif | Votes du Congrès US 1984 |

**Accès aux datasets** :

```r
# Lister tous les datasets
data(package = "ClusteringVariables")

# Charger un dataset
data(crime)
?crime  # Voir la documentation
```

---

## 🔧 Fonctionnalités avancées

### Méthode du coude automatique

Les algorithmes proposent une détection automatique du nombre optimal de clusters (la proposition de k optimal peut être fausse, le plus fiable est de référer à l'elbow plot pour choisir k) :

```r
# K-Means
data(crime)
crime_vars <- crime[, -1]  # Exclure CrimeRate

km <- KMeansVariablesQuant$new(k = 4)
km$fit(crime_vars)
elbow_result <- km$plot_elbow(k_range = 2:8)
print(elbow_result$optimal_k)  # k optimal suggéré : 4

```

### Récupérer les clusters

```r
# K-Means et VarClus
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

# ACM-CAH
clusters_table <- cm$get_clusters_table()  
```

### Qualité du clustering

#### K-Means : Inertie et R²

```r
results <- km$summary(print_output = FALSE)

# Inertie totale
print(results$global_quality)

# R² par variable
print(results$cluster)
#   Cluster Size Inertia Avg_R2.Var1 Avg_R2.Freq  
#1       1    2  1.7459           1      0.8730   
#2       2    2  1.5136           2      0.7568   
#3       3    3  2.3843           3      0.7948   
#4       4    6  4.4051           4      0.7342   
# ...

# Corrélation entre composantes latentes
print(results$cor_latent)
```

#### VarClus : R² et PCA

```r
results <- vc$summary(print_output = FALSE)

# Qualité par cluster
print(results$cluster_quality)
#   cluster mean_R2_own
# 1       1      0.8234
# 2       2      0.7891
# ...

# Détails R² par variable
print(results$R2_details)
```
---
## 🎨 Shiny Application
---
content shiny 
---
## 🧪 Tests

Le package inclut **203 tests unitaires** couvrant toutes les fonctionnalités.

### Exécuter les tests

```r
# Tous les tests
devtools::test()

# Tests spécifiques
testthat::test_file("tests/testthat/test-kmeans.R")
testthat::test_file("tests/testthat/test-acm_cah.R")
testthat::test_file("tests/testthat/test-varclus.R")
```

### Résultats attendus

```
✔ | 68 | acm_cah
✔ | 57 | kmeans
✔ | 78 | varclus
────────────────────────────────────
[ FAIL 0 | WARN 0 | SKIP 1 | PASS 203 ]
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

## 🛠️ Package structure 


Add tree

---

## 👥 Authors

- **Maissa Lajimi** 
- **Yassine Cheniour** 
- **Lamia Hatem** 

Master 2 Data Science (SISE), University of Lyon 2

## 📄 License

This project is licensed under the MIT License.
