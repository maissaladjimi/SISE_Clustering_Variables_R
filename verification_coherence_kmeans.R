# =============================================================================
# VERIFICATION DE COHERENCE : KMeansVariablesQuant R6 ↔ Module Shiny
#
# Ce script vérifie que TOUTES les méthodes et champs appelés dans le module
# Shiny existent réellement dans la classe R6.
#
# Usage: source("verification_coherence_kmeans.R")
# =============================================================================

library(R6)

# Charger la classe R6
source("R/kmeans.R")

cat("\n")
cat("========================================================================\n")
cat("  VERIFICATION DE COHERENCE : K-means R6 ↔ Module Shiny\n")
cat("========================================================================\n\n")

# =============================================================================
# ÉTAPE 1: Créer une instance de test
# =============================================================================
cat("📦 Création d'une instance de test...\n")

# Données de test
set.seed(42)
df_test <- data.frame(
  var1 = rnorm(100),
  var2 = rnorm(100),
  var3 = rnorm(100),
  var4 = rnorm(100),
  var5 = rnorm(100)
)

km <- KMeansVariablesQuant$new(k = 3, seed = 42)
km$fit(df_test)

cat("✅ Instance créée et modèle ajusté\n\n")

# =============================================================================
# ÉTAPE 2: Vérifier les méthodes publiques
# =============================================================================
cat("🔍 Vérification des méthodes publiques appelées dans le module Shiny...\n\n")

methodes_requises <- list(
  list(nom = "$print()",
       test = function() { capture.output(km$print()); TRUE }),

  list(nom = "$elbow(k_range, n_init, plot)",
       test = function() {
         res <- km$elbow(k_range = 2:5, n_init = 5, plot = FALSE)
         !is.null(res$optimal_k) && !is.null(res$plot)
       }),

  list(nom = "$plot_correlation_circle()",
       test = function() {
         pdf(NULL); km$plot_correlation_circle(); dev.off(); TRUE
       }),

  list(nom = "$plot_cluster_centers()",
       test = function() {
         pdf(NULL); km$plot_cluster_centers(); dev.off(); TRUE
       }),

  list(nom = "$predict(X_new)",
       test = function() {
         X_new <- data.frame(var_new = rnorm(100))
         res <- km$predict(X_new)
         is.data.frame(res) && all(c("variable", "cluster", "r2_max") %in% colnames(res))
       }),

  list(nom = "$summary(print_summary = FALSE)",
       test = function() {
         res <- km$summary(print_summary = FALSE)
         !is.null(res$cluster_members) && !is.null(res$cluster_summary)
       }),

  list(nom = "$get_clusters_table()",
       test = function() {
         res <- km$get_clusters_table()
         is.data.frame(res) && all(c("variable", "cluster") %in% colnames(res))
       })
)

resultats_methodes <- sapply(methodes_requises, function(m) {
  tryCatch({
    success <- m$test()
    if (success) {
      cat(sprintf("  ✅ %-45s : OK\n", m$nom))
      TRUE
    } else {
      cat(sprintf("  ❌ %-45s : ECHEC (retour invalide)\n", m$nom))
      FALSE
    }
  }, error = function(e) {
    cat(sprintf("  ❌ %-45s : ERREUR\n", m$nom))
    cat(sprintf("     → %s\n", conditionMessage(e)))
    FALSE
  })
})

cat("\n")

# =============================================================================
# ÉTAPE 3: Vérifier les champs publics
# =============================================================================
cat("🔍 Vérification des champs publics utilisés dans le module Shiny...\n\n")

champs_requis <- list(
  list(nom = "$data",
       test = function() !is.null(km$data) && is.data.frame(km$data)),

  list(nom = "$X_scaled",
       test = function() !is.null(km$X_scaled) && is.matrix(km$X_scaled)),

  list(nom = "$k",
       test = function() !is.null(km$k) && is.numeric(km$k)),

  list(nom = "$clusters",
       test = function() !is.null(km$clusters) && is.integer(km$clusters)),

  list(nom = "$centers",
       test = function() !is.null(km$centers) && is.matrix(km$centers)),

  list(nom = "$r2_matrix",
       test = function() !is.null(km$r2_matrix) && is.matrix(km$r2_matrix)),

  list(nom = "$inertia_total",
       test = function() !is.null(km$inertia_total) && is.numeric(km$inertia_total)),

  list(nom = "$inertia_by_cluster",
       test = function() !is.null(km$inertia_by_cluster) && is.numeric(km$inertia_by_cluster)),

  list(nom = "$n_iter",
       test = function() !is.null(km$n_iter) && is.numeric(km$n_iter)),

  list(nom = "$max_iter",
       test = function() !is.null(km$max_iter) && is.numeric(km$max_iter)),

  list(nom = "$n_init",
       test = function() !is.null(km$n_init) && is.numeric(km$n_init))
)

resultats_champs <- sapply(champs_requis, function(c) {
  tryCatch({
    success <- c$test()
    if (success) {
      cat(sprintf("  ✅ %-35s : OK\n", c$nom))
      TRUE
    } else {
      cat(sprintf("  ❌ %-35s : ECHEC (NULL ou type invalide)\n", c$nom))
      FALSE
    }
  }, error = function(e) {
    cat(sprintf("  ❌ %-35s : ERREUR\n", c$nom))
    cat(sprintf("     → %s\n", conditionMessage(e)))
    FALSE
  })
})

cat("\n")

# =============================================================================
# ÉTAPE 4: Vérifier les formats de sortie
# =============================================================================
cat("🔍 Vérification des formats de sortie attendus par Shiny...\n\n")

formats_requis <- list(
  list(nom = "elbow()$optimal_k",
       test = function() {
         res <- km$elbow(k_range = 2:5, n_init = 5, plot = FALSE)
         is.numeric(res$optimal_k) && res$optimal_k >= 2
       }),

  list(nom = "elbow()$results (data.frame)",
       test = function() {
         res <- km$elbow(k_range = 2:5, n_init = 5, plot = FALSE)
         is.data.frame(res$results) && all(c("k", "inertia") %in% colnames(res$results))
       }),

  list(nom = "elbow()$plot (function)",
       test = function() {
         res <- km$elbow(k_range = 2:5, n_init = 5, plot = FALSE)
         is.function(res$plot)
       }),

  list(nom = "predict() → data.frame avec 4 colonnes",
       test = function() {
         X_new <- data.frame(v1 = rnorm(100), v2 = rnorm(100))
         res <- km$predict(X_new)
         is.data.frame(res) && ncol(res) == 4
       }),

  list(nom = "summary()$cluster_members",
       test = function() {
         res <- km$summary(print_summary = FALSE)
         is.data.frame(res$cluster_members) &&
           all(c("cluster", "variable", "R2_own_pct", "R2_next_pct", "ratio") %in% colnames(res$cluster_members))
       }),

  list(nom = "summary()$cluster_summary",
       test = function() {
         res <- km$summary(print_summary = FALSE)
         is.data.frame(res$cluster_summary) &&
           all(c("cluster", "n_variables", "lambda", "variance_explained_pct") %in% colnames(res$cluster_summary))
       }),

  list(nom = "get_clusters_table() → data.frame trié",
       test = function() {
         res <- km$get_clusters_table()
         is.data.frame(res) &&
           all(c("variable", "cluster") %in% colnames(res)) &&
           is.character(res$variable) &&
           is.integer(res$cluster)
       }),

  list(nom = "inertia_by_cluster → vecteur numérique",
       test = function() {
         is.numeric(km$inertia_by_cluster) && length(km$inertia_by_cluster) == km$k
       })
)

resultats_formats <- sapply(formats_requis, function(f) {
  tryCatch({
    success <- f$test()
    if (success) {
      cat(sprintf("  ✅ %-50s : OK\n", f$nom))
      TRUE
    } else {
      cat(sprintf("  ❌ %-50s : ECHEC\n", f$nom))
      FALSE
    }
  }, error = function(e) {
    cat(sprintf("  ❌ %-50s : ERREUR\n", f$nom))
    cat(sprintf("     → %s\n", conditionMessage(e)))
    FALSE
  })
})

cat("\n")

# =============================================================================
# RAPPORT FINAL
# =============================================================================
cat("========================================================================\n")
cat("  RAPPORT FINAL\n")
cat("========================================================================\n\n")

total_tests <- length(resultats_methodes) + length(resultats_champs) + length(resultats_formats)
tests_reussis <- sum(resultats_methodes) + sum(resultats_champs) + sum(resultats_formats)

cat(sprintf("Méthodes publiques    : %d/%d ✅\n", sum(resultats_methodes), length(resultats_methodes)))
cat(sprintf("Champs publics        : %d/%d ✅\n", sum(resultats_champs), length(resultats_champs)))
cat(sprintf("Formats de sortie     : %d/%d ✅\n", sum(resultats_formats), length(resultats_formats)))
cat(sprintf("\nTOTAL                 : %d/%d tests réussis\n", tests_reussis, total_tests))

taux_reussite <- (tests_reussis / total_tests) * 100

cat("\n")
if (taux_reussite == 100) {
  cat("🎉 PARFAIT ! Le module Shiny est 100% cohérent avec la classe R6 !\n")
  cat("✅ Vous pouvez intégrer le module sans problème.\n")
} else if (taux_reussite >= 90) {
  cat("✅ TRÈS BON ! Le module Shiny est cohérent à", round(taux_reussite), "%\n")
  cat("⚠️  Quelques ajustements mineurs nécessaires.\n")
} else if (taux_reussite >= 70) {
  cat("⚠️  ATTENTION ! Le module Shiny est cohérent à", round(taux_reussite), "%\n")
  cat("❌ Des corrections sont nécessaires avant intégration.\n")
} else {
  cat("❌ PROBLÈME MAJEUR ! Le module Shiny est cohérent à seulement", round(taux_reussite), "%\n")
  cat("🔧 Révision complète nécessaire.\n")
}

cat("\n========================================================================\n\n")

# Retourner le rapport
invisible(list(
  taux_reussite = taux_reussite,
  tests_reussis = tests_reussis,
  total_tests = total_tests,
  details = list(
    methodes = resultats_methodes,
    champs = resultats_champs,
    formats = resultats_formats
  )
))
