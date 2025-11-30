# ==============================================================================
# R/data.R
# Documentation for datasets included in ClusteringVariables package
# ==============================================================================

#' US Crime Statistics (Quantitative)
#'
#' @description
#' Dataset containing crime statistics and demographic information for US states.
#' All variables are quantitative.
#'
#' **Tested with**: `KMeansVariablesQuant`
#'
#' @format A data frame with numeric variables
#'
#' @source Data Analysis and Statistical Learning (DASL) project
#'
#' @examples
#' \dontrun{
#' # Load dataset
#' data(crime)
#' str(crime)
#'
#' # K-means variable clustering
#' km <- KMeansVariablesQuant$new(k = 3)
#' km$fit(crime)
#' km$summary()
#' km$plot_biplot()
#' }
"crime"

#' US Crime Dataset (Quantitative)
#'
#' @description
#' US crime dataset with quantitative variables.
#'
#' **Tested with**: `VarClus`
#'
#' @format A data frame with numeric variables
#'
#' @examples
#' \dontrun{
#' # Load dataset
#' data(uscrime)
#' str(uscrime)
#'
#' # VarClus hierarchical clustering
#' vc <- VarClus$new(similarity = "pearson")
#' vc$fit(uscrime)
#' vc$summary()
#' dend_plot <- vc$get_dendrogram()
#' dend_plot()
#' }
"uscrime"

#' Automobile Characteristics (Mixed)
#'
#' @description
#' Dataset containing both quantitative and qualitative characteristics
#' of automobiles.
#'
#' **Tested with**: `VarClus` (on numeric variables)
#'
#' @format A data frame with 18 rows and 9 variables:
#' \describe{
#'   \item{Modele}{Car model name}
#'   \item{FINITION}{Finish type}
#'   \item{CYL}{Number of cylinders}
#'   \item{PUISS}{Power}
#'   \item{VMAX}{Maximum speed}
#'   \item{POIDS}{Weight}
#'   \item{LONG}{Length}
#'   \item{LARG}{Width}
#'   \item{PRIX}{Price}
#' }
#'
#' @examples
#' \dontrun{
#' # Load dataset
#' data(autos)
#' str(autos)
#'
#' # Extract numeric variables for VarClus
#' autos_num <- autos[, sapply(autos, is.numeric)]
#'
#' # Hierarchical variable clustering
#' vc <- VarClus$new()
#' vc$fit(autos_num)
#' vc$summary()
#' }
"autos"

#' Automobile Dataset 2005 (Mixed)
#'
#' @description
#' Automobile characteristics dataset from 2005.
#' Contains both quantitative and qualitative variables.
#'
#' **Tested with**: `KMeansVariablesQuant` (on numeric variables)
#'
#' @format A data frame with mixed variables
#'
#' @examples
#' \dontrun{
#' # Load dataset
#' data(autos2005)
#' str(autos2005)
#'
#' # Extract numeric variables for K-means
#' autos2005_num <- autos2005[, sapply(autos2005, is.numeric)]
#'
#' # K-means variable clustering
#' km <- KMeansVariablesQuant$new(k = 4)
#' km$fit(autos2005_num)
#' km$print()
#' km$plot_correlation_circle()
#' }
"autos2005"

#' Leisure Activities Dataset (Qualitative)
#'
#' @description
#' Dataset containing qualitative variables about leisure activities.
#' All variables are categorical.
#'
#' **Tested with**: `ClustModalities` (ACM-CAH method)
#'
#' @format A data frame with factor variables
#'
#' @examples
#' \dontrun{
#' # Load dataset
#' data(loisirs)
#' str(loisirs)
#'
#' # ACM-CAH clustering
#' cm <- ClustModalities$new(method = "acm", n_axes = 5)
#' cm$fit(loisirs, k = 3)
#' cm$summary()
#' cm$plot_factorial_map()
#' cm$plot_dendrogram()
#'
#' # DICE method
#' cm_dice <- ClustModalities$new(method = "dice")
#' cm_dice$fit(loisirs, k = 3)
#' cm_dice$plot_dendrogram()
#' }
"loisirs"

#' Voting Behavior Dataset (Qualitative)
#'
#' @description
#' Dataset containing categorical variables related to voting behavior.
#' All variables are qualitative.
#'
#' **Tested with**: `ClustModalities` (ACM-CAH method)
#'
#' @format A data frame with factor variables
#'
#' @examples
#' \dontrun{
#' # Load dataset
#' data(vote)
#' str(vote)
#'
#' # Cluster voting modalities with ACM
#' cm <- ClustModalities$new(method = "acm", n_axes = 5)
#' cm$fit(vote, k = 4)
#' cm$summary()
#' cm$plot_factorial_map()
#'
#' # Automatic k selection
#' result <- acm_cah_elbow(vote, method = "acm", k_max = 10)
#' print(result$optimal_k)
#' result$plot()
#'
#' # Use optimal k
#' cm_opt <- ClustModalities$new(method = "acm")
#' cm_opt$fit(vote, k = result$optimal_k)
#' }
"vote"
