#' Compute Weights for K-means-Based Membership Assignment (Internal)
#'
#' This internal helper function converts hard K-means cluster assignments
#' into a simple weight matrix. Each element of the input vector
#' represents the cluster index assigned to a model. The function computes
#' weights based on cluster frequencies, assigning each model a weight
#' inversely proportional to the size of its cluster.
#'
#' @description
#' For a cluster assignment vector \code{clusters}, the weight for model \code{i}
#' is defined as:
#'
#' \deqn{ w_i = 1 / (\text{size of cluster assigned to } i) }
#'
#' This results in:
#' \itemize{
#'   \item Models in large clusters receive smaller weights.
#'   \item Models in small clusters receive larger weights.
#' }
#'
#' A membership matrix with dimensions \code{n × k} is produced, where:
#' \itemize{
#'   \item \code{n}: number of models
#'   \item \code{k}: number of clusters
#' }
#'
#' For row \code{i} and cluster \code{clusters[i]}, the weight matrix entry
#' \code{[i, clusters[i]]} equals \code{w_i}. All other entries in row \code{i}
#' are zero.
#'
#' @param clusters Integer vector containing the K-means cluster assignments for
#'        each model. Values must be in \code{1:k}, where \code{k} is the number
#'        of clusters.
#'
#' @return A numeric matrix of size \code{n × k} containing membership weights
#'         derived from cluster frequencies. Row names are preserved if the input
#'         vector has names.
#'
#' @details
#' This function is intended to provide weights for Meta Fuzzy Function.
#' Unlike fuzzy methods, weights do not sum to 1 across each row;
#' they directly reflect inverse cluster frequencies.
#'
#' @keywords internal
#' @noRd
.weight_kmeans <- function(clusters) {
  k <- max(clusters)
  n <- length(clusters)

  # Hızlı frekans sayımı için tabulate kullanımı
  cluster_sizes <- tabulate(clusters, nbins = k)

  # Ağırlık hesabı: Küme boyutuyla ters orantılı (1/size)
  weights <- 1 / cluster_sizes[clusters]

  # Seyrek matris benzeri yapı oluşturma
  weight_matrix <- matrix(0, nrow = n, ncol = k)
  weight_matrix[cbind(seq_len(n), clusters)] <- weights

  if (!is.null(names(clusters))) {
    rownames(weight_matrix) <- names(clusters)
  }

  return(weight_matrix)
}

.mff_clustering_space <- function(x_transposed, stand, reduction) {
  processed <- x_transposed
  if (isTRUE(stand)) {
    centers <- colMeans(processed)
    scales <- apply(processed, 2L, stats::sd)
    scales[!is.finite(scales) | scales <= 0] <- 1
    processed <- sweep(processed, 2L, centers, FUN = "-")
    processed <- sweep(processed, 2L, scales, FUN = "/")
  }

  if (reduction == "none") {
    return(list(
      data = processed,
      metadata = list(
        method = "none",
        original_dimensions = ncol(processed),
        retained_dimensions = ncol(processed),
        distance_preserving = TRUE
      )
    ))
  }

  centered <- sweep(processed, 2L, colMeans(processed), FUN = "-")
  decomposition <- base::svd(centered, nu = min(dim(centered)), nv = 0L)
  if (!length(decomposition$d) || decomposition$d[[1L]] <= 0) {
    stop("PCA reduction failed because all candidate profiles are identical.",
         call. = FALSE)
  }
  tolerance <- max(dim(centered)) * decomposition$d[[1L]] *
    .Machine$double.eps
  rank <- sum(decomposition$d > tolerance)
  scores <- sweep(
    decomposition$u[, seq_len(rank), drop = FALSE],
    2L, decomposition$d[seq_len(rank)], FUN = "*"
  )
  rownames(scores) <- rownames(x_transposed)

  list(
    data = scores,
    metadata = list(
      method = "pca",
      original_dimensions = ncol(processed),
      retained_dimensions = rank,
      distance_preserving = TRUE,
      tolerance = tolerance
    )
  )
}
