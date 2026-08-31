#' Hyperparameter Search for Meta-Fuzzy Function
#'
#' @description
#' The \emph{tune.mff} function performs hyperparameter optimization via grid search for Meta Fuzzy Functions
#' (MFFs) by searching over clustering-related parameter combinations and selecting the configuration that
#' yields the lowest validation error.
#'
#' @param x A numeric matrix of base-model predictions with dimensions \eqn{N_{test} \times M}. Each
#' column corresponds to a base learner.
#' @param y numeric vector of validation targets. This vector is used to evaluate meta fuzzy function
#' predictions.
#' @param max_c An integer specifying the maximum number of clusters to be considered in the
#' search.
#' @param m_seq A numeric vector of candidate values for the fuzziness exponent m used in FCM-
#' type methods.
#' @param eta_seq A numeric vector of candidate values for the probabilistic regularization parameter
#' \eqn{\eta}{eta} used when mff.method = "pfcm".
#' @param iter.max An integer specifying the maximum number of iterations allowed for the clustering
#' algorithm within each grid evaluation..
#' @param nstart integer; An integer controlling the number of random initializations for k-means
#' when mff.method = "kmeans".
#' @param seed An integer used to set the random seed for reproducibility during weight computation
#' and parameter search.
#' @param mff.method A character string selecting the membership-generation method.
#' @param eval.method A character string specifying the metric used to select the best-performing
#' meta fuzzy function.
#' @param stand Logical; if \code{TRUE}, the transposed data is standardized
#' (mean=0, sd=1) before clustering to ensure scale-invariance between models.
#' @param parallel Logical; if \code{TRUE}, the grid search process is executed in parallel
#' to accelerate hyperparameter optimization.
#' @param num_cores An integer specifying the number of CPU cores to utilize for
#' parallel processing. If \code{NULL} (default), the function automatically detects
#' and uses all available cores minus one (\code{parallel::detectCores() - 1}).
#' @param logging A logical flag indicating whether progress information is printed during the search.
#' @param clustering.reduction Character string passed to \code{mff()} selecting
#' \code{"none"} or the distance-preserving \code{"pca"} representation.
#'
#' @details
#' Given a matrix of base-model predictions and the corresponding validation targets, \emph{tune.mff}
#' repeatedly calls \emph{mff} to compute membership weights, generate meta fuzzy function
#' predictions, and evaluate these predictions using a user-specified metric. The best
#' configuration is determined by the minimum value of the selected evaluation metric among
#' the scores obtained from the meta fuzzy function predictions produced under each candidate
#' setting.
#'
#' The search space depends on the selected membership-generation method. For classical Fuzzy
#' C-Means ("fcm"), the function explores combinations of the number of clusters c and the fuzziness index m. For possibilistic FCM ("pfcm"), the
#' grid additionally includes the possibilistic regularization parameter \eqn{\eta}{eta}. For k-means
#' ("kmeans"), the search is performed only over the number of clusters(c). The function returns
#' the best-performing configuration together with the corresponding weight structure, the index
#' of the best-performing meta fuzzy function, and the full set of evaluation results, enabling
#' transparent reporting and reproducible model selection.
#'
#' @return
#' \itemize{
#'   \item \code{algorithm}: The selected membership-generation method.
#'   \item \code{eval.method}: The evaluation metric used in model selection.
#'   \item \code{weights}: The membership (weight) matrix associated with the best-performing
#'   configuration.
#'   \item \code{best_params}: A list containing the hyperparameters that achieved the best score.
#'   \item \code{best_cluster}: The index of the meta fuzzy function yielding the minimum validation error.
#'   \item \code{best_weight}: The weight vector corresponding to the best-performing meta fuzzy function.
#'   \item \code{best_scores}: The full set of evaluation scores for all meta fuzzy function predictions under
#'   the best configuration.
#'   \item \code{search_diagnostics}: One row per requested configuration,
#'   recording its seed, success status, selection metric, and any error
#'   message. A failed configuration is skipped rather than aborting the full
#'   search.
#'   \item \code{n_successful}, \code{n_failed}: Numbers of successful and
#'   failed grid configurations.
#' }
#'
#' @seealso \code{\link{mff}}, \code{\link{model.train}}, \code{\link{predict.mff}}
#'
#' @examples
#' set.seed(123)
#' y_valid <- seq_len(30) + stats::rnorm(30, sd = 0.2)
#' pred_valid <- cbind(
#'   model_a = y_valid + stats::rnorm(30, sd = 0.3),
#'   model_b = y_valid + stats::rnorm(30, sd = 0.4),
#'   model_c = y_valid + stats::rnorm(30, sd = 1.2),
#'   model_d = y_valid + stats::rnorm(30, sd = 1.4)
#' )
#' fit <- tune.mff(
#'   pred_valid, y_valid,
#'   max_c = 3, mff.method = "kmeans",
#'   nstart = 10, parallel = FALSE, logging = FALSE
#' )
#' fit$best_params
#' fit$best_cluster
#'
#' @importFrom foreach foreach %dopar%
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom doParallel registerDoParallel
#' @export
tune.mff <- function(x, y, max_c, m_seq = seq(1.1, 3, by = 0.1),
                     eta_seq = seq(1.1, 3, by = 0.4), iter.max = NULL,
                     nstart = 1, seed = 123,
                     mff.method = c("fcm", "pfcm", "kmeans", "gk"),
                     eval.method = c("MAE", "RMSE", "MAPE", "SMAPE", "MSE", "MedAE"),
                     stand = FALSE, parallel = FALSE, num_cores = NULL,
                     logging = TRUE,
                     clustering.reduction = c("none", "pca")) {

  .validate_prediction_problem(x, y)
  max_c <- .validate_scalar_integer(max_c, "max_c", lower = 2L)
  nstart <- .validate_scalar_integer(nstart, "nstart")
  if (!is.null(iter.max)) {
    iter.max <- .validate_scalar_integer(iter.max, "iter.max")
  }
  if (length(seed) != 1L || !is.numeric(seed) || !is.finite(seed)) {
    stop("'seed' must be a single finite number.", call. = FALSE)
  }
  if (length(stand) != 1L || !is.logical(stand) || is.na(stand)) {
    stop("'stand' must be TRUE or FALSE.", call. = FALSE)
  }
  if (length(parallel) != 1L || !is.logical(parallel) || is.na(parallel)) {
    stop("'parallel' must be TRUE or FALSE.", call. = FALSE)
  }
  if (length(logging) != 1L || !is.logical(logging) || is.na(logging)) {
    stop("'logging' must be TRUE or FALSE.", call. = FALSE)
  }

  if (max_c > ncol(x)) {
    stop(sprintf("Number of clusters (%d) cannot exceed models (%d).", max_c, ncol(x)))
  }

  mff.method <- match.arg(mff.method)
  eval.method <- match.arg(eval.method)
  clustering.reduction <- match.arg(clustering.reduction)

  if (mff.method %in% c("fcm", "pfcm", "gk") &&
      (!is.numeric(m_seq) || !length(m_seq) || any(!is.finite(m_seq)) ||
       any(m_seq <= 1))) {
    stop("'m_seq' must contain finite numeric values greater than 1.",
         call. = FALSE)
  }
  if (mff.method == "pfcm" &&
      (!is.numeric(eta_seq) || !length(eta_seq) || any(!is.finite(eta_seq)) ||
       any(eta_seq <= 0))) {
    stop("'eta_seq' must contain finite positive numeric values for PFCM.",
         call. = FALSE)
  }

  # --- 1. Grid Construction ---
  if (mff.method == "pfcm") {
    search_grid <- expand.grid(m = m_seq, c = 2:max_c, eta = eta_seq,
                                nstart = nstart, KEEP.OUT.ATTRS = FALSE)
  } else if (mff.method == "fcm") {
    search_grid <- expand.grid(m = m_seq, c = 2:max_c, KEEP.OUT.ATTRS = FALSE)
  } else if (mff.method == "gk") {
    search_grid <- expand.grid(m = m_seq, c = 2:max_c,
                               nstart = nstart, KEEP.OUT.ATTRS = FALSE)
  } else if (mff.method == "kmeans") {
    search_grid <- expand.grid(c = 2:max_c, nstart = nstart, KEEP.OUT.ATTRS = FALSE)
  }

  num_combinations <- nrow(search_grid)
  if(logging) cat("Number of Combinations:", num_combinations, "\n")

  # --- 2. Parallel Setup ---
  if (parallel) {
    if (!requireNamespace("doParallel", quietly = TRUE)) stop("Package 'doParallel' is required for parallel processing.")
    if (!requireNamespace("foreach", quietly = TRUE)) stop("Package 'foreach' is required for parallel processing.")

    if (is.null(num_cores)) {
      detected_cores <- parallel::detectCores()
      num_cores <- if (is.na(detected_cores)) 1L else max(1L, detected_cores - 1L)
    } else {
      num_cores <- .validate_scalar_integer(num_cores, "num_cores")
    }
    cl <- parallel::makeCluster(num_cores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl)) # Ensure cluster stops even if function fails

    if(logging) cat("Running in parallel across", num_cores, "cores...\n")
  }

  # --- 3. Execution ---
  # We use a helper to wrap the logic so it works for both serial and parallel
  run_iteration <- function(i) {
    set.seed(seed + i) # Ensure each worker has a unique but reproducible seed
    params <- as.list(search_grid[i, , drop = FALSE])
    iteration_nstart <- if (is.null(params$nstart)) nstart else params$nstart

    tryCatch({
      mff_result <- mff(
        x = x, y = y, c = params$c,
        m = params$m, eta = params$eta,
        iter.max = iter.max, nstart = iteration_nstart,
        method = mff.method,
        stand = stand,
        clustering.reduction = clustering.reduction
      )
      scores <- mff_result$cluster_scores[, eval.method]
      finite_scores <- scores[is.finite(scores)]
      if (!length(finite_scores)) {
        stop("configuration produced no finite validation score",
             call. = FALSE)
      }
      list(
        success = TRUE, metric = min(finite_scores), result = mff_result,
        params = params, message = NA_character_
      )
    }, error = function(e) {
      list(
        success = FALSE, metric = Inf, result = NULL, params = params,
        message = conditionMessage(e)
      )
    })
  }

  if (parallel) {
    # Parallel loop using foreach
    all_results <- foreach::foreach(i = 1:num_combinations,
                                    .packages = c("e1071", "ppclust", "stats"),
                                    .export = c("mff", "evaluate", ".weight_kmeans")) %dopar% {
      run_iteration(i)
    }
  } else {
    # Standard serial loop
    all_results <- list()
    for (i in 1:num_combinations) {
      if(logging) cat(i, " ")
      all_results[[i]] <- run_iteration(i)
    }
    if(logging) cat("\n")
  }

  # --- 4. Best Model Selection ---
  metrics <- vapply(all_results, function(res) res$metric, numeric(1))
  successful <- vapply(
    all_results,
    function(res) isTRUE(res$success) && is.finite(res$metric),
    logical(1)
  )
  if (!any(successful)) {
    messages <- unique(vapply(all_results, function(res) {
      if (is.null(res$message) || is.na(res$message)) {
        "unknown error"
      } else {
        res$message
      }
    }, character(1)))
    stop(
      sprintf(
        "All %d '%s' grid configurations failed. First error: %s",
        num_combinations, mff.method, messages[[1L]]
      ),
      call. = FALSE
    )
  }
  best_idx <- which.min(metrics)
  best_res <- all_results[[best_idx]]$result

  idx_in_cluster <- unname(which.min(best_res$cluster_scores[, eval.method]))
  best_weight <- best_res$weights[, idx_in_cluster]

  search_diagnostics <- search_grid
  search_diagnostics$iteration_seed <- seed + seq_len(num_combinations)
  search_diagnostics$success <- successful
  search_diagnostics$metric <- metrics
  search_diagnostics$message <- vapply(
    all_results,
    function(res) if (isTRUE(res$success)) NA_character_ else res$message,
    character(1)
  )

  out <- list(
    algorithm = mff.method,
    eval.method = eval.method,
    clustering_reduction = best_res$clustering_reduction,
    weights = best_res$weights,
    best_params = all_results[[best_idx]]$params,
    best_cluster = idx_in_cluster,
    best_weight = best_weight,
    best_scores = best_res$cluster_scores,
    search_diagnostics = search_diagnostics,
    n_successful = sum(successful),
    n_failed = sum(!successful)
  )

  out <- structure(out, class = "mff")
  return(out)
}
