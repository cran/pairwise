#' @title Item Parameter Calculation with Standard Errors
#' @export pairSE
#' @keywords main item
#' @description Calculates Rasch/PCM item parameters and resampling standard errors.
#' @param daten Response matrix or data.frame, one independent person per row.
#' @param m Category counts; see \code{\link{pair}}. If NULL, inferred once from full data.
#' @param w Finite non-negative case weights, one per row, with a finite positive sum.
#' @param nsample Integer number of random replicates, at least two. Ignored for jackknife, which uses N replicates.
#' @param size Fraction strictly between zero and one for subsampling. The rounded sample size must be between 2 and N-1. The alias "jack" selects jackknife when method is omitted or "jackknife". Ignored for bootstrap and explicit jackknife.
#' @param seed Numeric seed for random resampling, or "no" to use the current RNG state.
#' @param method "subsampling" (default), "bootstrap", or "jackknife". Must be named; previous positional arguments retain their positions.
#' @param pot,zerocor,likelihood,pot2,delta,conv,maxiter,progress,init,zerosum See \code{\link{pair}}.
#' @param verbose Print resampling progress?
#' @param ... Additional arguments passed to \code{\link{pair}}.
#' @return A \code{pairSE} list with threshold, sigma, SE, SEsigma, weights and resp, in their previous order. Attribute \code{resampling} records method, N, actual sample size, number of replicates and the multiplier applied to R's replicate standard deviation.
#' @details For subsampling, rows are sampled without replacement and the replicate
#' standard deviation is multiplied by sqrt(m/(N-m)), using the actual rounded
#' sample size m. This is exact for the sample mean and an asymptotic approximation
#' for regular estimators of independent, identically distributed rows. It is not
#' a guarantee for sparse or nonregular pairwise fits. At m=N/2 the factor is one.
#' Bootstrap draws N rows with replacement and uses the replicate standard
#' deviation. Jackknife omits each row exactly once and uses
#' sqrt((N-1)/N * sum((estimate_i - mean(estimate_i))^2)). No parameter bias
#' correction is applied. Point estimates always come from the complete data.
#' More random replicates reduce Monte Carlo variability, but do not fix bias.
#' Category counts are held fixed from the full-data fit. Failed or non-finite
#' replicate fits cause an error; unused threshold cells remain NA.
#' Weights travel with their rows; rows are sampled uniformly, not proportional
#' to weights. This treats observed row/weight pairs as the sampling units and
#' does not implement design-based variance estimation for survey, cluster or
#' stratified samples, nor expand integer frequency weights into independent rows.
#' The jackknife is deterministic and does not set or consume the random seed.
#' @examples
#' data(sim200x3)
#' pairSE(sim200x3, nsample=30, seed=42, verbose=FALSE)
#' pairSE(sim200x3, method="bootstrap", nsample=30, seed=42, verbose=FALSE)
#' \dontrun{pairSE(sim200x3, method="jackknife", verbose=FALSE)}

pairSE <- function(daten, m=NULL, w=NULL, nsample=30, size=0.50,
                   seed="no", pot=TRUE, zerocor=TRUE, verbose=TRUE,
                   likelihood=NULL, pot2=2, delta=TRUE, conv=1e-04,
                   maxiter=3000, progress=TRUE, init=NULL, zerosum=TRUE, ...,
                   method=c("subsampling", "bootstrap", "jackknife")) {
  method_supplied <- !missing(method)
  method <- match.arg(method)
  if (!(is.matrix(daten) || is.data.frame(daten)) ||
      nrow(daten) < 2L || ncol(daten) < 1L)
    stop("daten must be a matrix or data.frame with at least two rows and one item")
  N <- nrow(daten)
  if (!is.null(w)) {
    if (!is.numeric(w) || !is.null(dim(w)) || length(w) != N || anyNA(w) ||
        any(!is.finite(w)) || any(w < 0) || !is.finite(sum(w)) || sum(w) <= 0)
      stop("w must contain one finite, non-negative case weight per row, with a finite positive sum")
    w <- as.numeric(w)
    names(w) <- rownames(daten)
  }
  if (is.character(size)) {
    if (length(size) != 1L || is.na(size) || size != "jack")
      stop("size must be between 0 and 1 or 'jack'")
    if (method_supplied && method != "jackknife")
      stop("size='jack' requires method='jackknife' or an omitted method")
    method <- "jackknife"
  }
  if (method == "jackknife") {
    if (N < 3L) stop("jackknife requires at least three persons")
    nsize <- N - 1L
    nsample <- N
    scale <- (N - 1) / sqrt(N)
  } else {
    if (!is.numeric(nsample) || length(nsample) != 1L ||
        !is.finite(nsample) || nsample < 2 || nsample != floor(nsample))
      stop("nsample must be an integer >= 2")
    if (method == "bootstrap") {
      if (!missing(size))
        warning("size is ignored for bootstrap; each replicate contains N rows")
      nsize <- N
      scale <- 1
    } else {
      if (!is.numeric(size) || length(size) != 1L || !is.finite(size) ||
          size <= 0 || size >= 1)
        stop("size should have values between 0 and 1")
      nsize <- round(N * size)
      if (nsize < 2L || nsize >= N)
        stop("subsamples must contain at least two persons and fewer than N persons")
      scale <- sqrt(nsize / (N - nsize))
    }
  }
  fit <- function(index, categories) {
    pair(daten[index, , drop=FALSE], m=categories,
         w=if (is.null(w)) NULL else unname(w[index]), pot=pot,
         zerocor=zerocor, likelihood=likelihood, pot2=pot2, delta=delta,
         conv=conv, maxiter=maxiter, progress=progress, init=init,
         zerosum=zerosum, ...)
  }
  full <- fit(seq_len(N), m)
  # Fix category counts from the full data, even if categories disappear in a draw.
  categories <- full$m
  threshold_mask <- is.finite(full$threshold)
  sigma_mask <- is.finite(full$sigma)
  if (!all(is.finite(full$threshold[!is.na(full$threshold)])) || !all(sigma_mask))
    stop("full-data item parameters must be finite (apart from unused threshold cells)")
  if (method != "jackknife" && is.numeric(seed)) set.seed(seed)
  thresholds <- matrix(NA_real_, nsample, length(full$threshold))
  sigmas <- matrix(NA_real_, nsample, length(full$sigma))
  for (i in seq_len(nsample)) {
    index <- if (method == "jackknife") seq_len(N)[-i] else
      sample.int(N, nsize, replace=(method == "bootstrap"))
    z <- tryCatch(fit(index, categories), error=function(e)
      stop("Resample ", i, " (", method, ") failed: ", conditionMessage(e), call.=FALSE))
    if (!identical(dim(z$threshold), dim(full$threshold)) ||
        !identical(rownames(z$threshold), rownames(full$threshold)) ||
        !identical(names(z$sigma), names(full$sigma)) ||
        length(z$sigma) != length(full$sigma) ||
        any(!is.finite(z$threshold[threshold_mask])) ||
        any(!is.finite(z$sigma[sigma_mask])))
      stop("Resample ", i, " (", method,
           ") has non-finite or incompatible parameters; no replicates were silently discarded")
    thresholds[i, ] <- as.vector(z$threshold)
    sigmas[i, ] <- as.vector(z$sigma)
    if (isTRUE(verbose)) cat("sample", i, "of", nsample, "with size n =", nsize, "(", method, ")\n")
  }
  get_se <- function(values, mask) {
    out <- rep(NA_real_, ncol(values))
    out[mask] <- apply(values[, mask, drop=FALSE], 2L, sd) * scale
    out
  }
  SE <- matrix(get_se(thresholds, as.vector(threshold_mask)),
               nrow=nrow(full$threshold), ncol=ncol(full$threshold))
  rownames(SE) <- names(full$sigma)
  colnames(SE) <- paste("threshold", seq_len(ncol(SE)), sep=".")
  SEsigma <- get_se(sigmas, sigma_mask)
  names(SEsigma) <- names(full$sigma)
  result <- list(threshold=full$threshold, sigma=full$sigma, SE=SE,
                 SEsigma=SEsigma, weights=full$weights, resp=full$resp)
  class(result) <- c("pairSE", "list")
  attr(result, "resampling") <- list(method=method, N=N, sample_size=nsize,
                                   replicates=nsample, se_scale=scale)
  result
}
