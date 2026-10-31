i.iff_pvx_matrix <- function(theta_v, thres) {
  theta_v <- as.numeric(theta_v)
  thres <- as.numeric(thres)
  thres <- thres[is.finite(thres)]
  
  if (length(theta_v) < 1L || any(!is.finite(theta_v))) {
    stop("'theta_v' must contain finite numeric values.")
  }
  if (length(thres) < 1L) {
    stop("At least one finite item threshold is required.")
  }
  
  scores <- 0:length(thres)
  eta <- outer(scores, theta_v, "*")
  eta <- sweep(eta, 1L, cumsum(c(0, thres)), "-")
  eta <- sweep(eta, 2L, apply(eta, 2L, max), "-")
  
  numerator <- exp(eta)
  probability <- sweep(numerator, 2L, colSums(numerator), "/")
  rownames(probability) <- paste0("cat.", scores)
  colnames(probability) <- as.character(theta_v)
  probability
}

i.iff_information <- function(theta_v, thres) {
  probability <- i.iff_pvx_matrix(theta_v, thres)
  scores <- 0:(nrow(probability) - 1L)
  score_matrix <- matrix(
    scores,
    nrow = length(scores),
    ncol = length(theta_v),
    byrow = FALSE
  )
  expected <- colSums(probability * score_matrix)
  centred <- sweep(score_matrix, 2L, expected, "-")
  category_information <- probability * centred^2
  information <- colSums(category_information)
  information[information < 0 & information > -sqrt(.Machine$double.eps)] <- 0
  
  list(
    information = information,
    category_information = t(category_information),
    probability = probability,
    expected_score = expected
  )
}


#' @title Item information function
#' @export iff
#' @keywords item fit
#' @description plotting function for plotting the Item information function(IIF). Item information function using the Rasch/PCM Fisher information.
#' @details This version of the funktion uses the conditional variance of the item score. For a dichotomous Rasch item, I_i(theta) = p(theta) * (1 - p(theta)); for a Partial Credit item, I_i(theta) = Var(X_i | theta).
#' @param pair_obj an object of class \code{"pair"} as a result from function \code{\link{pair}}.
#' @param itemnumber One numeric item position or one item name.
#' @param x Numeric theta values. If NULL, `nodes` points from -`ra` to `ra`
#'   are used. A single theta suppresses plotting.
#' @param ra,nodes Theta range and number of evaluation points when x is NULL.
#' @param plot Logical; draw the plot?
#' @param cat Logical; plot or return category-information contributions?
#' @param lwd,col,lty Graphical parameters for the item-information curve.
#' @param main,xlab,ylab,ylim,bty Plot settings.
#' @param ... Further arguments passed to plot().
#' @return Item information, or category-information contributions if cat=TRUE.
#' @examples ########
#' data(sim200x3)
#' result <- pair(sim200x3)
#' iff(pair_obj = result) 
#' #' # IFF plot for Item No. 2 
#' iff(pair_obj = result, itemnumber = 2 ) 
#' # IFF plot for Categories of Item No. 2
#' iff(pair_obj = result, itemnumber = 2 ,cat=TRUE)
#' # IFF at theta=0 for Item No. 2 
#' iff(pair_obj = result, itemnumber = 2 ,x=0) 
#' # IFF at theta=0 for Categories of Item No. 2
#' iff(pair_obj = result, itemnumber = 2 ,x=0,cat=TRUE)
#' # IFF of Item No. 2 for a given range of thetas 
#' iff(pair_obj = result, itemnumber = 2 ,x=seq(0,4,.1)) 
#' # ... etc.
#' iff(pair_obj = result, itemnumber = 2 ,x=seq(0,4,.1),cat=TRUE) 
#' ##### examples with other data ...
#' data(bfiN)
#' result <- pair(bfiN)
#' iff(pair_obj = result, itemnumber = 3 )
#' iff(pair_obj = result, itemnumber = 3 ,cat=TRUE)
####################################################

iff <- function(pair_obj,
                itemnumber = 1,
                x = NULL,
                ra = 4,
                nodes = 161,
                plot = TRUE,
                cat = FALSE,
                lwd = 2,
                col = 1,
                lty = 1,
                main = NULL,
                xlab = "Theta (ability logits)",
                ylab = "Information",
                ylim = NULL,
                bty = "n",
                ...) {

  if (is.null(pair_obj$threshold)) {
    stop("'pair_obj' must contain pair_obj$threshold.")
  }
  thresholds <- as.matrix(pair_obj$threshold)
  item_names <- rownames(thresholds)
  if (is.null(item_names)) {
    item_names <- paste0("Item ", seq_len(nrow(thresholds)))
  }

  if (is.character(itemnumber)) {
    if (length(itemnumber) != 1L) {
      stop("'itemnumber' must identify exactly one item.")
    }
    item_index <- match(itemnumber, item_names)
  } else {
    if (length(itemnumber) != 1L) {
      stop("'itemnumber' must identify exactly one item.")
    }
    item_index <- as.integer(itemnumber)
  }

  if (is.na(item_index) || item_index < 1L || item_index > nrow(thresholds)) {
    stop("'itemnumber' is not a valid item position or item name.")
  }

  if (is.null(x) || length(x) == 0L) {
    theta_v <- seq(-ra, ra, length.out = as.integer(round(nodes)))
  } else {
    theta_v <- as.numeric(x)
    if (any(!is.finite(theta_v))) {
      stop("'x' must contain finite numeric theta values.")
    }
  }
  if (length(theta_v) == 1L) {
    plot <- FALSE
  }

  thres <- as.numeric(thresholds[item_index, , drop = TRUE])
  thres <- thres[is.finite(thres)]
  result <- i.iff_information(theta_v, thres)

  item_information <- result$information
  category_information <- result$category_information

  if (isTRUE(plot)) {
    values <- if (isTRUE(cat)) category_information else item_information
    if (is.null(ylim)) {
      y_max <- max(values, 0, na.rm = TRUE)
      if (!is.finite(y_max) || y_max <= 0) y_max <- 1
      ylim_used <- c(0, 1.05 * y_max)
    } else {
      ylim_used <- ylim
    }

    if (is.null(main)) {
      main <- paste0("Item Information Function\nfor item ", item_names[item_index])
    }

    graphics::plot(
      range(theta_v), ylim_used,
      type = "n", ylim = ylim_used, bty = bty,
      xlab = xlab, ylab = ylab, main = main, ...
    )

    if (isTRUE(cat)) {
      graphics::matlines(
        theta_v, category_information,
        lwd = lwd, col = col, lty = seq_len(ncol(category_information))
      )
    } else {
      graphics::lines(theta_v, item_information, lwd = lwd, col = col, lty = lty)
    }
  }

  if (isTRUE(cat)) {
    if (length(theta_v) == 1L) {
      answer <- as.numeric(category_information[1L, ])
      names(answer) <- colnames(category_information)
      return(answer)
    }
    return(category_information)
  }

  if (length(theta_v) == 1L) {
    return(unname(item_information[1L]))
  }

  invisible(item_information)
}
