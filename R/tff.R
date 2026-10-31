## Numerically stable category probabilities for the Rasch/Partial Credit Model.
## This is a self-contained replacement for pvx.matrix(). Categories are coded
## 0, ..., m, where m is the number of thresholds.
i.tff_pvx_matrix <- function(theta_v, thres, xm_v = NULL) {
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
  threshold_sum <- cumsum(c(0, thres))
  
  eta <- outer(scores, theta_v, "*")
  eta <- sweep(eta, MARGIN = 1L, STATS = threshold_sum, FUN = "-")
  
  ## Log-sum-exp stabilisation: subtract the largest linear predictor at each
  ## theta value before exponentiation.
  eta <- sweep(eta, MARGIN = 2L, STATS = apply(eta, 2L, max), FUN = "-")
  numerator <- exp(eta)
  probability <- sweep(
    numerator,
    MARGIN = 2L,
    STATS = colSums(numerator),
    FUN = "/"
  )
  
  rownames(probability) <- paste0("cat.", scores)
  colnames(probability) <- as.character(theta_v)
  
  if (is.null(xm_v) || length(xm_v) == 0L) {
    return(probability)
  }
  
  xm_v <- as.integer(xm_v)
  if (length(xm_v) == 1L) {
    xm_v <- rep(xm_v, length(theta_v))
  }
  if (length(xm_v) != length(theta_v) ||
      anyNA(xm_v) || any(xm_v < 0L) || any(xm_v > max(scores))) {
    stop(
      "'xm_v' must contain one valid zero-based category index per theta value."
    )
  }
  
  probability[cbind(xm_v + 1L, seq_along(theta_v))]
}


## Compute category probabilities, expected scores, category contributions,
## and Fisher item information for one item.
i.tff_item_information <- function(theta_v, thres) {
  probability <- i.tff_pvx_matrix(theta_v = theta_v, thres = thres)
  scores <- 0:(nrow(probability) - 1L)
  
  score_matrix <- matrix(
    scores,
    nrow = length(scores),
    ncol = length(theta_v),
    byrow = FALSE
  )
  
  expected_score <- colSums(probability * score_matrix)
  centred_score <- sweep(
    score_matrix,
    MARGIN = 2L,
    STATS = expected_score,
    FUN = "-"
  )
  
  category_information <- probability * centred_score^2
  information <- colSums(category_information)
  
  ## Remove harmless negative rounding noise.
  information[information < 0 & information > -sqrt(.Machine$double.eps)] <- 0
  
  rownames(category_information) <- rownames(probability)
  colnames(category_information) <- colnames(probability)
  
  list(
    probability = probability,
    expected_score = expected_score,
    category_information = category_information,
    information = information
  )
}


i.tff_strip_unique_suffix <- function(x) {
  x <- trimws(as.character(x))
  sub("\\.[0-9]+$", "", x)
}


.tff_valid_name_vector <- function(x, n_items) {
  !is.null(x) &&
    length(x) == n_items &&
    all(!is.na(x)) &&
    all(nzchar(trimws(as.character(x))))
}


## Collect item-name aliases from all relevant components of a pair object.
## pairwise may internally append suffixes such as ".0" to threshold labels,
## while the response matrix retains the original item names.
.tff_item_name_info <- function(pair_obj, thresholds) {
  n_items <- nrow(thresholds)
  
  threshold_names <- rownames(thresholds)
  response_names <- if (!is.null(pair_obj$resp)) colnames(pair_obj$resp) else NULL
  sb_names <- if (!is.null(pair_obj$sb)) names(pair_obj$sb) else NULL
  
  candidate_vectors <- list(
    response = response_names,
    threshold = threshold_names,
    sb = sb_names
  )
  
  valid_candidates <- vapply(
    candidate_vectors,
    .tff_valid_name_vector,
    logical(1),
    n_items = n_items
  )
  
  ## Prefer original response-variable names for display. They are usually the
  ## names supplied by the user and do not contain threshold/category suffixes.
  if (valid_candidates["response"]) {
    display_names <- as.character(response_names)
  } else if (valid_candidates["threshold"]) {
    display_names <- i.tff_strip_unique_suffix(threshold_names)
    if (anyDuplicated(display_names)) {
      display_names <- as.character(threshold_names)
    }
  } else if (valid_candidates["sb"]) {
    display_names <- as.character(sb_names)
  } else {
    display_names <- paste0("Item ", seq_len(n_items))
  }
  
  aliases <- vector("list", n_items)
  for (i in seq_len(n_items)) {
    values <- display_names[i]
    
    for (candidate in candidate_vectors[valid_candidates]) {
      values <- c(values, as.character(candidate[[i]]))
    }
    
    values <- c(values, i.tff_strip_unique_suffix(values))
    values <- trimws(values)
    aliases[[i]] <- unique(values[!is.na(values) & nzchar(values)])
  }
  names(aliases) <- display_names
  
  list(
    display_names = display_names,
    aliases = aliases,
    threshold_names = threshold_names,
    response_names = response_names,
    sb_names = sb_names
  )
}


i.tff_resolve_items <- function(selection, item_names, aliases = NULL,
                                argument = "items", null_means_all = TRUE) {
  n_items <- length(item_names)
  
  if (is.null(selection) || length(selection) == 0L) {
    if (null_means_all) {
      return(seq_len(n_items))
    }
    return(integer(0))
  }
  
  if (is.logical(selection)) {
    if (length(selection) != 1L || is.na(selection)) {
      stop("'", argument, "' must not be a logical vector of length > 1.")
    }
    if (isTRUE(selection)) {
      return(seq_len(n_items))
    }
    return(integer(0))
  }
  
  if (is.character(selection)) {
    if (length(selection) == 1L && tolower(trimws(selection)) == "all") {
      return(seq_len(n_items))
    }
    
    if (is.null(aliases)) {
      aliases <- lapply(item_names, function(x) unique(c(x, i.tff_strip_unique_suffix(x))))
    }
    
    normalise <- function(x) {
      tolower(trimws(as.character(x)))
    }
    
    alias_norm <- lapply(aliases, normalise)
    selection_norm <- normalise(selection)
    index <- integer(length(selection_norm))
    
    for (j in seq_along(selection_norm)) {
      hits <- which(vapply(
        alias_norm,
        function(x) selection_norm[j] %in% x,
        logical(1)
      ))
      
      if (length(hits) == 0L) {
        stop(
          "Unknown item name in '", argument, "': ", selection[j], ". ",
          "Available item names are: ",
          paste(item_names, collapse = ", ")
        )
      }
      
      if (length(hits) > 1L) {
        stop(
          "Ambiguous item name in '", argument, "': ", selection[j], ". ",
          "It matches: ", paste(item_names[hits], collapse = ", "), "."
        )
      }
      
      index[j] <- hits
    }
  } else if (is.numeric(selection) || is.integer(selection)) {
    index <- as.integer(selection)
    if (anyNA(index) || any(index < 1L) || any(index > n_items)) {
      stop(
        "'", argument, "' contains invalid item positions. Valid positions are 1 to ",
        n_items, "."
      )
    }
  } else {
    stop(
      "'", argument, "' must be NULL, TRUE/FALSE, 'all', item positions, ",
      "or item names."
    )
  }
  
  index[!duplicated(index)]
}


i.tff_is_black <- function(col) {
  if (is.null(col) || length(col) < 1L) {
    return(FALSE)
  }
  
  col_character <- tolower(as.character(col))
  all(col_character %in% c("black", "#000000", "#000000ff", "1"))
}


i.tff_auto_colours <- function(n) {
  if (n < 1L) {
    return(character(0))
  }
  
  if (exists("hcl.colors", envir = asNamespace("grDevices"), mode = "function")) {
    grDevices::hcl.colors(n, palette = "Dark 3")
  } else {
    grDevices::rainbow(n)
  }
}


## Convert numeric palette indices and numeric-looking character values to
## explicit colour names. This prevents c(1, "black") from becoming the
## invalid colour vector c("1", "black") in graphics::legend().
i.tff_normalize_colours <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(character(0))
  }
  
  out <- as.character(x)
  palette_values <- grDevices::palette()
  
  numeric_colour <- grepl("^[[:space:]]*[0-9]+[[:space:]]*$", out)
  if (any(numeric_colour)) {
    index <- suppressWarnings(as.integer(out[numeric_colour]))
    valid <- !is.na(index) & index >= 1L
    
    replacement <- out[numeric_colour]
    replacement[valid] <- palette_values[
      ((index[valid] - 1L) %% length(palette_values)) + 1L
    ]
    out[numeric_colour] <- replacement
  }
  
  out
}


## Convert numeric line-type codes to their canonical character names before
## they are combined with character or hexadecimal line patterns. Without this
## conversion, c(1, "dashed") becomes c("1", "dashed"), and "1" is not a
## valid character line type for graphics::segments()/graphics::legend().
i.tff_normalize_lty <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(character(0))
  }
  
  out <- as.character(x)
  standard_names <- c(
    "blank", "solid", "dashed", "dotted",
    "dotdash", "longdash", "twodash"
  )
  
  numeric_lty <- grepl("^[[:space:]]*[0-6][[:space:]]*$", out)
  if (any(numeric_lty)) {
    index <- suppressWarnings(as.integer(out[numeric_lty]))
    out[numeric_lty] <- standard_names[index + 1L]
  }
  
  out
}

################################################################################
################################################################################


#' @title Test information function
#' @export tff
#' @keywords model fit
#' @description plotting function for plotting the test information function (TIF) with optional item-information overlays. Computes and plots the test information function (TIF) for a Rasch or Partial Credit Model fitted by pairwise::pair(). Selected item information functions (IIFs) can be added to the same plot. #'
#' @details The Item information is calculated as the conditional variance of the item score, \code{I_i(theta) = sum_k p_ik(theta) * (k - E[X_i | theta])^2. For a dichotomous Rasch item this reduces to p(theta) * (1 - p(theta))}. Test information is the sum of the item information functions of the items selected through 'items'.
#' @param pair_obj An object of class "pair", returned by pairwise::pair().
#' @param items NULL, numeric item positions, or character item names defining
#'   which items contribute to the test information. NULL selects all items.
#' @param item.info Controls the item-information overlays. FALSE or NULL plots
#'   only the TIF; TRUE or "all" plots the IIFs of all items selected through
#'   `items`; a numeric or character vector plots only the requested IIFs.
#' @param x Numeric theta values. If NULL, 'nodes' values from -'ra' to 'ra'
#'   are used. A single value suppresses plotting, as in the earlier versions of the function.
#' @param ra Positive theta range used when x is NULL.
#' @param nodes Number of theta values used when x is NULL.
#' @param main Plot title.
#' @param plot Logical; draw the plot?
#' @param cat Logical; additionally draw category-information contributions
#'   p_ik(theta) * (k - E[X_i | theta])^2 for the test items. This option is
#'   mainly retained for compatibility and may produce many curves.
#' @param lwd Line width of the test information function.
#' @param col Colour of the test information function. With col = "black"
#'   (or col = 1), item curves default to black and are distinguished by line
#'   types and, optionally, plotting symbols.
#' @param test.lty Line type of the test information function.
#' @param item.lwd Line width of item information functions.
#' @param item.col Optional colours for item information functions. If NULL,
#'   black is used in a black-and-white display; otherwise colours are created
#'   automatically.
#' @param item.lty Line types used to distinguish item information functions.
#' @param item.points Logical; add plotting symbols to item curves?
#' @param item.pch Plotting symbols used to distinguish items.
#' @param point.every Put an item symbol at every nth theta value.
#' @param point.cex Size of item plotting symbols.
#' @param category.lty Line types for category-information contributions.
#' @param category.lwd Line width for category-information contributions.
#' @param category.col Optional colour for category-information contributions.
#'   If NULL, the corresponding item colour is used.
#' @param show.legend Logical; draw a legend for the TIF and displayed IIFs?
#' @param legend.position Position of the item legend.
#' @param legend.cex Character expansion of the legend.
#' @param legend.ncol Number of legend columns. NULL chooses this automatically.
#' @param xlab,ylab Axis labels.
#' @param ylim Optional y-axis limits.
#' @param bty Box type passed to plot().
#' @param verbose Logical; print which items contribute to the TIF and which
#'   IIFs are displayed?
#' @param ... Further arguments passed to the initial plot().
#'
#' @return When plot = TRUE, invisibly returns a list containing theta values,
#'   test information, item information, category contributions, and item
#'   selections. When plot = FALSE and cat = FALSE, returns a data frame (or a
#'   named numeric vector for one theta value) containing the TIF and requested
#'   IIFs. With cat = TRUE, the complete result list is returned.
#' @examples
#' # result <- pairwise::pair(sim200x3)
#' # tff(result)                                  # TIF only
#' # tff(result, item.info = TRUE)                # TIF + all IIFs
#' # tff(result, item.info = c(1, 3))             # selected IIFs
#' # tff(result, item.info = c("V1", "V3"))
#' # tff(result, items = 1:4, item.info = TRUE)   # TIF from items 1:4
#' # tff(result, item.info = 1:5, col = "black",
#' #     item.points = TRUE)

tff <- function(pair_obj,
                items = NULL,
                item.info = FALSE,
                x = NULL,
                ra = 4,
                nodes = 161,
                main = "Test Information Function",
                plot = TRUE,
                cat = FALSE,
                lwd = 2.5,
                col = 1,
                test.lty = 1,
                item.lwd = 1.25,
                item.col = NULL,
                item.lty = c(
                  "dashed", "dotted", "dotdash", "longdash", "twodash",
                  "44", "13", "1343", "73", "2262", "12223242", "solid"
                ),
                item.points = FALSE,
                item.pch = c(
                  1, 2, 0, 5, 6, 3, 4, 8, 9, 10,
                  11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                  21, 22, 23, 24, 25
                ),
                point.every = 12,
                point.cex = 0.7,
                category.lty = c("dotted", "dotdash", "longdash", "twodash"),
                category.lwd = 0.7,
                category.col = NULL,
                show.legend = TRUE,
                legend.position = "topright",
                legend.cex = 0.8,
                legend.ncol = NULL,
                xlab = "Theta (ability logits)",
                ylab = "Information",
                ylim = NULL,
                bty = "n",
                verbose = FALSE,
                ...) {

  ## ------------------------------------------------------------------
  ## 1. Validate object and resolve item selections
  ## ------------------------------------------------------------------
  if (is.null(pair_obj$threshold)) {
    stop("'pair_obj' must contain pair_obj$threshold.")
  }

  thresholds <- as.matrix(pair_obj$threshold)
  if (nrow(thresholds) < 1L) {
    stop("pair_obj$threshold does not contain any items.")
  }

  item_name_info <- .tff_item_name_info(
    pair_obj = pair_obj,
    thresholds = thresholds
  )
  item_names <- item_name_info$display_names
  item_aliases <- item_name_info$aliases

  test_index <- i.tff_resolve_items(
    selection = items,
    item_names = item_names,
    aliases = item_aliases,
    argument = "items",
    null_means_all = TRUE
  )

  if (length(test_index) < 1L) {
    stop("At least one item must contribute to the test information.")
  }

  if (isTRUE(item.info) ||
      (is.character(item.info) && length(item.info) == 1L &&
       tolower(item.info) == "all")) {
    overlay_index <- test_index
  } else if (isFALSE(item.info) || is.null(item.info) || length(item.info) == 0L) {
    overlay_index <- integer(0)
  } else {
    overlay_index <- i.tff_resolve_items(
      selection = item.info,
      item_names = item_names,
      aliases = item_aliases,
      argument = "item.info",
      null_means_all = FALSE
    )
  }

  ## ------------------------------------------------------------------
  ## 2. Theta grid
  ## ------------------------------------------------------------------
  if (is.null(x) || length(x) == 0L) {
    if (!is.numeric(ra) || length(ra) != 1L || !is.finite(ra) || ra <= 0) {
      stop("'ra' must be one positive finite number.")
    }
    if (!is.numeric(nodes) || length(nodes) != 1L ||
        !is.finite(nodes) || nodes < 2) {
      stop("'nodes' must be a finite number of at least 2.")
    }
    theta_v <- seq(-ra, ra, length.out = as.integer(round(nodes)))
  } else {
    theta_v <- as.numeric(x)
    if (length(theta_v) < 1L || any(!is.finite(theta_v))) {
      stop("'x' must contain finite numeric theta values.")
    }
  }

  if (length(theta_v) == 1L) {
    plot <- FALSE
  }

  ## ------------------------------------------------------------------
  ## 3. Item and test information
  ## ------------------------------------------------------------------
  calculation_index <- unique(c(test_index, overlay_index))
  components <- vector("list", length(calculation_index))
  names(components) <- item_names[calculation_index]

  for (j in seq_along(calculation_index)) {
    index <- calculation_index[j]
    thres <- as.numeric(thresholds[index, , drop = TRUE])
    thres <- thres[is.finite(thres)]

    if (length(thres) < 1L) {
      stop("Item '", item_names[index], "' has no finite thresholds.")
    }

    components[[j]] <- i.tff_item_information(
      theta_v = theta_v,
      thres = thres
    )
  }

  get_component <- function(index) {
    components[[match(index, calculation_index)]]
  }

  test_item_information <- vapply(
    test_index,
    FUN = function(index) get_component(index)$information,
    FUN.VALUE = numeric(length(theta_v))
  )

  if (is.null(dim(test_item_information))) {
    test_item_information <- matrix(
      test_item_information,
      nrow = length(theta_v),
      ncol = 1L
    )
  }
  colnames(test_item_information) <- item_names[test_index]
  rownames(test_item_information) <- as.character(theta_v)

  test_information <- rowSums(test_item_information)

  if (length(overlay_index) > 0L) {
    overlay_information <- vapply(
      overlay_index,
      FUN = function(index) get_component(index)$information,
      FUN.VALUE = numeric(length(theta_v))
    )
    if (is.null(dim(overlay_information))) {
      overlay_information <- matrix(
        overlay_information,
        nrow = length(theta_v),
        ncol = 1L
      )
    }
    colnames(overlay_information) <- item_names[overlay_index]
    rownames(overlay_information) <- as.character(theta_v)
  } else {
    overlay_information <- matrix(
      numeric(0),
      nrow = length(theta_v),
      ncol = 0L,
      dimnames = list(as.character(theta_v), character(0))
    )
  }

  category_information <- setNames(
    lapply(test_index, function(index) {
      t(get_component(index)$category_information)
    }),
    item_names[test_index]
  )

  ## ------------------------------------------------------------------
  ## 4. Plot
  ## ------------------------------------------------------------------
  test_col <- if (is.null(col) || length(col) == 0L) "black" else col[1L]
  test_col <- i.tff_normalize_colours(test_col)[1L]
  test_lty_used <- i.tff_normalize_lty(test.lty)[1L]
  n_overlay <- length(overlay_index)

  if (n_overlay > 0L) {
    if (is.null(item.col) || length(item.col) == 0L) {
      if (i.tff_is_black(test_col)) {
        item_col <- rep("black", n_overlay)
      } else {
        item_col <- i.tff_auto_colours(n_overlay)
      }
    } else {
      item_col <- rep(item.col, length.out = n_overlay)
    }

    item_col <- i.tff_normalize_colours(item_col)
    item_lty_used <- i.tff_normalize_lty(
      rep(item.lty, length.out = n_overlay)
    )
    item_pch_used <- rep(item.pch, length.out = n_overlay)
  } else {
    item_col <- character(0)
    item_lty_used <- numeric(0)
    item_pch_used <- integer(0)
  }

  if (!is.logical(item.points) || length(item.points) != 1L || is.na(item.points)) {
    stop("'item.points' must be TRUE or FALSE.")
  }
  if (!is.numeric(point.every) || length(point.every) != 1L ||
      !is.finite(point.every) || point.every < 1) {
    stop("'point.every' must be a positive finite number.")
  }
  point.every <- as.integer(round(point.every))

  plot_values <- test_information
  if (n_overlay > 0L) {
    plot_values <- c(plot_values, as.numeric(overlay_information))
  }
  if (isTRUE(cat)) {
    plot_values <- c(
      plot_values,
      unlist(category_information, use.names = FALSE)
    )
  }

  if (is.null(ylim)) {
    y_max <- max(plot_values[is.finite(plot_values)], 0, na.rm = TRUE)
    if (!is.finite(y_max) || y_max <= 0) {
      y_max <- 1
    }
    ylim_used <- c(0, 1.05 * y_max)
  } else {
    if (!is.numeric(ylim) || length(ylim) != 2L || any(!is.finite(ylim))) {
      stop("'ylim' must contain two finite numeric values.")
    }
    ylim_used <- ylim
  }

  if (isTRUE(plot)) {
    graphics::plot(
      x = range(theta_v),
      y = ylim_used,
      type = "n",
      ylim = ylim_used,
      bty = bty,
      xlab = xlab,
      ylab = ylab,
      main = main,
      ...
    )

    ## Category contributions are drawn first and remain visually subordinate.
    if (isTRUE(cat)) {
      n_category_curves <- sum(vapply(
        category_information,
        FUN = ncol,
        FUN.VALUE = integer(1)
      ))
      if (n_category_curves > 30L) {
        warning(
          "cat = TRUE draws ", n_category_curves,
          " category-information curves; the plot may be crowded."
        )
      }

      for (j in seq_along(test_index)) {
        category_matrix <- category_information[[j]]
        base_col <- if (is.null(category.col) || length(category.col) == 0L) {
          if (test_index[j] %in% overlay_index) {
            item_col[match(test_index[j], overlay_index)]
          } else {
            "gray60"
          }
        } else {
          rep(category.col, length.out = length(test_index))[j]
        }

        for (k in seq_len(ncol(category_matrix))) {
          graphics::lines(
            theta_v,
            category_matrix[, k],
            col = base_col,
            lty = rep(category.lty, length.out = ncol(category_matrix))[k],
            lwd = category.lwd
          )
        }
      }
    }

    if (n_overlay > 0L) {
      marker_index <- unique(c(
        seq.int(1L, length(theta_v), by = point.every),
        length(theta_v)
      ))

      for (j in seq_len(n_overlay)) {
        graphics::lines(
          theta_v,
          overlay_information[, j],
          col = item_col[j],
          lty = item_lty_used[j],
          lwd = item.lwd
        )

        if (isTRUE(item.points)) {
          graphics::points(
            theta_v[marker_index],
            overlay_information[marker_index, j],
            col = item_col[j],
            pch = item_pch_used[j],
            cex = point.cex
          )
        }
      }
    }

    ## Draw the TIF last, so that it remains the dominant curve.
    graphics::lines(
      theta_v,
      test_information,
      col = test_col,
      lty = test_lty_used,
      lwd = lwd
    )

    if (isTRUE(show.legend) && n_overlay > 0L) {
      if (is.null(legend.ncol)) {
        legend.ncol.used <- max(1L, ceiling((n_overlay + 1L) / 10L))
      } else {
        legend.ncol.used <- as.integer(legend.ncol)
        if (length(legend.ncol.used) != 1L || is.na(legend.ncol.used) ||
            legend.ncol.used < 1L) {
          stop("'legend.ncol' must be NULL or one positive integer.")
        }
      }

      graphics::legend(
        legend.position,
        legend = c("Test information", item_names[overlay_index]),
        col = c(test_col, item_col),
        lty = c(test_lty_used, item_lty_used),
        lwd = c(lwd, rep(item.lwd, n_overlay)),
        pch = c(
          NA_integer_,
          if (isTRUE(item.points)) item_pch_used else rep(NA_integer_, n_overlay)
        ),
        pt.cex = point.cex,
        cex = legend.cex,
        ncol = legend.ncol.used,
        bty = "n"
      )
    }
  }

  ## ------------------------------------------------------------------
  ## 5. Return values
  ## ------------------------------------------------------------------
  result <- list(
    theta = theta_v,
    test_items = item_names[test_index],
    displayed_item_information = item_names[overlay_index],
    test_information = test_information,
    item_information = test_item_information,
    overlay_information = overlay_information,
    category_information = category_information,
    components = components
  )
  class(result) <- c("tff_result", "list")

  if (isTRUE(verbose)) {
    base::cat(
      "Test information based on item(s): ",
      paste(item_names[test_index], collapse = ", "),
      "\n",
      sep = ""
    )
    if (n_overlay > 0L) {
      base::cat(
        "Displayed item information function(s): ",
        paste(item_names[overlay_index], collapse = ", "),
        "\n",
        sep = ""
      )
    }
  }

  if (isTRUE(plot)) {
    return(invisible(result))
  }

  if (isTRUE(cat)) {
    return(result)
  }

  output <- data.frame(
    theta = theta_v,
    Test = test_information,
    check.names = FALSE
  )
  if (n_overlay > 0L) {
    output <- cbind(output, as.data.frame(overlay_information, optional = TRUE))
  }

  if (length(theta_v) == 1L) {
    answer <- as.numeric(output[1L, -1L, drop = TRUE])
    names(answer) <- names(output)[-1L]
    return(answer)
  }

  output
}
