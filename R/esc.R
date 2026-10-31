## Internal, numerically stable equivalent of pvx.matrix().
## It computes the category probabilities of the Partial Credit Model.
i.esc_pvx_matrix <- function(theta_v, thres) {
  s <- 0:length(thres)
  thres0 <- c(0, thres)
  
  eta <- outer(s, theta_v, "*")
  eta <- sweep(eta, MARGIN = 1L, STATS = cumsum(thres0), FUN = "-")
  
  ## Subtract the column maximum before exponentiation for numerical stability.
  eta <- sweep(eta, MARGIN = 2L, STATS = apply(eta, 2L, max), FUN = "-")
  numerator <- exp(eta)
  probabilities <- sweep(
    numerator,
    MARGIN = 2L,
    STATS = colSums(numerator),
    FUN = "/"
  )
  
  rownames(probabilities) <- paste0("cat.", s)
  colnames(probabilities) <- theta_v
  probabilities
}


## Detect an explicitly requested black-and-white item display.
i.esc_is_black <- function(col) {
  if (is.null(col) || length(col) < 1L) {
    return(FALSE)
  }
  
  col_character <- tolower(as.character(col))
  all(col_character %in% c("black", "#000000", "#000000ff", "1"))
}


## Form common ability groups for all selected items.
i.esc_make_groups <- function(theta, integ,
                              group.method = c("auto", "theta_levels", "equal_n")) {
  group.method <- match.arg(group.method)
  
  valid <- is.finite(theta)
  if (!any(valid)) {
    stop("No finite WLE values are available.")
  }
  
  theta_valid <- theta[valid]
  n_valid <- length(theta_valid)
  unique_theta <- sort(unique(theta_valid))
  n_unique <- length(unique_theta)
  requested_groups <- min(as.integer(integ), n_valid)
  
  frequency_theta <- table(theta_valid)
  largest_tie <- max(frequency_theta) / n_valid
  
  method_used <- group.method
  if (group.method == "auto") {
    ## Use theta-level grouping when WLEs are discrete or when a large tie
    ## would otherwise be split across several equal-n groups.
    method_used <- if (
      n_unique <= max(20L, 2L * requested_groups) || largest_tie >= 0.20
    ) {
      "theta_levels"
    } else {
      "equal_n"
    }
  }
  
  group <- rep(NA_integer_, length(theta))
  
  if (method_used == "theta_levels") {
    number_groups <- min(requested_groups, n_unique)
    
    ## Assign adjacent distinct WLE levels to groups. Persons with identical
    ## WLEs therefore always remain in the same empirical group.
    theta_level_group <- ceiling(
      seq_along(unique_theta) * number_groups / n_unique
    )
    group[valid] <- theta_level_group[match(theta_valid, unique_theta)]
  } else {
    number_groups <- requested_groups
    ordered_positions <- order(theta_valid, seq_along(theta_valid))
    ordered_group <- ceiling(
      seq_len(n_valid) * number_groups / n_valid
    )
    group_valid <- integer(n_valid)
    group_valid[ordered_positions] <- ordered_group
    group[valid] <- group_valid
  }
  
  group_data <- data.frame(
    theta = theta,
    group = group
  )
  group_data <- group_data[!is.na(group_data$group), , drop = FALSE]
  
  group_levels <- sort(unique(group_data$group))
  group_summary <- do.call(
    rbind,
    lapply(group_levels, function(g) {
      x <- group_data$theta[group_data$group == g]
      data.frame(
        group = g,
        theta = mean(x),
        n = length(x),
        theta_min = min(x),
        theta_max = max(x)
      )
    })
  )
  rownames(group_summary) <- NULL
  
  list(
    group = group,
    summary = group_summary,
    method = method_used,
    requested_groups = as.integer(integ),
    actual_groups = nrow(group_summary),
    n_unique_theta = n_unique,
    largest_tie_proportion = largest_tie
  )
}

###############################################################################
###############################################################################




#' @title Expected score curves for one or several items
#' @export esc
#' @keywords item fit
#' @description plotting function for plotting model-implied expected score curves from a Rasch or Partial Credit Model together with empirical item-score means. Several items can be drawn in one common coordinate system.
#' @details The empirical ability groups are formed once for all selected items. By default, ties in WLE estimates are not split across groups. This is important for Rasch WLEs, which often take only a small number of distinct values.
#' @param pers_obj Object of class "pers", returned by pairwise::pers().
#' @param itemnumber NULL, a numeric vector of item positions, or a character
#'   vector of item names. The default NULL selects all items contained in
#'   pers_obj$pair$threshold.
#' @param integ Positive integer giving the requested number of empirical
#'   ability groups, or "all" for the original score-wise display.
#' @param group.method Method for numeric integ:
#'   "auto" chooses "theta_levels" when ties are substantial and otherwise
#'   "equal_n"; "theta_levels" groups adjacent distinct WLE values without
#'   splitting ties; "equal_n" forms approximately equally sized person groups.
#' @param ra Positive number defining the theoretical theta range -ra to ra.
#' @param nodes Number of theta evaluation points.
#' @param lwd Line width of model-implied curves.
#' @param lty Line type(s) of model-implied curves in colour plots.
#' @param col Item colour(s). If NULL, colours are generated automatically.
#'   If all selected items are drawn with col = "black", the items are
#'   automatically distinguished by different line types.
#' @param bw.lty Line types used to distinguish items in a black-and-white
#'   plot. They are recycled when more items than line types are selected.
#' @param empirical Logical; add empirical curves?
#' @param empirical.lty Line type of empirical curves.
#' @param empirical.lwd Line width of empirical curves.
#' @param pch Plotting symbols used to distinguish items at the empirical
#'   group means. The vector is recycled when necessary.
#' @param show.legend Logical; draw legends?
#' @param legend.position Position of item legend.
#' @param type.legend.position Position of curve-type legend.
#' @param main,sub Optional plot title and subtitle.
#' @param verbose Logical; print information about the empirical grouping?
#' @param ... Further arguments passed to plot().
#'
#' @return Invisibly returns a list containing grouping information and all
#'   theoretical and empirical plotting values.#'
#' @examples
#' data(bfiN)
#' pers_obj <- pers(pair(bfiN))
#' esc(pers_obj, integ = 6)             # plot all items
#' esc(pers_obj, itemnumber = 1:5, integ = 6)
#' esc(pers_obj, itemnumber = 1:5, integ = 6, group.method = "theta_levels")
#' esc(pers_obj, itemnumber = 1:3, integ = "all")
#' esc(pers_obj, col = "black")             # black-and-white


esc <- function(pers_obj,
                itemnumber = NULL,
                integ = 6,
                group.method = c("auto", "theta_levels", "equal_n"),
                ra = 4,
                nodes = 100,
                lwd = 2,
                lty = 1,
                col = NULL,
                bw.lty = c(
                  "solid", "dashed", "dotted", "dotdash",
                  "longdash", "twodash", "44", "13",
                  "1343", "73", "2262", "12223242"
                ),
                empirical = TRUE,
                empirical.lty = 2,
                empirical.lwd = lwd + 0.3,
                pch = c(
                  1, 2, 0, 5, 6, 3, 4, 8, 9, 10,
                  11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                  21, 22, 23, 24, 25
                ),
                show.legend = TRUE,
                legend.position = "topleft",
                type.legend.position = "bottomright",
                main = NULL,
                sub = NULL,
                verbose = FALSE,
                ...) {

  group.method <- match.arg(group.method)

  ## ------------------------------------------------------------------
  ## 1. Validate and extract the pairwise results
  ## ------------------------------------------------------------------
  if (is.null(pers_obj$pair$resp) ||
      is.null(pers_obj$pair$threshold) ||
      is.null(pers_obj$pers$WLE)) {
    stop(
      "'pers_obj' must contain pers_obj$pair$resp, ",
      "pers_obj$pair$threshold, and pers_obj$pers$WLE."
    )
  }

  responses <- as.data.frame(pers_obj$pair$resp)
  thresholds <- as.matrix(pers_obj$pair$threshold)
  theta_wle <- as.numeric(pers_obj$pers$WLE)

  if (nrow(responses) != length(theta_wle)) {
    stop("The number of response rows does not equal the number of WLE values.")
  }

  threshold_names <- rownames(thresholds)
  response_names <- colnames(responses)

  if (is.null(threshold_names)) {
    threshold_names <- if (!is.null(response_names) &&
                           ncol(responses) == nrow(thresholds)) {
      response_names
    } else {
      paste0("Item ", seq_len(nrow(thresholds)))
    }
  }

  ## Match response columns to threshold rows by item name whenever possible.
  response_index_for_threshold <- seq_len(nrow(thresholds))
  if (!is.null(response_names) && all(threshold_names %in% response_names)) {
    response_index_for_threshold <- match(threshold_names, response_names)
  }

  if (is.null(itemnumber)) {
    ## Default: select every item represented by a threshold row.
    item_index <- seq_len(nrow(thresholds))
  } else if (is.character(itemnumber)) {
    item_index <- match(itemnumber, threshold_names)
    if (anyNA(item_index)) {
      unknown <- itemnumber[is.na(item_index)]
      stop("Unknown item name(s): ", paste(unknown, collapse = ", "))
    }
  } else {
    item_index <- as.integer(itemnumber)
  }

  if (length(item_index) < 1L ||
      anyNA(item_index) ||
      any(item_index < 1L) ||
      any(item_index > nrow(thresholds))) {
    stop(
      "'itemnumber' must be NULL, valid item positions, or valid item names."
    )
  }

  item_index <- item_index[!duplicated(item_index)]
  item_names <- threshold_names[item_index]
  response_index <- response_index_for_threshold[item_index]
  n_items <- length(item_index)

  if (anyNA(response_index) || any(response_index > ncol(responses))) {
    stop("Selected threshold rows cannot be matched to response columns.")
  }

  if (!is.numeric(ra) || length(ra) != 1L || is.na(ra) || ra <= 0) {
    stop("'ra' must be one positive number.")
  }
  if (!is.numeric(nodes) || length(nodes) != 1L || is.na(nodes) || nodes < 2) {
    stop("'nodes' must be a number of at least 2.")
  }

  integ_all <- is.character(integ) && length(integ) == 1L && integ == "all"
  integ_numeric <- is.numeric(integ) && length(integ) == 1L &&
    !is.na(integ) && integ >= 1

  if (!integ_all && !integ_numeric) {
    stop("'integ' must be a positive integer or the character string 'all'.")
  }

  if (integ_numeric) {
    integ <- as.integer(round(integ))
  }

  nodes <- as.integer(round(nodes))
  theta_theo <- seq(-ra, ra, length.out = nodes)

  black_and_white <- i.esc_is_black(col) && n_items > 1L

  if (length(bw.lty) < 1L) {
    stop("'bw.lty' must contain at least one valid line type.")
  }

  if (is.null(col)) {
    col <- if (n_items == 1L) {
      "black"
    } else if (exists("hcl.colors", envir = asNamespace("grDevices"))) {
      grDevices::hcl.colors(n_items, palette = "Dark 3")
    } else {
      grDevices::rainbow(n_items)
    }
  }

  col <- rep_len(col, n_items)
  item_lty <- if (black_and_white) {
    rep_len(bw.lty, n_items)
  } else {
    rep_len(lty, n_items)
  }
  pch <- rep_len(pch, n_items)

  ## In black-and-white plots, empirical curves retain the item-specific line
  ## type. Their points distinguish them from the smooth model curves.
  empirical_item_lty <- if (black_and_white) {
    item_lty
  } else {
    rep_len(empirical.lty, n_items)
  }

  ## ------------------------------------------------------------------
  ## 2. Form one common empirical grouping for all selected items
  ## ------------------------------------------------------------------
  grouping <- NULL
  if (empirical && integ_numeric) {
    grouping <- i.esc_make_groups(
      theta = theta_wle,
      integ = integ,
      group.method = group.method
    )

    if (verbose) {
      message(
        "Empirical grouping method: ", grouping$method,
        "; requested groups: ", grouping$requested_groups,
        "; actual groups: ", grouping$actual_groups,
        "; distinct WLE values: ", grouping$n_unique_theta,
        "; largest tie: ",
        formatC(100 * grouping$largest_tie_proportion,
                digits = 1, format = "f"), "%"
      )
      print(grouping$summary, row.names = FALSE)
    }
  }

  ## ------------------------------------------------------------------
  ## 3. Calculate theoretical and empirical values item by item
  ## ------------------------------------------------------------------
  plot_values <- vector("list", n_items)
  names(plot_values) <- item_names
  maximum_score <- 0

  for (j in seq_along(item_index)) {
    threshold_row <- item_index[j]
    response_column <- response_index[j]

    threshold <- as.numeric(stats::na.omit(thresholds[threshold_row, ]))
    if (length(threshold) < 1L) {
      stop("No estimable threshold was found for item '", item_names[j], "'.")
    }

    categories <- 0:length(threshold)
    maximum_score <- max(maximum_score, max(categories))

    probability_matrix <- i.esc_pvx_matrix(
      theta_v = theta_theo,
      thres = threshold
    )
    expected_score <- colSums(categories * probability_matrix)

    score_raw <- responses[[response_column]]
    score <- if (is.factor(score_raw)) {
      suppressWarnings(as.numeric(as.character(score_raw)))
    } else {
      suppressWarnings(as.numeric(score_raw))
    }
    if (all(is.na(score)) && any(!is.na(score_raw))) {
      stop("Responses for item '", item_names[j],
           "' cannot be converted to numeric scores.")
    }

    item_data <- data.frame(
      theta = theta_wle,
      score = score
    )
    n_missing <- sum(!(is.finite(item_data$theta) & is.finite(item_data$score)))

    empirical_values <- NULL
    frequency_values <- NULL

    if (empirical) {
      if (integ_numeric) {
        empirical_data <- data.frame(
          group = grouping$group,
          score = score
        )
        empirical_data <- empirical_data[
          !is.na(empirical_data$group) & is.finite(empirical_data$score),
          ,
          drop = FALSE
        ]

        if (nrow(empirical_data) > 0L) {
          score_groups <- sort(unique(empirical_data$group))
          score_summary <- do.call(
            rbind,
            lapply(score_groups, function(g) {
              x <- empirical_data$score[empirical_data$group == g]
              data.frame(
                group = g,
                score = mean(x),
                n_item = length(x)
              )
            })
          )
          rownames(score_summary) <- NULL

          empirical_values <- merge(
            grouping$summary,
            score_summary,
            by = "group",
            all.x = FALSE,
            all.y = FALSE,
            sort = TRUE
          )
        }
      } else {
        complete_item <- item_data[
          is.finite(item_data$theta) & is.finite(item_data$score),
          ,
          drop = FALSE
        ]

        if (nrow(complete_item) > 0L) {
          ## Mean theta for each observed item score.
          empirical_values <- stats::aggregate(
            theta ~ score,
            data = complete_item,
            FUN = mean
          )
          empirical_values <- empirical_values[order(empirical_values$score), ]

          ## Frequencies of exact theta-score combinations.
          frequency_values <- stats::aggregate(
            rep(1L, nrow(complete_item)),
            by = list(
              theta = complete_item$theta,
              score = complete_item$score
            ),
            FUN = length
          )
          names(frequency_values)[3L] <- "frequency"
        }
      }
    }

    plot_values[[j]] <- list(
      item_number = threshold_row,
      response_column = response_column,
      item_name = item_names[j],
      thresholds = threshold,
      categories = categories,
      theta = theta_theo,
      expected_score = expected_score,
      empirical = empirical_values,
      frequencies = frequency_values,
      n_missing = n_missing
    )
  }

  ## ------------------------------------------------------------------
  ## 4. Create one common coordinate system
  ## ------------------------------------------------------------------
  if (is.null(main)) {
    main <- if (n_items == 1L) {
      paste0("Expected Score Curve for Item ", item_names)
    } else {
      "Expected Score Curves"
    }
  }

  if (is.null(sub)) {
    if (n_items == 1L) {
      sub <- paste0("n missing: ", plot_values[[1L]]$n_missing)
    } else if (empirical && integ_numeric) {
      sub <- paste0(
        if (black_and_white) {
          "Line type/symbol = item; points = empirical means; grouping: "
        } else {
          "Colour/symbol = item; solid = model; dashed/points = empirical; grouping: "
        },
        grouping$method
      )
    } else {
      sub <- if (black_and_white) {
        "Line type/symbol = item; points = empirical means"
      } else {
        "Colour/symbol = item; solid = model; dashed/points = empirical means"
      }
    }
  }

  graphics::plot(
    x = range(theta_theo),
    y = c(0, maximum_score),
    type = "n",
    bty = "n",
    ylab = "Expected score",
    xlab = "Theta (ability logits)",
    main = main,
    sub = sub,
    ...
  )

  ## Model-implied expected score curves.
  for (j in seq_along(plot_values)) {
    graphics::lines(
      x = plot_values[[j]]$theta,
      y = plot_values[[j]]$expected_score,
      col = col[j],
      lty = item_lty[j],
      lwd = lwd
    )
  }

  ## Empirical values.
  if (empirical) {
    for (j in seq_along(plot_values)) {
      empirical_values <- plot_values[[j]]$empirical

      if (!is.null(empirical_values) && nrow(empirical_values) > 0L) {
        if (integ_all) {
          frequency_values <- plot_values[[j]]$frequencies

          if (!is.null(frequency_values) && nrow(frequency_values) > 0L) {
            graphics::symbols(
              x = frequency_values$theta,
              y = frequency_values$score,
              circles = sqrt(frequency_values$frequency),
              inches = 0.09,
              add = TRUE,
              fg = grDevices::adjustcolor(col[j], alpha.f = 0.55)
            )
          }
        }

        graphics::lines(
          x = empirical_values$theta,
          y = empirical_values$score,
          type = "o",
          col = col[j],
          lty = empirical_item_lty[j],
          lwd = empirical.lwd,
          pch = pch[j]
        )
      }
    }
  }

  ## ------------------------------------------------------------------
  ## 5. Legends
  ## ------------------------------------------------------------------
  if (show.legend && n_items > 1L) {
    graphics::legend(
      legend.position,
      legend = item_names,
      col = col,
      lty = item_lty,
      lwd = lwd,
      pch = pch,
      bty = "n",
      title = "Item"
    )
  }

  if (show.legend && empirical) {
    type_labels <- if (integ_all) {
      c("Model expectation", "Empirical score mean", "Observed frequency")
    } else {
      c("Model expectation", "Empirical group mean")
    }

    type_lty <- if (integ_all) {
      c(item_lty[1L], empirical_item_lty[1L], NA)
    } else {
      c(item_lty[1L], empirical_item_lty[1L])
    }
    type_pch <- if (integ_all) {
      c(NA, pch[1L], 1)
    } else {
      c(NA, pch[1L])
    }
    type_lwd <- if (integ_all) {
      c(lwd, empirical.lwd, NA)
    } else {
      c(lwd, empirical.lwd)
    }

    graphics::legend(
      type.legend.position,
      legend = type_labels,
      col = rep("black", length(type_labels)),
      lty = type_lty,
      lwd = type_lwd,
      pch = type_pch,
      bty = "n"
    )
  }

  invisible(list(
    grouping = grouping,
    items = plot_values,
    styles = list(
      black_and_white = black_and_white,
      col = col,
      item_lty = item_lty,
      empirical_lty = empirical_item_lty,
      pch = pch
    )
  ))
}
