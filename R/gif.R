## Convert vectors from pairwise result objects to numeric values while
## preserving already-numeric vectors.
i.gif_as_numeric <- function(x, object_name = "vector") {
  if (is.factor(x)) {
    value <- suppressWarnings(as.numeric(as.character(x)))
  } else {
    value <- suppressWarnings(as.numeric(x))
  }
  
  if (all(is.na(value)) && any(!is.na(x))) {
    stop("'", object_name, "' cannot be converted to numeric values.")
  }
  
  value
}


## Numerically stable category probabilities of the Partial Credit Model.
## This is equivalent to pvx.matrix(theta_v, thres) and avoids a dependency on
## the older scalar pvx() helper.
i.gif_pvx_matrix <- function(theta_v, thres) {
  score <- 0:length(thres)
  thres0 <- c(0, thres)
  
  eta <- outer(score, theta_v, "*")
  eta <- sweep(eta, MARGIN = 1L, STATS = cumsum(thres0), FUN = "-")
  
  ## Stabilise exponentiation separately at every theta value.
  eta <- sweep(eta, MARGIN = 2L, STATS = apply(eta, 2L, max), FUN = "-")
  numerator <- exp(eta)
  probability <- sweep(
    numerator,
    MARGIN = 2L,
    STATS = colSums(numerator),
    FUN = "/"
  )
  
  rownames(probability) <- paste0("cat.", score)
  colnames(probability) <- theta_v
  probability
}


## Detect an explicitly requested black-and-white item display.
i.gif_is_black <- function(col) {
  if (is.null(col) || length(col) < 1L) {
    return(FALSE)
  }
  
  col_character <- tolower(as.character(col))
  all(col_character %in% c("black", "#000000", "#000000ff", "1"))
}


## Summarise a common grouping. Median theta is used as plotting coordinate,
## matching the original gif() function; mean theta is returned as well.
i.gif_group_summary <- function(theta, group, group_value = NULL) {
  valid <- is.finite(theta) & !is.na(group)
  theta <- theta[valid]
  group <- group[valid]
  
  if (!is.null(group_value)) {
    group_value <- group_value[valid]
  }
  
  group_levels <- sort(unique(group))
  
  summary <- do.call(
    rbind,
    lapply(group_levels, function(g) {
      index <- group == g
      value <- if (is.null(group_value)) g else unique(group_value[index])[1L]
      
      data.frame(
        group = g,
        group_value = value,
        theta = stats::median(theta[index]),
        theta_mean = mean(theta[index]),
        n = sum(index),
        theta_min = min(theta[index]),
        theta_max = max(theta[index])
      )
    })
  )
  
  rownames(summary) <- NULL
  summary
}


## Form common WLE groups without splitting ties when requested.
i.gif_make_groups <- function(theta, integ,
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
    
    ## Adjacent distinct WLE levels are combined. Equal WLEs therefore always
    ## belong to the same empirical group.
    theta_level_group <- ceiling(
      seq_along(unique_theta) * number_groups / n_unique
    )
    group[valid] <- theta_level_group[match(theta_valid, unique_theta)]
  } else {
    number_groups <- requested_groups
    ordered_positions <- order(theta_valid, seq_along(theta_valid))
    ordered_group <- ceiling(seq_len(n_valid) * number_groups / n_valid)
    group_valid <- integer(n_valid)
    group_valid[ordered_positions] <- ordered_group
    group[valid] <- group_valid
  }
  
  list(
    group = group,
    summary = i.gif_group_summary(theta, group),
    method = method_used,
    requested_groups = as.integer(integ),
    actual_groups = length(unique(stats::na.omit(group))),
    n_unique_theta = n_unique,
    largest_tie_proportion = largest_tie
  )
}


## Form common raw-score groups.
i.gif_make_raw_groups <- function(theta, raw) {
  valid <- is.finite(theta) & is.finite(raw)
  if (!any(valid)) {
    stop("No complete finite WLE and raw-score values are available.")
  }
  
  raw_levels <- sort(unique(raw[valid]))
  group <- rep(NA_integer_, length(theta))
  group[valid] <- match(raw[valid], raw_levels)
  
  list(
    group = group,
    summary = i.gif_group_summary(
      theta = theta,
      group = group,
      group_value = raw
    ),
    method = "raw",
    requested_groups = length(raw_levels),
    actual_groups = length(raw_levels),
    n_unique_theta = length(unique(theta[is.finite(theta)])),
    largest_tie_proportion = max(table(theta[is.finite(theta)])) /
      sum(is.finite(theta))
  )
}




#' @title Graphical item-fit plots for one or several items
#' @export gif
#' @keywords item fit
#' @description Plots model-implied category probability curves from a Rasch or Partial Credit Model together with empirical category probabilities. Several items can be drawn in one common coordinate system. 
#' @details For several items in colour, colour and the empirical plotting symbol identify the item, whereas line type identifies the response category. With \code{col = "black"}, line type and the empirical plotting symbol identify the item; sparse symbols on the model curves identify the response category. Model-implied probabilities are shown as lines; empirical probabilities are superimposed as lines with item-specific points. The empirical ability groups are formed once for all selected items. For a numeric value of \code{integ}, ties in WLE estimates are not split across groups when \code{group.method = "theta_levels"} is used. The default \code{"auto"} chooses this method when the WLE distribution contains substantial ties.
#' @param pers_obj an object of class \code{"pers"} as a result from function \code{\link{pers}}.
#' @param itemnumber NULL, a numeric vector of item positions, or a character
#'   vector of item names. The default NULL selects all items contained in
#'   pers_obj$pair$threshold.
#' @param ra Positive number defining the theoretical theta range -ra to ra.
#' @param nodes Number of theta evaluation points for the model curves.
#' @param integ Either a positive integer giving the requested number of common
#'   empirical ability groups, or "raw" for grouping by the raw score stored in
#'   pers_obj$pers$raw.
#' @param group.method Method used when `integ` is numeric: "auto",
#'   "theta_levels", or "equal_n".
#' @param kat Either "all" or a numeric vector of category numbers. For
#'   compatibility with the original function, category number 1 denotes the
#'   first response category (score 0), category number 2 denotes score 1, etc.
#' @param col Item colour(s). If NULL, colours are generated automatically.
#'   If all selected items are drawn with col = "black", items are
#'   automatically distinguished by different line types.
#' @param item.lty Line types used to distinguish items in a black-and-white
#'   plot. They are recycled when necessary.
#' @param category.lty Line types used to distinguish response categories in
#'   colour plots. In black-and-white plots, categories are distinguished by
#'   plotting symbols instead.
#' @param item.pch Plotting symbols used for the empirical points of the
#'   selected items. The vector is recycled when necessary.
#' @param category.pch Plotting symbols used to distinguish response
#'   categories on model curves in black-and-white plots.
#' @param model.marker.every Approximate spacing, in theta nodes, between
#'   category symbols placed on model curves in black-and-white plots.
#' @param model.marker.cex Size of those model-curve category symbols.
#' @param model.lwd Line width of model-implied probability curves.
#' @param empirical.lwd Line width of empirical probability curves.
#' @param empirical Logical; add empirical category probabilities?
#' @param show.legend Logical; draw legends?
#' @param item.legend.position Position of the item legend.
#' @param category.legend.position Position of the response-category legend.
#' @param type.legend.position Position of the model/empirical legend.
#' @param label.model.categories NULL or logical. If NULL, category labels at
#'   the model-curve maxima are shown for a single item and omitted for several
#'   items.
#' @param label.empirical.categories NULL or logical. If NULL, labels at the
#'   empirical points are shown for a single item and omitted for several
#'   items.
#' @param main,sub Optional plot title and subtitle.
#' @param verbose Logical; print information about the empirical grouping?
#' @param ... Further arguments passed to plot().
#'
#' @return Invisibly returns a list containing grouping information and all theoretical and empirical plotting values.
#' @examples ########
#' data(bfiN)
#' pers_obj <- pers(pair(bfiN))
#' #### plot 
#' # gif(pers_obj)                                  # all items
#' # gif(pers_obj, itemnumber = 1:4)               # selected items
#' # gif(pers_obj, itemnumber = c("i1", "i2"))   # item names
#' # gif(pers_obj, itemnumber = 1:4, integ = 6)
#' # gif(pers_obj, itemnumber = 1:4, integ = 6,
#' #     group.method = "theta_levels", kat = 1)
#' # gif(pers_obj, itemnumber = 1:4, col = "black")
#' ####################################################

gif <- function(pers_obj,
                itemnumber = NULL,
                ra = 4,
                nodes = 100,
                integ = "raw",
                group.method = c("auto", "theta_levels", "equal_n"),
                kat = "all",
                col = NULL,
                item.lty = c(
                  "solid", "dashed", "dotted", "dotdash",
                  "longdash", "twodash", "44", "13",
                  "1343", "73", "2262", "12223242"
                ),
                category.lty = 1:6,
                item.pch = c(
                  1, 2, 0, 5, 6, 3, 4, 8, 9, 10,
                  11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                  21, 22, 23, 24, 25
                ),
                category.pch = 1:25,
                model.marker.every = 15,
                model.marker.cex = 0.55,
                model.lwd = 1.5,
                empirical.lwd = 1.5,
                empirical = TRUE,
                show.legend = TRUE,
                item.legend.position = "topleft",
                category.legend.position = "topright",
                type.legend.position = "bottomright",
                label.model.categories = NULL,
                label.empirical.categories = NULL,
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
  theta_wle <- i.gif_as_numeric(pers_obj$pers$WLE, "pers_obj$pers$WLE")

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

  nodes <- as.integer(round(nodes))
  theta_theo <- seq(-ra, ra, length.out = nodes)

  integ_raw <- is.character(integ) && length(integ) == 1L && integ == "raw"
  integ_numeric <- is.numeric(integ) && length(integ) == 1L &&
    !is.na(integ) && integ >= 1

  if (!integ_raw && !integ_numeric) {
    stop("'integ' must be a positive integer or the character string 'raw'.")
  }

  if (integ_numeric) {
    integ <- as.integer(round(integ))
  }

  kat_all <- is.character(kat) && length(kat) == 1L && kat == "all"
  kat_numeric <- is.numeric(kat) && length(kat) >= 1L &&
    all(is.finite(kat)) && all(kat >= 1) && all(kat == round(kat))

  if (!kat_all && !kat_numeric) {
    stop(
      "'kat' must be 'all' or positive integer category numbers. ",
      "Category number 1 denotes response score 0."
    )
  }

  if (kat_numeric) {
    kat <- as.integer(kat)
    kat <- kat[!duplicated(kat)]
  }

  ## ------------------------------------------------------------------
  ## 2. Form one common empirical grouping for all selected items
  ## ------------------------------------------------------------------
  grouping <- NULL
  if (empirical) {
    if (integ_raw) {
      if (is.null(pers_obj$pers$raw)) {
        stop("For integ = 'raw', pers_obj$pers$raw is required.")
      }
      raw_score <- i.gif_as_numeric(pers_obj$pers$raw, "pers_obj$pers$raw")
      if (length(raw_score) != length(theta_wle)) {
        stop("The number of raw-score values does not equal the number of WLE values.")
      }
      grouping <- i.gif_make_raw_groups(theta_wle, raw_score)
    } else {
      grouping <- i.gif_make_groups(
        theta = theta_wle,
        integ = integ,
        group.method = group.method
      )
    }

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
  maximum_category <- 0L
  number_curves <- 0L

  for (j in seq_along(item_index)) {
    threshold_row <- item_index[j]
    response_column <- response_index[j]

    threshold <- as.numeric(stats::na.omit(thresholds[threshold_row, ]))
    if (length(threshold) < 1L) {
      stop("No estimable threshold was found for item '", item_names[j], "'.")
    }

    categories <- 0:length(threshold)
    maximum_category <- max(maximum_category, max(categories))

    category_position <- if (kat_all) seq_along(categories) else kat
    if (any(category_position > length(categories))) {
      stop(
        "Item '", item_names[j], "' has only ", length(categories),
        " categories, but category number ", max(category_position),
        " was requested."
      )
    }
    selected_categories <- categories[category_position]
    number_curves <- number_curves + length(selected_categories)

    probability_matrix <- i.gif_pvx_matrix(
      theta_v = theta_theo,
      thres = threshold
    )

    response <- i.gif_as_numeric(
      responses[[response_column]],
      paste0("responses for item '", item_names[j], "'")
    )

    n_missing <- sum(!(is.finite(theta_wle) & is.finite(response)))
    empirical_values <- NULL

    if (empirical) {
      empirical_data <- data.frame(
        group = grouping$group,
        response = response
      )
      empirical_data <- empirical_data[
        !is.na(empirical_data$group) & is.finite(empirical_data$response),
        ,
        drop = FALSE
      ]

      if (nrow(empirical_data) > 0L) {
        group_levels <- grouping$summary$group

        empirical_values <- do.call(
          rbind,
          lapply(group_levels, function(g) {
            response_g <- empirical_data$response[empirical_data$group == g]
            if (length(response_g) == 0L) {
              return(NULL)
            }

            group_row <- grouping$summary[grouping$summary$group == g, , drop = FALSE]

            do.call(
              rbind,
              lapply(selected_categories, function(category) {
                data.frame(
                  group = g,
                  group_value = group_row$group_value,
                  theta = group_row$theta,
                  theta_mean = group_row$theta_mean,
                  n_group = group_row$n,
                  category = category,
                  probability = mean(response_g == category),
                  n_item = length(response_g)
                )
              })
            )
          })
        )

        if (!is.null(empirical_values)) {
          rownames(empirical_values) <- NULL
        }
      }
    }

    plot_values[[j]] <- list(
      item_number = threshold_row,
      response_column = response_column,
      item_name = item_names[j],
      thresholds = threshold,
      categories = categories,
      selected_categories = selected_categories,
      theta = theta_theo,
      probability = probability_matrix,
      empirical = empirical_values,
      n_missing = n_missing
    )
  }

  if (number_curves > 30L) {
    warning(
      number_curves, " model curves will be drawn. ",
      "Consider selecting fewer items or categories with 'itemnumber' or 'kat'.",
      call. = FALSE
    )
  }

  ## ------------------------------------------------------------------
  ## 4. Prepare graphical encodings
  ## ------------------------------------------------------------------
  black_and_white <- i.gif_is_black(col) && n_items > 1L

  if (length(item.lty) < 1L) {
    stop("'item.lty' must contain at least one valid line type.")
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
  item_lty <- rep_len(item.lty, n_items)

  if (length(item.pch) < 1L) {
    stop("'item.pch' must contain at least one plotting symbol.")
  }
  item_pch <- rep_len(item.pch, n_items)

  category.lty <- rep_len(category.lty, maximum_category + 1L)
  category.pch <- rep_len(category.pch, maximum_category + 1L)

  if (!is.numeric(model.marker.every) || length(model.marker.every) != 1L ||
      is.na(model.marker.every) || model.marker.every < 1) {
    stop("'model.marker.every' must be one positive number.")
  }
  if (!is.numeric(model.marker.cex) || length(model.marker.cex) != 1L ||
      is.na(model.marker.cex) || model.marker.cex <= 0) {
    stop("'model.marker.cex' must be one positive number.")
  }
  model.marker.every <- as.integer(round(model.marker.every))

  if (is.null(label.model.categories)) {
    label.model.categories <- n_items == 1L
  }
  if (is.null(label.empirical.categories)) {
    label.empirical.categories <- n_items == 1L
  }

  if (is.null(main)) {
    main <- if (n_items == 1L) {
      paste0(
        "Model-derived and empirical\ncategory probability curves for item ",
        item_names
      )
    } else {
      "Model-derived and empirical category probability curves"
    }
  }

  if (is.null(sub)) {
    if (n_items == 1L) {
      sub <- paste0(
        "n missing: ", plot_values[[1L]]$n_missing,
        if (empirical) paste0("; grouping: ", grouping$method) else ""
      )
    } else if (empirical) {
      sub <- paste0(
        if (black_and_white) {
          "Line type/empirical symbol = item; model symbol = category; grouping: "
        } else {
          "Colour/empirical symbol = item; line type = category; grouping: "
        },
        grouping$method
      )
    } else {
      sub <- if (black_and_white) {
        "Line type/empirical symbol = item; model symbol = category"
      } else {
        "Colour/empirical symbol = item; line type = category"
      }
    }
  }

  ## ------------------------------------------------------------------
  ## 5. Create one common coordinate system and draw curves
  ## ------------------------------------------------------------------
  graphics::plot(
    x = range(theta_theo),
    y = c(0, 1),
    type = "n",
    bty = "n",
    xaxt = "n",
    ylab = "Category probability",
    xlab = "Theta (ability logits)",
    main = main,
    sub = sub,
    ...
  )

  axis_ticks <- seq(ceiling(-ra), floor(ra), by = 1)
  graphics::axis(1, at = axis_ticks, labels = axis_ticks)

  ## Model-implied category probability curves.
  for (j in seq_along(plot_values)) {
    item <- plot_values[[j]]

    for (category in item$selected_categories) {
      style_index <- category + 1L

      curve_lty <- if (black_and_white) {
        item_lty[j]
      } else {
        category.lty[style_index]
      }

      graphics::lines(
        x = item$theta,
        y = item$probability[style_index, ],
        col = col[j],
        lty = curve_lty,
        lwd = model.lwd
      )

      ## In black-and-white plots, sparse symbols retain the category coding
      ## that would otherwise have been carried by the line type.
      if (black_and_white) {
        marker_index <- unique(c(
          1L,
          seq.int(1L, length(item$theta), by = model.marker.every),
          length(item$theta)
        ))
        graphics::points(
          x = item$theta[marker_index],
          y = item$probability[style_index, marker_index],
          pch = category.pch[style_index],
          cex = model.marker.cex,
          col = col[j]
        )
      }

      if (isTRUE(label.model.categories)) {
        probability <- item$probability[style_index, ]
        maximum_index <- which.max(probability)
        graphics::text(
          x = item$theta[maximum_index],
          y = probability[maximum_index],
          labels = paste0("cat.", category),
          pos = 1,
          cex = 0.7,
          col = col[j]
        )
      }
    }
  }

  ## Empirical category probabilities.
  if (empirical) {
    for (j in seq_along(plot_values)) {
      empirical_values <- plot_values[[j]]$empirical
      if (is.null(empirical_values) || nrow(empirical_values) == 0L) {
        next
      }

      for (category in plot_values[[j]]$selected_categories) {
        style_index <- category + 1L
        category_data <- empirical_values[
          empirical_values$category == category,
          ,
          drop = FALSE
        ]
        category_data <- category_data[order(category_data$theta), , drop = FALSE]

        graphics::lines(
          x = category_data$theta,
          y = category_data$probability,
          type = "o",
          col = col[j],
          lty = if (black_and_white) item_lty[j] else category.lty[style_index],
          lwd = empirical.lwd,
          pch = item_pch[j]
        )

        if (isTRUE(label.empirical.categories)) {
          graphics::text(
            x = category_data$theta,
            y = category_data$probability,
            labels = paste0("cat.", category),
            cex = 0.7,
            pos = 3,
            col = col[j]
          )
        }
      }
    }
  }

  ## ------------------------------------------------------------------
  ## 6. Legends
  ## ------------------------------------------------------------------
  if (show.legend && n_items > 1L) {
    graphics::legend(
      item.legend.position,
      legend = item_names,
      col = col,
      lty = if (black_and_white) item_lty else 1,
      lwd = model.lwd,
      pch = item_pch,
      bty = "n",
      title = "Item"
    )
  }

  if (show.legend) {
    displayed_categories <- sort(unique(unlist(
      lapply(plot_values, function(x) x$selected_categories)
    )))
    style_index <- displayed_categories + 1L

    graphics::legend(
      category.legend.position,
      legend = paste0("Category ", displayed_categories),
      col = "black",
      lty = if (black_and_white) NA else category.lty[style_index],
      lwd = if (black_and_white) NA else model.lwd,
      pch = if (black_and_white) category.pch[style_index] else NA,
      bty = "n",
      title = "Response category"
    )
  }

  if (show.legend && empirical) {
    graphics::legend(
      type.legend.position,
      legend = c("Model probability", "Empirical probability"),
      col = "black",
      lty = if (black_and_white) item_lty[1L] else 1,
      lwd = c(model.lwd, empirical.lwd),
      pch = c(NA, item_pch[1L]),
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
      item_pch = item_pch,
      category_lty = category.lty,
      model_marker_every = model.marker.every,
      model_marker_cex = model.marker.cex,
      category_pch = category.pch
    )
  ))
}
