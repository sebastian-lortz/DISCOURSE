#' Internal helper functions for discourse package
#'
#' @description Utility functions for data handling and calculations
#'
#' @return Various helper outputs
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom rlang .data
#' @importFrom stats aov as.formula coef formula lm model.frame reformulate resid sd setNames terms
#' @importFrom utils combn head str write.csv
#' @noRd
NULL

count_decimals <- function(vec, min_decimals = 0) {
  sapply(as.character(vec), function(x) {
    # Check if there's a decimal point in the string.
    if (grepl("\\.", x)) {
      # Split the string at the decimal point and return the number of digits after it.
      parts <- strsplit(x, "\\.", fixed = FALSE)[[1]]
      nchar(parts[2])
    } else {
      min_decimals
    }
  })
}

get_design <- function(candidate, reg_equation, terms_obj) {
 p         <- ncol(candidate)

  # check if names are provided
  main_names <- colnames(candidate)
  if (is.null(main_names)) {
    stop("Candidate predictors must have column names.")
  }

  # Build interaction names in the same order as in C++
  interaction_names <- c()
  if (p > 1) {
    for (i in 1:(p - 1)) {
      for (j in (i + 1):p) {
        interaction_names <- c(interaction_names, paste0(main_names[i], ":", main_names[j]))
      }
    }
  }

  # add intercept and main names
  full_names <- c("(Intercept)", main_names, interaction_names)

  # extract target names
  target_names <- c("(Intercept)", labels(terms_obj))


  # match and extract the desired position
  positions <- vapply(target_names, function(nm) {
    # try exact
    pos <- match(nm, full_names)
    # if it’s an interaction and not found, swap sides and try again
    if (is.na(pos) && grepl(":", nm)) {
      parts <- strsplit(nm, ":", fixed = TRUE)[[1]]
      rev_nm <- paste(parts[2], parts[1], sep = ":")
      pos <- match(rev_nm, full_names)
    }
    if (is.na(pos)) stop("Term not in design: ", nm)
    pos
  }, integer(1))

  return(list(
    target_names = target_names,
    full_names = full_names,
    positions = positions
  ))
}

is_integer_vector <- function(vec, tol = .Machine$double.eps^0.5) {
  all(abs(vec - round(vec)) < tol)
}


long_to_wide <- function(data) {
  if (!is.data.frame(data)) stop("Input must be a data.frame.")
  if (ncol(data) < 3) stop("Need: ID + time + at least 1 measure.")

  participant_col <- names(data)[1]
  time_col <- "time"
  if (!time_col %in% names(data)) stop("'time' column not found")

  time_index <- which(names(data) == time_col)
  # If time column comes immediately after ID, no between-subject columns exist
  if (time_index > 2) {
    between_cols <- names(data)[2:(time_index - 1)]
  } else {
    between_cols <- character(0)
  }

  value_cols <- setdiff(names(data), c(participant_col, between_cols, time_col))

  wide_data <- tidyr::pivot_wider(
    data,
    id_cols     = tidyselect::all_of(c(participant_col, between_cols)),
    names_from  = tidyselect::all_of(time_col),
    values_from = tidyselect::all_of(value_cols),
    names_glue  = "{.value}_{time}"
  )

  as.data.frame(wide_data)
}


wide_to_long <- function(data) {
  # Convert matrix to data.frame if needed
  if (!is.data.frame(data) && !is.matrix(data)) stop("Input must be a data.frame or matrix.")
  if (ncol(data) < 3) stop("Need the data in wide format.")
  if (is.matrix(data)) data <- as.data.frame(data)

  # Ensure first column is named "ID"
  if (!"ID" %in% names(data)) {
    data <- cbind(1:nrow(data),data)
    names(data)[1] <- "ID"
  }

  # Identify id (between - subject) cols = those WITHOUT an underscore
  id_cols <- names(data)[!grepl("_", names(data))]
  if (is.null(id_cols)) stop("The data in wide format have to contain at least two repeated measures with columns named [var]_[time.index]; e.g. V1_1, V2_2")

  # check for at least two var_time columns
  vt <- grep("^[^_]+_[0-9]+$", names(data), value = TRUE)
  if (length(vt) < 2) {
    stop("Need at least two repeated‐measure columns named like V1_1, V1_2.")
  }
  data %>%
    tidyr::pivot_longer(
      cols       = -tidyselect::all_of(id_cols),
      names_to   = c(".value", "time"),
      names_sep  = "_"
    ) %>%
    # convert time from character integer
    dplyr::mutate(time = as.integer(.data$time)) %>%
    as.data.frame()
}


heuristic_move <- function(candidate, target_sd, range) {
  lower_bound <- range[1]
  upper_bound <- range[2]

  # Precompute current SD and maximum value
  current_sd <- stats::sd(candidate)
  increaseSD <- (current_sd < target_sd)
  cand_max <- max(candidate) # dont exceed the current max.

  # Select candidate for decrement
  # Eligible indices: candidate > lower_bound and, if increasing SD, avoid the maximum.
  dec_idx <- which(candidate > lower_bound & (!increaseSD | candidate < cand_max))
  if (length(dec_idx) == 0) return(candidate)
  i_dec <- sample(dec_idx, 1)

  # Select candidate for increment
  # Eligible indices: candidate < upper_bound.
  inc_idx <- which(candidate < upper_bound)
  # If NOT increasing SD, prefer only those less than candidate[i_dec].
  if (!increaseSD) {
    inc_idx <- inc_idx[candidate[inc_idx] < candidate[i_dec]]
  }
  if (length(inc_idx) == 0) return(candidate)
  i_inc <- sample(inc_idx, 1)

  # Determine possible delta values
  max_dec <- candidate[i_dec] - lower_bound
  max_inc <- upper_bound - candidate[i_inc]
  max_delta <- floor(min(max_dec, max_inc))
  if (max_delta < 1) return(candidate)

  # Sample delta from 1:max_delta (using sample.int for speed)
  delta <- sample.int(max_delta, 1)

  # Apply the move
  candidate[i_dec] <- candidate[i_dec] - delta
  candidate[i_inc] <- candidate[i_inc] + delta

  candidate
}


factor_matrix <- function(sample_size, levels, subgroup_sizes = NULL) {
  # Generate the full factorial design.
  design <- expand.grid(lapply(levels, seq_len))
  # Ensure lexicographic order: order by the first factor, then the second, etc.
  design <- design[do.call(order, design), , drop = FALSE]

  total_combinations <- nrow(design)

  # If subgroup_sizes is provided, check its length; otherwise, equally divide sample_size.
  if (!is.null(subgroup_sizes)) {
    if (length(subgroup_sizes) != total_combinations) {
      stop("Length of subgroup_sizes must equal the total number of combinations: ", total_combinations)
    }
    total_sample_size <- sum(subgroup_sizes)
  } else {
    base_size <- floor(sample_size / total_combinations)
    remainder <- sample_size %% total_combinations
    subgroup_sizes <- rep(base_size, total_combinations)
    if (remainder > 0) {
      cat("Please note: unequal group sizes.\n")
      subgroup_sizes[1:remainder] <- subgroup_sizes[1:remainder] + 1
    }
    total_sample_size <- sample_size
  }

  # Replicate each combination the specified number of times.
  design_list <- mapply(function(row, rep_n) {
    matrix(unlist(rep(row, rep_n)), ncol = length(row), byrow = TRUE)
  }, split(design, seq(nrow(design))), subgroup_sizes, SIMPLIFY = FALSE)

  full_design <- do.call(rbind, design_list)
  full_design <- as.matrix(full_design)
  colnames(full_design) <- paste0("Factor", seq_len(ncol(full_design)))
  rownames(full_design) <- NULL
  full_design <- lapply(as.data.frame(full_design), as.factor)
  return(as.data.frame(full_design))
}


mixed_factor_matrix <- function(sample_size, levels, factor_type, subgroup_sizes = NULL) {
  # Check that 'levels' and 'factor_type' have the same length.
  if(length(levels) != length(factor_type)) {
    stop("The length of 'levels' must equal the length of 'factor_type'.")
  }

  # Identify between and within factor indices.
  between_idx <- which(tolower(factor_type) == "between")
  within_idx  <- which(tolower(factor_type) == "within")

  # Generate the between-group factorial design (if any).
  if(length(between_idx) > 0) {
    between_design <- expand.grid(lapply(levels[between_idx], seq_len))
    between_design <- between_design[do.call(order, between_design), , drop = FALSE]
    n_between <- nrow(between_design)
    between.factor = TRUE
  } else {
    # No between factors: create a dummy design with one group.
    between_design <- data.frame(dummy = rep(1, sample_size))
    n_between <- sample_size
    between.factor = FALSE
  }

  # Generate the within-group factorial design (if any).
  if(length(within_idx) > 0) {
    within_design <- expand.grid(lapply(levels[within_idx], seq_len))
    within_design <- within_design[do.call(order, within_design), , drop = FALSE]
    n_within <- nrow(within_design)
  } else {
    within_design <- data.frame()
    n_within <- 1
  }

  # Determine subject allocation to between-group combinations.
  if(between.factor) {
    if(!is.null(subgroup_sizes)) {
      if(length(subgroup_sizes) != n_between) {
        stop("Length of subgroup_sizes must equal the number of between-group combinations: ", n_between)
      }
      subjects_per_group <- subgroup_sizes
      total_assigned <- sum(subjects_per_group)
      if(total_assigned != sample_size) {
        warning("Total subjects assigned in subgroup_sizes (", total_assigned,
                ") does not equal sample_size (", sample_size, ").")
      }
    } else {
      base_size <- floor(sample_size / n_between)
      remainder <- sample_size %% n_between
      subjects_per_group <- rep(base_size, n_between)
      if (remainder > 0) {
        cat("Please note: unequal group sizes.\n")
        subjects_per_group[1:remainder] <- subjects_per_group[1:remainder] + 1
      }
    }
  } else {
    subjects_per_group <- sample_size
  }

  # Replicate between-design rows according to subject allocation.
  if(between.factor) {
    full_between <- between_design[rep(1:n_between, subjects_per_group), , drop = FALSE]
  } else {
    full_between <- between_design
  }

  # For each subject in the between design, append the full within design.
  if(ncol(within_design) > 0) {
    subject_list <- lapply(seq_len(nrow(full_between)), function(i) {
      withCallingHandlers({
        temp <- cbind(unname(full_between[i, , drop = FALSE]), unname(within_design))
        as.data.frame(temp, stringsAsFactors = FALSE)
      }, warning = function(w) {
        invokeRestart("muffleWarning")
      })
    })
    final_df <- do.call(rbind, subject_list)
    if(!between.factor) {
      final_df <- final_df[,-1]
    }
  } else {
    final_df <- full_between
  }

  # Determine number of subjects.
  n_subjects <- if(between.factor) nrow(full_between) else sample_size

  # Add subject identifier ("ID") as the first column.
  if(ncol(within_design) > 0) {
    final_df <- cbind(ID = rep(1:n_subjects, each = n_within), final_df)
  } else {
    final_df <- cbind(ID = 1:n_subjects, final_df)
  }

  # Remove any row names to avoid warnings.
  rownames(final_df) <- NULL

  # Rename factor columns (all except ID) to "Factor1", "Factor2", etc.
  n_factor_cols <- ncol(final_df) - 1
  if(n_factor_cols > 0) {
    colnames(final_df)[-1] <- paste0("Factor", seq_len(n_factor_cols))
  }

  # Convert all factor columns (except ID) to factors.
  final_df[-1] <- lapply(final_df[-1], function(x) as.factor(x))

  # Attach attributes for reference.
  attr(final_df, "factor_type") <- factor_type
  attr(final_df, "sample_size") <- sample_size

  return(final_df)
}


calcMarginalMeans <- function(factor_mat, group_means, group_sizes) {
  # Convert the factor matrix to a data frame for easier handling
  df <- as.data.frame(factor_mat, stringsAsFactors = FALSE)

  # Append group means and sizes to the data frame
  df$group_mean <- group_means
  df$group_size <- group_sizes

  # Determine the number of factors (columns)
  n_factors <- ncol(factor_mat)

  # Initialize a list to store the marginal means for each factor
  marginal_means <- list()

  # Loop over each factor (each column)
  for (i in seq_len(n_factors)) {
    # Use the column name if available; otherwise, use a generic name
    factor_name <- if (!is.null(colnames(df))) colnames(df)[i] else paste0("Factor", i)

    # Identify all unique levels for the current factor
    levels_i <- unique(df[[i]])

    # Compute the weighted (by group_size) average for each level of this factor
    marginal_means[[factor_name]] <- sapply(levels_i, function(lvl) {
      idx <- which(df[[i]] == lvl)
      sum(df$group_mean[idx] * df$group_size[idx]) / sum(df$group_size[idx])
    })
  }

  return(marginal_means)
}


# Function to generate an integer candidate vector for one group
generate_candidate_group <- function(tMean, n, range) {
  if (tMean < range[1] || tMean > range[2]) {
    stop("Target mean is outside the allowable range.")
  }
  # Calculate total points (should be integer if target is GRIM-consistent)
  total_points <- round(tMean * n)
  # Compute base integer value and remainder
  base <- total_points %/% n
  remainder <- total_points %% n
  # Create a vector: 'remainder' values will be (base+1) and the rest will be base
  vec <- c(rep(base + 1, remainder), rep(base, n - remainder))
  return(vec)
}

compute_sequential_MSE <- function(means, sizes, uniq_factor_mat, F_values, df_effects) {
  SS <- compute_sequential_SS(means = means, sizes = sizes, uniq_factor_mat = uniq_factor_mat)
  # Combine main effects' SS with the interaction SS.
  SS_effects <- c(unlist(SS$sequential_SS), Interaction = SS$SS_interaction)
  SS_effects <- SS_effects[SS_effects > 0]
  # Compute mean squares for each effect.
  MS_effects <- SS_effects / df_effects
  # Back-calculate the error mean square from the reported F values:
  # F = MS_effect / MS_error  => MS_error = MS_effect / F.
  # We take the average over the effects.
  if (length(F_values) == 1) {
    MS_error <-  MS_effects / F_values
  } else {
    MS_error <- mean(MS_effects / F_values, na.rm = TRUE)
  }
  return(MS_error)
}

compute_sequential_SS <- function(means, sizes, uniq_factor_mat) {
  # Build the data frame.
  df <- data.frame(Y = means, uniq_factor_mat, weights = sizes)

  # Fit the intercept-only model.
  mod0 <- stats::lm(Y ~ 1, data = df, weights = df$weights)
  SS_total <- sum(df$weights * (df$Y - stats::coef(mod0))^2)

  sequential_SS <- list()
  prev_model <- mod0
  predictors <- character(0)

  # Loop over each factor (column in factor_mat) in sequence.
  for (factor_name in colnames(uniq_factor_mat)) {
    predictors <- c(predictors, factor_name)
    formula_str <- paste("Y ~", paste(predictors, collapse = " + "))
    mod <- stats::lm(stats::as.formula(formula_str), data = df, weights = df$weights)
    # The SS for this effect is the reduction in weighted error from the previous model.
    SS_effect <- sum(df$weights * (stats::predict(mod) - stats::predict(prev_model))^2)
    sequential_SS[[factor_name]] <- SS_effect
    prev_model <- mod
  }

  # Now, to get the additional variance explained by the interactions,
  # fit the full model including all interactions.
  full_formula <- stats::as.formula(paste("Y ~", paste(colnames(uniq_factor_mat), collapse = " * ")))
  mod_full <- stats::lm(full_formula, data = df, weights = df$weights)
  SS_interaction <- sum(df$weights * (stats::predict(mod_full) - stats::predict(prev_model))^2)

  return(list(
    SS_total_between = SS_total,
    sequential_SS = sequential_SS,
    SS_interaction = SS_interaction
  ))
}


# plots histograms given the data
plot_histogram <- function(df, tol = 1e-8, SD = TRUE) {
  is_int <- function(x) all(abs(x - round(x)) < tol)

  plots <- lapply(names(df), function(var) {
    x   <- df[[var]]
    m   <- mean(x, na.rm = TRUE)
    s   <- stats::sd(x,   na.rm = TRUE)
    if (SD) {
    lbl <- sprintf("M = %.2f\nSD = %.2f", m, s)
    } else {
      lbl <- paste0("M = ", round(m,1))
}
    if (is_int(x)) {
      # frequency bar chart
      tbl <- as.data.frame(table(x, useNA = "no"))
      names(tbl) <- c("Value", "Count")
      tbl$Value <- as.numeric(as.character(tbl$Value))

      p <- ggplot2::ggplot(tbl, ggplot2::aes(x = .data$Value, y = .data$Count)) +
        ggplot2::geom_col(fill = "steelblue") +
        ggplot2::geom_vline(xintercept = m,
                            linetype   = "dashed",
                            linewidth  = 1,
                            color      = "darkgrey") +
        ggplot2::annotate("text",
                          x      = Inf, y = Inf,
                          hjust  = 1.1, vjust = 1.5,
                          label  = lbl,
                          size   = 4) +
        ggplot2::labs(title = paste("Frequency of", var),
                      x     = var,
                      y     = "Count")
    } else {
      # histogram
      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[var]])) +
        ggplot2::geom_histogram(binwidth = (max(x, na.rm=TRUE)-min(x, na.rm=TRUE))/30,
                                fill     = "steelblue") +
        ggplot2::geom_vline(xintercept = m,
                            linetype   = "dashed",
                            linewidth  = 1,
                            color      = "darkgrey") +
        ggplot2::annotate("text",
                          x      = Inf, y = Inf,
                          hjust  = 1.1, vjust = 1.5,
                          label  = lbl,
                          size   = 4) +
        ggplot2::labs(title = paste("Distribution of", var),
                      x     = var,
                      y     = "Count")
    }

    p +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        axis.text.x     = ggplot2::element_text(hjust = 1),
        legend.position = "none",
        plot.title      = ggplot2::element_text(face = "bold")
      )
  })

  names(plots) <- names(df)
  plots
}

# partial regression plots given a model
plot_partial_regression <- function(model) {
  # extract the exact data used to fit
  df_orig    <- as.data.frame(model.frame(model))
  fm         <- stats::formula(model)
  resp_name  <- as.character(fm)[2]
  term_labels<- attr(stats::terms(model), "term.labels")

  # make syntactically valid names
  safe_labels <- make.names(term_labels, unique = TRUE)

  # copy & augment df with interaction cols if needed
  df <- df_orig
  for (i in seq_along(term_labels)) {
    if (term_labels[i] != safe_labels[i]) {
      parts <- strsplit(term_labels[i], ":", fixed=TRUE)[[1]]
      df[[ safe_labels[i] ]] <- df[[ parts[1] ]] * df[[ parts[2] ]]
    }
  }

  # now build one partial regression plot per term
  plots <- lapply(seq_along(safe_labels), function(i) {
    term_safe <- safe_labels[i]
    term_lbl  <- term_labels[i]

    # everything except this term
    others    <- setdiff(safe_labels, term_safe)

    # residuals of Y ~ others
    fY <- stats::reformulate(others, resp_name)
    yres <- stats::resid(stats::lm(fY, data = df))

    # residuals of X_term ~ others
    fX <- stats::reformulate(others, term_safe)
    xres <- stats::resid(stats::lm(fX, data = df))

    # fit and extract slope & SD
    fit   <- stats::lm(yres ~ xres)
    beta  <- stats::coef(fit)[2]
    sd_r  <- stats::sd(stats::resid(fit))

    ggplot2::ggplot(data.frame(xres,yres), ggplot2::aes(x=xres,y=yres)) +
      ggplot2::geom_point(color="steelblue", alpha=0.7) +
      ggplot2::geom_smooth(method="lm", se=FALSE,
                  color="darkgrey", linewidth=1) +
      ggplot2::annotate("text", x=Inf,y=Inf, hjust=1.1,vjust=1.5,
               label=sprintf("Beta = %.3f\nSD(resid)=%.3f", beta, sd_r),
               size=4) +
      ggplot2::labs(
        title = paste("Partial Regression for", term_lbl),
        x     = paste(term_lbl, "residuals"),
        y     = paste(resp_name,   "residuals")
      ) +
      ggplot2::theme_minimal(base_size=14) +
      ggplot2::theme(
        axis.text.x     = ggplot2::element_text(angle=45, hjust=1),
        plot.title      = ggplot2::element_text(face="bold"),
        legend.position = "none"
      )
  })

  names(plots) <- term_labels
  plots
}

# method of moments estiamte of random intercept var
var_tau <- function(x, y) {
  Y      <- x[[y]]
  id     <- factor(x$ID)
  n      <- length(unique(x$time))
  aovTab <- summary(stats::aov(Y ~ id, data = x))[[1]]
  MSB    <- aovTab["id",       "Mean Sq"]
  MSW    <- aovTab["Residuals", "Mean Sq"]
  tau2_mom <- (MSB - MSW) / n
  tau2_mom
}

# re order the target cor according to input order of columns
remap_target_cor <- function(target_cor, sim_data, vars_new) {
  # original names & dimension
  vars_old <- colnames(sim_data)
  p_old    <- length(vars_old)
  if (length(target_cor) != p_old*(p_old-1)/2) {
    stop("target_cor length (", length(target_cor),
         ") does not match ncol(sim_data) = ", p_old)
  }

  # build full symmetric matrix
  mat_old <- matrix(NA_real_, p_old, p_old,
                    dimnames = list(vars_old, vars_old))
  mat_old[upper.tri(mat_old)] <- target_cor
  mat_old[lower.tri(mat_old)] <- t(mat_old)[lower.tri(mat_old)]
  diag(mat_old) <- 1

  # reorder rows & columns
  if (!all(vars_new %in% vars_old)) {
    stop("Some vars_new not found in sim_data: ",
         paste(setdiff(vars_new, vars_old), collapse = ", "))
  }
  mat_new <- mat_old[vars_new, vars_new]

  # return new upper triangle
  as.vector(mat_new[upper.tri(mat_new)])
}
