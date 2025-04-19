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
#' @noRd
NULL

count_decimals <- function(vec) {
  sapply(as.character(vec), function(x) {
    # Check if there's a decimal point in the string.
    if (grepl("\\.", x)) {
      # Split the string at the decimal point and return the number of digits after it.
      parts <- strsplit(x, "\\.", fixed = FALSE)[[1]]
      nchar(parts[2])
    } else {
      0
    }
  })
}


check_grim <- function(n, target_mean, decimals, tol.r = .Machine$double.eps^0.5) {
  # Compute the nearest possible sum (the total number of "points")
  total_points <- round(target_mean * n)

  # Calculate the mean that would result from that total
  possible_mean <- total_points / n

  # Determine the absolute difference between the target mean and the possible mean
  diff <- abs(target_mean - possible_mean)

  # Define the allowed margin (half the smallest increment plus a tolerance for rounding errors)
  allowed_margin <- (0.1 ^ decimals) / 2 + tol.r

  if (diff > allowed_margin) {
    adjusted_mean <- round(possible_mean, decimals)
    cat("\nMean", target_mean, "fails GRIM test. The adjusted mean", adjusted_mean, "is plausible.\n")
    return(list(test = FALSE, grim_mean = adjusted_mean))
  } else {
    cat("\nMean", target_mean, "passed GRIM test. The mean is plausible.\n")
    return(list(test = TRUE, grim_mean = target_mean))
  }
}


get_design <- function(sim_data, reg_equation, terms_obj) {
  # split in candidate and outcome
  candidate <- as.matrix(sim_data[, -ncol(sim_data)])
  outcome <- sim_data[, ncol(sim_data)]
  p <- ncol(candidate)

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
  positions <- match(target_names, full_names)

  return(list(
    target_names = target_names,
    full_names = full_names,
    positions = positions
  ))
}

is_integer_vector <- function(vec, tol = .Machine$double.eps) {
  all(abs(vec %% 1) < tol)
}

long_to_wide <- function(data) {
  if (!is.data.frame(data)) stop("Input must be a data.frame.")
  if (ncol(data) < 3) stop("Need: pnumber + time + at least 1 measure.")

  participant_col <- names(data)[1]
  time_col <- "time"
  if (!time_col %in% names(data)) stop("'time' column not found")

  time_index <- which(names(data) == time_col)
  # If time column comes immediately after pnumber, no between-subject columns exist
  if (time_index > 2) {
    between_cols <- names(data)[2:(time_index - 1)]
  } else {
    between_cols <- character(0)
  }

  value_cols <- setdiff(names(data), c(participant_col, between_cols, time_col))

  wide_data <- tidyr::pivot_wider(
    data,
    id_cols     = c(participant_col, between_cols),
    names_from  = time_col,
    values_from = all_of(value_cols),
    names_sep   = "_"
  )

  as.data.frame(wide_data)
}


wide_to_long <- function(data) {
  # Convert matrix to data.frame if needed
  if (is.matrix(data)) data <- as.data.frame(data)

  # Ensure first column is named "pnumber"
  if (!"pnumber" %in% names(data)) {
    names(data)[1] <- "pnumber"
  }

  # Identify id (between - subject) cols = those WITHOUT an underscore
  id_cols <- names(data)[!grepl("_", names(data))]

  data %>%
    pivot_longer(
      cols       = -all_of(id_cols),
      names_to   = c(".value", "time"),
      names_sep  = "_"
    ) %>%
    # convert time from character integer
    mutate(time = as.integer(.data$time)) %>%
    as.data.frame()
}


heuristic_move <- function(candidate, target_sd, range) {
  lower_bound <- range[1]
  upper_bound <- range[2]

  # Precompute current SD and maximum value to avoid repeated computation.
  current_sd <- stats::sd(candidate)
  increaseSD <- (current_sd < target_sd)
  cand_max <- max(candidate)

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
  } else {
    # No between factors: create a dummy design with one group.
    between_design <- data.frame(dummy = rep(1, sample_size))
    n_between <- sample_size
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
  if(ncol(between_design) > 0) {
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
  if(ncol(between_design) > 0) {
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
  } else {
    final_df <- full_between
  }

  # Determine number of subjects.
  n_subjects <- if(ncol(between_design) > 0) nrow(full_between) else sample_size

  # Add subject identifier ("pnumber") as the first column.
  if(ncol(within_design) > 0) {
    final_df <- cbind(pnumber = rep(1:n_subjects, each = n_within), final_df)
  } else {
    final_df <- cbind(pnumber = 1:n_subjects, final_df)
  }

  # Remove any row names to avoid warnings.
  rownames(final_df) <- NULL

  # Rename factor columns (all except pnumber) to "Factor1", "Factor2", etc.
  n_factor_cols <- ncol(final_df) - 1
  if(n_factor_cols > 0) {
    colnames(final_df)[-1] <- paste0("Factor", seq_len(n_factor_cols))
  }

  # Convert all factor columns (except pnumber) to factors.
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
  # Calculate total points (should be integer if target is GRIM-consistent)
  total_points <- round(tMean * n)
  # Compute base integer value and remainder
  base <- total_points %/% n
  remainder <- total_points %% n
  # Create a vector: 'remainder' values will be (base+1) and the rest will be base
  vec <- c(rep(base + 1, remainder), rep(base, n - remainder))
  # Optionally, randomize the order within the group
  vec <- sample(vec)
  # Force values to be within the allowed range (if needed)
  vec <- pmax(pmin(vec, range[2]), range[1])
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

