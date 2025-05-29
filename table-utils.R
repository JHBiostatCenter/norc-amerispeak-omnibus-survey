#' Format numbers for table display
#' @param x Numeric vector to format
#' @param digits Number of decimal places
#' @return Formatted character vector
format_table_numbers <- function(x, digits = 1) {
  ifelse(is.na(x), "", format(round(x, digits), nsmall = digits))
}

#' Create cross-tabulation with weighted percentages
#' @param data Data frame containing the variables
#' @param var1 First variable name (character)
#' @param var2 Second variable name (character, optional)
#' @return Tibble with weighted percentages and standard errors
tab <- function(data, var1, var2 = NULL) {
  # Create survey design object
  svy_design <- svydesign(
    ids = ~1,
    weights = ~WEIGHT_EN,
    data = data,
    na.action = na.omit
  )

  if (is.null(var2)) {
    # Single variable tabulation
    tab_result <- svytable(as.formula(paste("~", var1)), design = svy_design)
    total_weight <- sum(weights(svy_design))

    result <- as.data.frame(tab_result) |>
      rename(category = 1, n_wtd = Freq) |>
      mutate(
        pct_wtd = n_wtd / total_weight * 100,
        se = sqrt(pct_wtd * (100 - pct_wtd) / total_weight),
        pct_wtd = sprintf("%.1f (%.1f)", pct_wtd, se)
      ) |>
      select(category, pct_wtd)
  } else {
    # Cross-tabulation
    tab_result <- svytable(
      as.formula(paste("~", var1, "+", var2)),
      design = svy_design
    )

    result <- as.data.frame(tab_result) |>
      rename(
        category = 1,
        group = 2,
        n_wtd = Freq
      ) |>
      group_by(group) |>
      mutate(
        group_total = sum(n_wtd),
        pct_wtd = n_wtd / group_total * 100,
        se = sqrt(pct_wtd * (100 - pct_wtd) / group_total),
        pct_wtd = sprintf("%.1f (%.1f)", pct_wtd, se)
      ) |>
      ungroup() |>
      select(category, group, pct_wtd)
  }
  return(result)
}

#' Calculate category statistics for survey data
#' @param data Survey dataset
#' @param var Variable name
#' @param val Category value
#' @param design Survey design object
#' @return Tibble with category statistics
calculate_category_stats <- function(data, var, val, design) {
  tryCatch(
    {
      # Unweighted N
      unwt_n <- sum(data[[var]] == val, na.rm = TRUE)

      # Create 0/1 indicator for this value
      temp_data <- data |>
        filter(WEIGHT_EN > 1e-10) |>
        mutate(temp = ifelse(as.character(.data[[var]]) == as.character(val), 1, 0)) |>
        filter(!is.na(temp), !is.na(WEIGHT_EN))

      temp_design <- svydesign(ids = ~1, weights = ~WEIGHT_EN, data = temp_data)

      svy_result <- svytotal(~temp, temp_design)

      wt_n <- coef(svy_result)[1]
      se <- SE(svy_result)[1]

      tibble(
        value = as.character(val),
        unweighted_n = unwt_n,
        weighted_n = round(wt_n, 1),
        se = round(se, 1),
        ci_lb = round(wt_n - 1.96 * se, 1),
        ci_ub = round(wt_n + 1.96 * se, 1)
      ) |>
        mutate(across(
          c(unweighted_n, weighted_n, se, ci_lb, ci_ub),
          ~ format_table_numbers(.x)
        ))
    },
    error = function(e) {
      warning(glue("Error calculating stats for {var} = {val}: {e$message}"))
      tibble(
        value = as.character(val),
        unweighted_n = NA,
        weighted_n = NA,
        se = NA,
        ci_lb = NA,
        ci_ub = NA
      )
    }
  )
}

#' Generate demographic table with weighted statistics
#' @param data The survey dataset
#' @param vars Named vector of variables to include
#' @param design Survey design object
#' @return Formatted table with weighted statistics
generate_demographic_table <- function(data, vars, design) {
  map_dfr(names(vars), function(label) {
    var <- vars[[label]]
    values <- na.omit(levels(droplevels(factor(data[[var]]))))

    # Header row
    var_header <- tibble(
      value = label,
      unweighted_n = NA,
      weighted_n = NA,
      se = NA,
      ci_lb = NA,
      ci_ub = NA
    )

    # Category rows
    category_rows <- map_dfr(values, function(val) {
      calculate_category_stats(data, var, val, design)
    })

    bind_rows(var_header, category_rows)
  })
}

#' Format table for display
#' @param table_data Table data to format
#' @param caption Table caption
#' @return Formatted kable table
format_table_for_display <- function(table_data, caption) {
  table_data |>
    kable(
      format = "html",
      digits = 1,
      align = c("l", "r", "r", "r", "r", "r"),
      caption = caption
    ) |>
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE
    )
}
