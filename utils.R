library(ggplot2)
library(dplyr)
library(forcats)
library(scales)
library(stringr)

filter_all_caps <- function(data, column) {
  data |>
    filter(!grepl("^[A-Z ]+$", .data[[column]]))
}

extract_question_metadata <- function(row) {
  # Extract the text from the row
  text <- str_trim(row$question_raw_text)

  # Use a regular expression to match all bracketed text
  matches <- stringr::str_extract_all(text, "\\[[^\\]]+\\]")[[1]]

  # Remove brackets from matches for topics and routing logic classification
  cleaned_matches <- stringr::str_remove_all(matches, "\\[|\\]")

  # Classify topics as Sentence case
  topics <- str_trim(cleaned_matches[grepl("^[A-Z][a-z]", cleaned_matches)])

  # Anything remaining is classified as routing logic
  routing_logic <- setdiff(cleaned_matches, topics)

  # Remove any leading bracketed text from the question text
  while (grepl("^\\[.*?\\](\\s*\\[.*?\\])*", text)) {
    text <- str_trim(stringr::str_remove(text, "^\\[.*?\\](\\s*\\[.*?\\])*"))
  }

  # Handle cases where there might be multiple topics or routing logic
  topic <- if (length(topics) > 0) paste(topics, collapse = " | ") else NA
  routing <- if (length(routing_logic) > 0) paste(routing_logic, collapse = " | ") else NA

  # There is a question about public health that doesn't follow the above logic; handle it separately
  if (grepl("public health", text, ignore.case = TRUE)) {
    topic <- "Public Health"
  }

  # If a question ends in a colon, the topic is appended to the question. Otherwise, the question is standalone
  if (grepl(":$", text)) {
    question <- paste0(text, " [", topic, "]")
  }
  # If they are asked "Which issues..."
  else if (grepl("^Which issues", text)) {
    question <- paste0(text, " [Respondent chose ", topic, "]")
  }
  # If they are provided items (always starts with "Here..."), append
  else if (grepl("^Here", text)) {
    question <- paste0(text, " [", topic, "]")
  }
  # Otherwies, question == text
  else {
    question <- text
  }


  # Return a named list
  list(
    question = question,
    topic = topic,
    routing_logic = routing
  )
}

tabulate_var <- function(var, plot_title, plot_subtitle = NULL, filter_var = NULL, filter_value = NULL, filter_reason = NULL) {
  # Capture the variable name as a symbol
  if (is.character(var)) {
    var <- sym(var) # Convert string to symbol
  } else {
    var <- enquo(var) # Capture the variable name as a symbol
  }

  # Filter if filter_var and filter_value are provided
  if (!is.null(filter_var) && !is.null(filter_value)) {
    filter_var <- sym(filter_var) # Capture the filter variable as a symbol
    data <- data |> filter(!!filter_var == filter_value) # Apply the filter
  } else {
    data <- data # No filtering applied
  }

  # Raw counts
  raw_counts <- data |>
    group_by(!!var) |> # Unquote `var` to evaluate it as a column name
    summarise(n = n(), .groups = "drop")

  # Add total N for raw counts
  total_n_raw <- sum(raw_counts$n)

  # Weighted counts with precise and rounded values
  weighted_counts <- data |>
    group_by(!!var) |> # Unquote `var` to evaluate it as a column name
    summarise(
      n_wtd_precise = round(sum(WEIGHT_EN, na.rm = TRUE), 3),
      n_wtd_rounded = round(n_wtd_precise),
      pct_wtd = round(n_wtd_precise / sum(data$WEIGHT_EN, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    )

  # Add total N for weighted counts
  total_n_weighted <- round(sum(weighted_counts$n_wtd_precise))

  # Return the results as a list with total N and optional filter reason
  tabulation_list <- list(
    raw_counts = raw_counts,
    weighted_counts = weighted_counts,
    total_n_raw = total_n_raw,
    total_n_weighted = total_n_weighted,
    filter_reason = filter_reason
  )

  bar_chart <- create_bar_plot_univariate_tabulation(tabulation_list, plot_title, plot_subtitle)

  tabulation_list$bar_chart <- bar_chart

  return(tabulation_list)
}



create_bar_plot_univariate_tabulation <- function(tabulations, plot_title, plot_subtitle = NULL) {
  # Extract relevant data and filter out NA
  var <- names(tabulations$weighted_counts)[1]
  data <- tabulations$weighted_counts |>
    filter(!is.na(!!sym(var)) & !(!!sym(var) %in% c("NA", "SKIPPED ON WEB", "REFUSED", "DON'T KNOW")))

  # Extract title and caption
  title <- str_wrap(plot_title, width = 50)
  subtitle <- if (!is.null(plot_subtitle)) str_wrap(plot_subtitle, width = 70) else NULL
  caption <- paste0("N=", tabulations$total_n_weighted, " (weighted)")

  # Create the ggplot object
  plot <- ggplot(data, aes(x = fct_rev(!!sym(var)), y = pct_wtd)) +
    geom_bar(stat = "identity", fill = "#002D72") + # Hopkins Blue color
    geom_text(aes(label = paste0(round(pct_wtd), "%")), hjust = -0.2, size = 4) +
    coord_flip(clip = "off") +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    theme_jhu_bar() +
    theme(
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 10, margin = margin(b = 10), lineheight = 1.2),
      plot.margin = margin(10, 40, 10, 20)
    ) +
    scale_y_continuous(
      expand = c(0, 0.05),
      breaks = seq(0, 100, 10),
      labels = function(x) paste0(x, "%")
    )

  return(plot)
}

aggregate_var_tabulations <- function(var, display_name, filter_var = NULL, filter_value = NULL, filter_reason = NULL, tabulations_list = list()) {
  # Ensure `var` is captured as a symbol
  var <- enquo(var)

  # Run tabulations for the variable with optional filtering
  new_tabulations <- tabulate_var(var, display_name, NULL, filter_var, filter_value, filter_reason)

  # Store the metadata (both tabulations and display name)
  tabulations_list[[quo_name(var)]] <- list(
    tabulations = new_tabulations,
    bar_chart = new_tabulations$bar_chart,
    display_name = display_name
  )

  # Return the updated list with the new tabulation appended
  return(tabulations_list)
}

display_var_tabulations <- function(tabulations) {
  # Loop through each tabulation in the list
  for (var_name in names(tabulations)) {
    # Extract components from the tabulation
    display_name <- tabulations[[var_name]]$display_name
    raw_counts <- tabulations[[var_name]]$tabulations$raw_counts
    weighted_counts <- tabulations[[var_name]]$tabulations$weighted_counts
    total_n_raw <- tabulations[[var_name]]$tabulations$total_n_raw
    total_n_weighted <- tabulations[[var_name]]$tabulations$total_n_weighted
    filter_reason <- tabulations[[var_name]]$tabulations$filter_reason
    bar_chart <- tabulations[[var_name]]$bar_chart

    # Separator
    cat("***\n")

    # Print the display name header
    cat(paste("###", display_name, "\n\n"))

    # Print the filter reason if applicable
    if (!is.null(filter_reason)) {
      cat(paste("**Filtered on**:", filter_reason, "\n\n"))
    }

    # Print Raw Counts header and table
    cat("#### Raw Counts\n\n")
    print(kable(raw_counts, format = "markdown"))

    # Print Weighted Counts header and table
    cat("\n#### Weighted Counts\n\n")
    print(kable(weighted_counts, format = "markdown"))

    # Display the bar chart if it exists
    if (!is.null(bar_chart)) {
      print(bar_chart)
      cat("\n\n")
    }

    for (cross_var in names(tabulations[[var_name]]$cross_tabulations)) {
      cross_tab_results <- tabulations[[var_name]]$cross_tabulations[[cross_var]]

      cat(paste0("### By ", cross_tab_results$display_name, "\n\n"))

      cross_tab <- create_cross_tab_table(cross_tab_results)
      print(cross_tab)


      # Print the plot
      cross_tab_plot <- create_cross_tab_plot(
        cross_tab_results,
        plot_title = topic,
        plot_subtitle = raw_question
      )


      facet_rows <- ceiling(length(unique(cross_tab_results$weighted_cross_tab$group)) / 2)
      plot_height <- max(6, facet_rows * 3) # Minimum height of 6 inches, adjust by 3 inches per row

      dir.create(paste0("images/", var_name), recursive = TRUE, showWarnings = FALSE)

      # Save the plot
      ggsave(
        filename = paste0("images/", var_name, "/", cross_var, ".png"),
        plot = cross_tab_plot,
        width = 10,
        height = plot_height,
        units = "in",
        dpi = 150 # Reduce DPI to speed up rendering
      )

      # Output the markdown image link using cat()
      cat(
        "![Cross Tab Plot](images/", var_name, "/", cross_var, ".png)\n\n",
        sep = ""
      )


      cat("\n\n")
    }
  }
}

cross_tabulate_var <- function(var, cross_var, filter_var = NULL, filter_value = NULL) {
  # Run the tabulation for the given variable
  tabulations <- tabulate_var(var, filter_var, filter_value)

  # Cross-tabulate raw counts
  raw_cross_tab <- data |>
    group_by(!!sym(cross_var), !!sym(var)) |>
    summarise(n = n(), .groups = "drop")

  # Calculate total N for the cross-tabulation group
  total_n_raw <- raw_cross_tab |>
    group_by(!!sym(cross_var)) |>
    summarise(total_n = sum(n)) |>
    pull(total_n)

  # Cross-tabulate weighted counts
  weighted_cross_tab <- data |>
    group_by(!!sym(cross_var), !!sym(var)) |>
    summarise(
      n_wtd_precise = sum(WEIGHT_EN, na.rm = TRUE),
      n_wtd_rounded = round(n_wtd_precise),
      .groups = "drop"
    ) |>
    rename(
      group = !!sym(cross_var),
      response = !!sym(var)
    )

  # Calculate total weighted count for each cross-tab category (e.g., each region)
  total_n_weighted <- weighted_cross_tab |>
    group_by(group) |>
    summarise(total_wtd = sum(n_wtd_precise)) |>
    pull(total_wtd)

  # Add percentages within each cross-tab group (so they sum to 100)
  weighted_cross_tab <- weighted_cross_tab |>
    mutate(
      pct_wtd = round((n_wtd_precise / total_n_weighted[match(group, unique(weighted_cross_tab$group))]) * 100, 1)
    )

  # Return the cross-tab results
  list(
    raw_cross_tab = raw_cross_tab,
    weighted_cross_tab = weighted_cross_tab,
    total_n_raw = total_n_raw,
    total_n_weighted = total_n_weighted
  )
}

create_cross_tab_table <- function(cross_tab_results) {
  # Define responses to exclude
  exclusion_list <- c("NA", "REFUSED", "SKIPPED ON WEB", "DON'T KNOW")

  # Filter out unwanted rows and responses
  data <- cross_tab_results$weighted_cross_tab |>
    filter(!is.na(group) & !group %in% exclusion_list, !response %in% exclusion_list)

  # Pivot the data to have response values as columns
  wide_data <- data |>
    select(group, response, pct_wtd) |>
    pivot_wider(
      names_from = response,
      values_from = pct_wtd,
      values_fill = 0
    )

  # Remove unwanted columns after pivoting
  wide_data <- wide_data |>
    select(-any_of(exclusion_list))

  # Use kable for pretty R Markdown output
  wide_data |>
    mutate(across(-group, ~ paste0(round(.x, 1), "%"))) |> # Format as percentages
    kable(
      format = "markdown",
      caption = cross_tab_results$display_name
    )
}
create_cross_tab_plot <- function(cross_tab_results, plot_title, plot_subtitle = NULL) {
  # Define responses to exclude
  exclusion_list <- c("NA", "REFUSED", "SKIPPED ON WEB", "DON'T KNOW")

  # Extract and filter data
  data <- cross_tab_results$weighted_cross_tab |>
    filter(!is.na(group) & !group %in% exclusion_list, !response %in% exclusion_list) |>
    mutate(n_wtd_rounded = round(n_wtd_rounded)) # Ensure n is rounded

  # Extract title and subtitle
  title <- str_wrap(plot_title, width = 50)
  subtitle <- if (!is.null(plot_subtitle)) str_wrap(plot_subtitle, width = 70) else NULL

  # Find the maximum percentage across all facets for consistent scaling
  max_pct <- max(data$pct_wtd, na.rm = TRUE)

  # Create a custom labeller for facet titles, wrapping text
  custom_labeller <- function(labels) {
    labels$group <- str_wrap(labels$group, width = 25) # Wrap facet titles
    return(labels)
  }

  # Create the faceted ggplot object
  plot <- ggplot(data, aes(x = fct_rev(response), y = pct_wtd, fill = response)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(pct_wtd), "%")), hjust = -0.2, size = 3) +
    facet_wrap(~group, scales = "free_y", ncol = 2, labeller = custom_labeller) + # Use custom labeller
    coord_flip() +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = "Percentage"
    ) +
    scale_fill_manual(values = c(colors$HopkinsBlue, colors$Blue, colors$BlueLight, colors$PaleBlue, colors$Gray1)) +
    theme_jhu_bar() +
    theme(
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 10, margin = margin(b = 10), lineheight = 1.2),
      strip.text = element_text(size = 10, lineheight = 1.2), # Adjust facet title size
      plot.margin = margin(10, 40, 10, 20),
      legend.position = "none"
    ) +
    scale_y_continuous(
      limits = c(0, max_pct * 1.2), # Set y-axis limit slightly above the max percentage
      expand = c(0, 0.05),
      breaks = seq(0, 100, 20),
      labels = function(x) paste0(x, "%")
    )

  return(plot)
}
display_item_tabulations <- function(tabulations) {
  for (var_name in names(tabulations)) {
    # Extract components from the tabulation
    display_name <- tabulations[[var_name]]$display_name
    raw_counts <- tabulations[[var_name]]$univariate_tabulation$raw_counts
    weighted_counts <- tabulations[[var_name]]$univariate_tabulation$weighted_counts
    total_n_raw <- tabulations[[var_name]]$univariate_tabulation$total_n_raw
    total_n_weighted <- tabulations[[var_name]]$univariate_tabulation$total_n_weighted
    filter_reason <- tabulations[[var_name]]$univariate_tabulation$filter_reason
    bar_chart <- tabulations[[var_name]]$univariate_tabulation$bar_chart

    topic <- tabulations[[var_name]]$topic
    question <- tabulations[[var_name]]$question

    # Separator
    cat("***\n")

    # Print the topic and question
    cat(paste0("## ", topic, "\n"))

    # Need to replace brackets with html entities for markdown output
    raw_question <- question
    question <- gsub("\\[", "&#91;", question)
    question <- gsub("\\]", "&#93;", question)

    cat(paste0("*Question*:  ", question, " \n\n"))

    # Print Raw Counts header and table
    cat("#### Raw Counts\n\n")
    print(kable(raw_counts, format = "markdown"))

    # Print Weighted Counts header and table
    cat("\n#### Weighted Counts\n\n")
    print(kable(weighted_counts, format = "markdown"))

    # Display the bar chart if it exists
    if (!is.null(bar_chart)) {
      print(bar_chart)
      cat("\n\n")
    }

    for (cross_var in names(tabulations[[var_name]]$cross_tabulations)) {
      cross_tab_results <- tabulations[[var_name]]$cross_tabulations[[cross_var]]

      cat(paste0("### By ", cross_tab_results$display_name, "\n\n"))

      cross_tab <- create_cross_tab_table(cross_tab_results)
      print(cross_tab)


      # Print the plot
      cross_tab_plot <- create_cross_tab_plot(
        cross_tab_results,
        plot_title = topic,
        plot_subtitle = raw_question
      )


      facet_rows <- ceiling(length(unique(cross_tab_results$weighted_cross_tab$group)) / 2)
      plot_height <- max(6, facet_rows * 3) # Minimum height of 6 inches, adjust by 3 inches per row

      dir.create(paste0("images/", var_name), recursive = TRUE, showWarnings = FALSE)

      # Save the plot
      ggsave(
        filename = paste0("images/", var_name, "/", cross_var, ".png"),
        plot = cross_tab_plot,
        width = 10,
        height = plot_height,
        units = "in",
        dpi = 150 # Reduce DPI to speed up rendering
      )

      # Output the markdown image link using cat()
      cat(
        "![Cross Tab Plot](images/", var_name, "/", cross_var, ".png)\n\n",
        sep = ""
      )


      cat("\n\n")
    }
  }
}

recode_responses <- function(response, response_set) {
  response <- as.character(response) # Ensure the response is a character
  case_when(
    response_set == "importance" &
      # response %in% c("Extremely important", "Very important") ~ "Very Important",
      response %in% c("Extremely important", "Very important") ~ "Very/Extremely important",
    response_set == "importance" &
      # response %in% c("Somewhat important", "A little important") ~ "Somewhat important",
      response %in% c("Somewhat important", "A little important") ~ "A little/Somewhat important",
    response_set == "importance" &
      response == "Not important at all" ~ "Not important",
    response_set == "support" &
      # response %in% c("Strongly support", "Support") ~ "Support",
      response %in% c("Strongly support", "Support") ~ "Support/Strongly support",
    response_set == "support" &
      response == "Neither support nor oppose" ~ "Neither support nor oppose",
    response_set == "support" &
      # response %in% c("Oppose", "Strongly oppose") ~ "Oppose",
      response %in% c("Oppose", "Strongly oppose") ~ "Oppose/Strongly oppose",
    response_set == "party_id" &
      response %in% c("Democrat", "Lean Democrat") ~ "Democrat",
    response_set == "party_id" &
      response %in% c("Republican", "Lean Republican") ~ "Republican",
    TRUE ~ response
  )
}
# recode_responses <- function(response, response_set) {
#   response <- as.character(response) # Ensure response is a character
#
#   recoded <- case_when(
#     response_set == "importance" & response %in% c("Extremely important", "Very important") ~ "Very/Extremely important",
#     response_set == "importance" & response %in% c("Somewhat important", "A little important") ~ "A little/Somewhat important",
#     response_set == "importance" & response == "Not important at all" ~ "Not important",
#
#     response_set == "support" & response %in% c("Strongly support", "Support") ~ "Support/Strongly support",
#     response_set == "support" & response == "Neither support nor oppose" ~ "Neither support nor oppose",
#     response_set == "support" & response %in% c("Oppose", "Strongly oppose") ~ "Oppose/Strongly oppose",
#
#     response_set == "party_id" & response %in% c("Democrat", "Lean Democrat") ~ "Democrat",
#     response_set == "party_id" & response %in% c("Republican", "Lean Republican") ~ "Republican",
#
#     TRUE ~ response  # Keep original response if not recoded
#   )

# # Ensure valid levels are assigned without removing missing ones
# levels_list <- list(
#   importance = c("Not important", "A little/Somewhat important", "Very/Extremely important"),
#   support = c("Oppose/Strongly oppose", "Neither support nor oppose", "Support/Strongly support"),
#   party_id = c("Democrat", "Republican", "Switch")  # Ensure "Switch" is included if applicable
# )
#
# # Assign explicit factor ordering **only if response_set is recognized**
# if (response_set %in% names(levels_list)) {
#   recoded <- factor(recoded, levels = levels_list[[response_set]], ordered = TRUE)
# } else {
#   recoded <- factor(recoded, levels = unique(recoded), ordered = TRUE)  # Preserve all values
# }
#
#   return(as.character(recoded))  # Ensure it remains a character vector
# }


tab <- function(data, var, by = NULL, weight_var = "WEIGHT_EN") {
  # Convert `var` and `by` to symbols
  var <- sym(var)
  weight_var <- sym(weight_var)

  # Calculate total weight for the entire dataset
  total_weight <- sum(data[[as_string(weight_var)]], na.rm = TRUE)

  if (!is.null(by)) {
    # Convert `by` to a symbol if provided
    by <- sym(by)

    # Perform cross-tabulation
    cross_tab <- data |>
      group_by(!!by, !!var) |>
      summarise(
        n_wtd_precise = sum(!!weight_var, na.rm = TRUE),
        n_wtd_rounded = round(n_wtd_precise),
        .groups = "drop"
      ) |>
      group_by(!!by) |>
      mutate(
        pct_wtd = round(n_wtd_precise / sum(n_wtd_precise, na.rm = TRUE) * 100, 1),
        # Format numbers for readability
        n_wtd_precise = comma(n_wtd_precise),
        n_wtd_rounded = comma(n_wtd_rounded)
      ) |>
      ungroup()

    return(cross_tab)
  } else {
    # Perform single-variable tabulation
    single_tab <- data |>
      group_by(!!var) |>
      summarise(
        n_wtd_precise = sum(!!weight_var, na.rm = TRUE),
        n_wtd_rounded = round(n_wtd_precise),
        pct_wtd = round(n_wtd_precise / total_weight * 100, 1), # Percent vs total dataset
        .groups = "drop"
      ) |>
      # Format numbers for readability
      mutate(
        n_wtd_precise = comma(n_wtd_precise),
        n_wtd_rounded = comma(n_wtd_rounded)
      )

    return(single_tab)
  }
}

strip_nonresponse_of_any_form <- function(x) {
  x[!x %in% c("SKIPPED ON WEB", "REFUSED")]
}


clean_svytotal <- function(svy_total_output, variable_name) {
  names(svy_total_output) <- gsub(paste0("^", variable_name), "", names(svy_total_output))
  svy_total_output
}
