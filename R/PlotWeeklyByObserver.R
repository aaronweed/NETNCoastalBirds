
#' Plot weekly survey counts by observer against historical variation
#'
#' Compare current-year weekly survey counts with historical seasonal
#' patterns. Historical summaries include the median, 50% range,
#' 95% range, and full observed range for each week.
#'
#' For creche surveys, counts are summarized by species, life stage,
#' date, and observer. For incubation surveys, the supplied survey
#' records are used at the observer and segment level.
#'
#' Historical summaries exclude the year or years specified by
#' `current_yr`. Historical quantile ranges are calculated only for
#' groups with at least three non-missing observations.
#'
#' @param df A data frame containing raw survey records. For creche
#'   surveys, this is typically `cbb$creche_raw`; for incubation surveys,
#'   `cbb$incubation_raw`.
#' @param survey Character string specifying the survey type:
#'   `"creche"` or `"incubation"`.
#' @param current_yr Numeric or character vector identifying the year or
#'   years to display as current observations. These years are excluded
#'   from the historical summaries.
#' @param species Optional character vector of species codes to include.
#'   If `NULL`, all species in `data` are retained.
#' @param var Optional character vector of creche life-stage units to
#'   include. Relevant values include `"F-Lone"`, `"Chick"`, and
#'   `"F-Tend"`. Ignored for incubation surveys.
#' @param print Logical. If `TRUE`, print the plot when the function is
#'   called. The ggplot object is returned invisibly either way.
#'
#' @return A ggplot object showing current-year weekly counts and
#'   historical seasonal variation.
#'
#' @details
#' The historical median and ranges are calculated across available
#' observer records. Creche counts are first summed by date, species,
#' life stage, and observer. Incubation records are retained at the
#' supplied segment and observer level.
#'
#' Only primary and complete surveys from the outer islands are included.
#' The island name `"Roaring Bulls"` is recoded to `"The Graves"`.
#'
#' @importFrom dplyr filter mutate group_by summarise across case_when select distinct
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom ggplot2 ggplot aes geom_ribbon geom_line geom_point scale_x_continuous scale_color_manual scale_fill_manual labs theme_bw theme element_text facet_wrap
#' @importFrom lubridate isoweek
#' @export
#' 
PlotWeeklyByObserver <- function(
    df,
    survey = c("creche", "incubation"),
    current_yr,
    species = NULL,
    var = NULL,
    print = TRUE
) {
  
  survey <- match.arg(survey)
  
  # ---- Check arguments --------------------------------------------------
  
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame.", call. = FALSE)
  }
  
  if (missing(current_yr) || is.null(current_yr)) {
    stop("'current_yr' must specify at least one year.", call. = FALSE)
  }
  
  if (!is.logical(print) || length(print) != 1L || is.na(print)) {
    stop("'print' must be TRUE or FALSE.", call. = FALSE)
  }
  
  outer <- c(
    "Calf", "Little Calf", "Green", "The Graves",
    "Middle Brewster", "Outer Brewster", "Shag Rocks",
    "Little Brewster"
  )
  
  # Required columns differ slightly by survey type.
  required_cols <- c(
    "Island", "Date", "year", "Species_Code", "Observer",
    "Survey_Primary", "Survey_Complete", "Unit_Count"
  )
  
  if (survey == "creche") {
    required_cols <- c(required_cols, "Species_Unit")
  }
  
  if (survey == "incubation") {
    required_cols <- c(required_cols, "Segment")
  }
  
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in 'df': ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # ---- Prepare survey data ---------------------------------------------
  
  df <- df |>
    dplyr::mutate(
      Island = dplyr::case_when(
        Island == "Roaring Bulls" ~ "The Graves",
        .default = as.character(Island)
      ),
      Date = as.Date(Date),
      year = as.integer(year),
      week = lubridate::isoweek(Date),
      Unit_Count = as.numeric(Unit_Count)
    ) |>
    dplyr::filter(
      Survey_Primary == "Yes",
      Survey_Complete == "Yes",
      Island %in% outer
    )
  
  # Apply species filter when supplied.
  if (!is.null(species)) {
    df <- df |>
      dplyr::filter(Species_Code %in% species)
  }
  
  # Creche-specific preparation.
  if (survey == "creche") {
    
    if (!is.null(var)) {
      df <- df |>
        dplyr::filter(Species_Unit %in% var)
    }
    
    df <- df |>
      dplyr::group_by(
        Species_Code, Date, year, month, week,
        Species_Unit, Observer
      ) |>
      dplyr::summarise(
        Unit_Count = sum(Unit_Count, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(Island = "Outer Islands")
    
    # Grouping and faceting identifiers.
    group_cols <- c("Island", "Species_Code", "Species_Unit")
    facet_formula <- ~Island
    
  } else {
    
    # Incubation data are retained at the supplied segment level.
    group_cols <- c("Island", "Segment", "Species_Code")
    facet_formula <- ~Island + Segment
  }
  
  if (nrow(df) == 0) {
    stop("No survey records remain after filtering.", call. = FALSE)
  }
  
  # ---- Historical summaries -------------------------------------------
  
  historical <- df |>
    dplyr::filter(!year %in% current_yr, !is.na(Unit_Count)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "week")))) |>
    dplyr::summarise(
      num_samps = dplyr::n(),
      median_val = median(Unit_Count),
      min_val = min(Unit_Count),
      max_val = max(Unit_Count),
      lower_95 = if (dplyr::n() >= 3) {
        unname(stats::quantile(Unit_Count, 0.025))
      } else {
        NA_real_
      },
      upper_95 = if (dplyr::n() >= 3) {
        unname(stats::quantile(Unit_Count, 0.975))
      } else {
        NA_real_
      },
      lower_50 = if (dplyr::n() >= 3) {
        unname(stats::quantile(Unit_Count, 0.25))
      } else {
        NA_real_
      },
      upper_50 = if (dplyr::n() >= 3) {
        unname(stats::quantile(Unit_Count, 0.75))
      } else {
        NA_real_
      },
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(lower_50))
  
  if (nrow(historical) == 0) {
    stop(
      "No historical groups have at least three observations.",
      call. = FALSE
    )
  }
  
  # Create an identifier to keep separate series from connecting.
  series_cols <- c(group_cols, "week")
  
  historical$.series <- do.call(
    interaction,
    c(historical[series_cols], list(drop = TRUE))
  )
  
  # ---- Current-year observations ---------------------------------------
  
  df_current <- df |>
    dplyr::filter(year %in% current_yr, !is.na(Unit_Count)) |>
    dplyr::mutate(metric_type = "value")
  
  if (nrow(df_current) == 0) {
    stop("No current-year observations were found.", call. = FALSE)
  }
  
  df_current$.series <- do.call(
    interaction,
    c(df_current[series_cols], list(drop = TRUE))
  )
  
  # ---- Prepare historical median and ranges for plotting ---------------
  
  df_med <- historical |>
    dplyr::mutate(metric_type = "median")
  
  df_sum_plot <- historical |>
    dplyr::select(
      dplyr::all_of(group_cols),
      week,
      .series,
      lower_100 = min_val,
      upper_100 = max_val,
      lower_95,
      upper_95,
      lower_50,
      upper_50
    ) |>
    tidyr::pivot_longer(
      cols = c(
        lower_100, upper_100,
        lower_95, upper_95,
        lower_50, upper_50
      ),
      names_to = "metric",
      values_to = "value"
    ) |>
    dplyr::mutate(
      metric_type = ifelse(
        grepl("^lower", metric), "lower", "upper"
      ),
      distrib = paste0("d", gsub("\\D", "", metric))
    ) |>
    dplyr::select(-metric) |>
    tidyr::pivot_wider(
      names_from = metric_type,
      values_from = value
    ) |>
    dplyr::mutate(metric_type = distrib)
  
  # ---- Plot settings ----------------------------------------------------
  
  plot_values <- c(
    "d100" = "#B8D8ED",
    "d95" = "#7FB9DD",
    "d50" = "#1378b5",
    "median" = "blue",
    "value" = "black"
  )
  
  plot_breaks <- c("d100", "d95", "d50", "median", "value")
  
  plot_labels <- c(
    "d100" = "Historic range",
    "d95" = "Hist. 95% range",
    "d50" = "Hist. 50% range",
    "median" = "Hist. median",
    "value" = "Current year counts"
  )
  
  # Construct a descriptive y-axis label.
  species_label <- paste(
    unique(df$Species_Code),
    collapse = ", "
  )
  
  y_label <- if (survey == "creche") {
    unit_label <- paste(unique(df$Species_Unit), collapse = ", ")
    paste("No. of", species_label, unit_label)
  } else {
    paste("No. of", species_label)
  }
  
  # ---- Create plot ------------------------------------------------------
  
  p <- ggplot2::ggplot() +
    
    ggplot2::geom_ribbon(
      data = df_sum_plot,
      ggplot2::aes(
        x = week, 
        ymin = lower, ymax = upper, fill = metric_type, group= metric_type
      ),
      alpha = 0.2
    ) +
    
    ggplot2::geom_line(
      data = df_med,
      ggplot2::aes(
        y = median_val, x = week, 
        color = metric_type, group = metric_type,
        text = paste0(
          "Island: ", Island, "<br>",
          "Week: ", week, "<br>",
          "Historical Median: ", round(median_val, 1), "<br>"
        )
      ),
      linewidth = 0.7
    ) +
    
    ggplot2::geom_point(
      data = df_current,
      ggplot2::aes(
        y = Unit_Count, 
        x = week,
        color = metric_type, group = metric_type, shape= Observer,
      )
    ) +
    
    ggplot2::scale_x_continuous(
      breaks = seq(1, 52, 1),
      name = "Week of Year"
    ) +
    
    ggplot2::scale_color_manual(
      values = plot_values,
      breaks = plot_breaks,
      labels = plot_labels,
      name = NULL
    ) +
    
    ggplot2::scale_fill_manual(
      values = plot_values,
      breaks = plot_breaks,
      labels = plot_labels,
      name = NULL
    ) +
    
    ggplot2::labs(
      title = paste(
        "Historical Seasonal Pattern (weekly) vs",
        paste(current_yr, collapse = ", "),
        "Observations across Outer Islands"
      ),
      y = y_label,
      fill = "Metric Type",
      color = "Metric Type"
    ) +
    
    ggplot2::theme_bw() +
    
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        color = "black", size = 12
      ),
      axis.text.x = ggplot2::element_text(
        angle = 0, size = 10
      ),
      axis.title.x = ggplot2::element_text(
        size = 16, face = "bold"
      ),
      axis.title.y = ggplot2::element_text(
        size = 16, face = "bold"
      ),
      plot.title = ggplot2::element_text(
        size = 12, face = "bold"
      )
    ) +
    
    ggplot2::facet_wrap(
      facet_formula,
      scales = "free_y",
      ncol = 3
    )
  
  # Print if requested, but always return the plot.
  if (print) {
    print(p)
  }
  
  invisible(p)
}
