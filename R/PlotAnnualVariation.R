
#' Plot annual variation in survey counts
#'
#' Visualize variation in annual survey counts across islands using
#' boxplots or violin plots with individual observations overlaid.
#' The function supports incubation, creche, and ground survey data.
#'
#' Incubation and creche data are filtered to annual maximum counts
#' (`stat == "max"`), whereas ground survey data are filtered by
#' species and variable without restricting the summary statistic.
#'
#' @param df A data frame containing annual survey summaries. Expected
#'   columns include `Species_Code`, `variable`, `Island`, `year`,
#'   `value`, and `shape`. Incubation and creche data must also include
#'   `stat`.
#' @param plot_type Character string specifying the plot type:
#'   `"boxplot"` or `"violin"`. Defaults to `"boxplot"`.
#' @param survey Character string specifying the survey type:
#'   `"incubation"`, `"creche"`, or `"ground"`.
#' @param species Character string specifying the species code to plot.
#' @param var Character string specifying the survey variable to plot.
#' @param current_yr Optional reporting year used in the plot title.
#'   Defaults to the maximum year in `df`.
#' @param show_plot Logical. If `TRUE`, print the plot when the function is
#'   called. The ggplot object is returned invisibly either way.
#'
#' @return A ggplot object displaying annual variation in survey counts.
#'
#' @details
#' For incubation and creche surveys, the function uses annual maximum
#' counts. For ground surveys, the supplied summary statistic is retained.
#' Individual observations are overlaid on the boxplot or violin plot.
#'
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_violin geom_jitter coord_flip scale_shape_manual scale_color_viridis_c theme_bw guides labs ggtitle theme element_text element_rect element_line unit
#' @export
#' 
PlotAnnualVariation <- function(
    df,
    plot_type = c("boxplot", "violin"),
    survey,
    species,
    var,
    current_yr = NULL,
    show_plot = TRUE
) {
  
  # ---- Check arguments --------------------------------------------------
  
  plot_type <- match.arg(plot_type)
  
  survey <- match.arg(
    survey,
    choices = c("incubation", "creche", "ground")
  )
  
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame.", call. = FALSE)
  }
  
  if (missing(species) || length(species) != 1L || is.na(species)) {
    stop("'species' must be a single species code.", call. = FALSE)
  }
  
  if (missing(var) || length(var) != 1L || is.na(var)) {
    stop("'var' must be a single variable name.", call. = FALSE)
  }
  
  if (!is.logical(show_plot) || length(show_plot) != 1L || is.na(show_plot)) {
    stop("'show_plot' must be TRUE or FALSE.", call. = FALSE)
  }
  
  required_cols <- c(
    "Species_Code", "variable", "Island",
    "year", "value", "shape"
  )
  
  if (survey %in% c("incubation", "creche")) {
    required_cols <- c(required_cols, "stat")
  }
  
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in 'df': ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  
  # ---- Filter df ------------------------------------------------------
  
  df <- df |>
    dplyr::filter(
      Species_Code == species,
      variable == var
    )
  
  if (survey %in% c("incubation", "creche")) {
    df <- df |>
      dplyr::filter(stat == "max")
  }
  
  if (nrow(df) == 0) {
    stop(
      "No observations found for the specified survey, species, and variable.",
      call. = FALSE
    )
  }
  
  # Ensure year and value are suitable for plotting.
  df <- df |>
    dplyr::mutate(
      year = as.numeric(as.character(year)),
      value = as.numeric(value),
      Island = as.factor(Island),
      shape = as.factor(shape)
    ) |>
    dplyr::filter(
      !is.na(year),
      !is.na(value),
      !is.na(Island)
    )
  
  if (nrow(df) == 0) {
    stop("No non-missing observations are available to plot.", call. = FALSE)
  }
  
  # ---- Determine reporting year ----------------------------------------
  
  if (is.null(current_yr)) {
    current_yr <- max(df$year, na.rm = TRUE)
  }
  
  # ---- Create plot ------------------------------------------------------
  
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = Island,
      y = value,
      color = year,
      shape = shape
    )
  )
  
  if (plot_type == "boxplot") {
    
    p <- p +
      ggplot2::geom_boxplot(
        aes(group = Island),
        alpha = 0.3,
        notch = FALSE,
        outlier.shape = NA
      )
    
  } else if (plot_type == "violin") {
    
    p <- p +
      ggplot2::geom_violin(
        aes(group = Island),
        alpha = 0.3
      )
  }
  
  p <- p +
    ggplot2::geom_jitter(
      size = 3,
      width = 0.15,
      height = 0
    ) +
    
    ggplot2::coord_flip() +
    
    ggplot2::scale_shape_manual(
      values = c(19, 8)
    ) +
    
    ggplot2::scale_color_viridis_c(
      option = "D",
      name = "Year",
      direction = -1
    ) +
    
    ggplot2::labs(
      title = paste0(
        "Variation in annual counts of ",
        species, " ", var,
        " from 2007 to ", current_yr, "."
      ),
      y = "Count",
      x = NULL,
      color = "Year"
    ) +
    
    ggplot2::guides(
      shape = "none"
    ) +
    
    ggplot2::theme_bw() +
    facet_wrap(~Island, scales = "free_x")+
    
    ggplot2::theme(
      legend.position = "right",
      legend.key.height = grid::unit(2, "cm"),
      legend.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 12),
      axis.title.x = ggplot2::element_text(
        size = 16,
        face = "bold"
      ),
    
      axis.title.y = ggplot2::element_text(
        size = 16,
        face = "bold"
      ),
      strip.text = ggplot2::element_text(
        size = 12,
        face = "bold.italic"
      ),
      panel.background = ggplot2::element_rect(
        fill = "white",
        colour = "black"
      ),
      panel.grid.major = ggplot2::element_line(
        colour = "grey90"
      ),
      plot.title = ggplot2::element_text(
        size = 12,
        face = "bold"
      )
    )
  
  # ---- Print and return -------------------------------------------------
  
  if (show_plot) {
    suppressWarnings(print(p))
  }
  
  invisible(p)
}
