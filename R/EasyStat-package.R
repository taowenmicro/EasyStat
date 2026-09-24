#' EasyStat: automated statistical decision and visualization workflow
#'
#' The package routes each variable to one-way ANOVA, Welch's ANOVA or the
#' Kruskal-Wallis test according to its own assumption tests, applies the
#' matching post-hoc procedure, corrects the omnibus p-values across variables,
#' and returns compact-letter annotations in a standardised result object that
#' the plotting functions consume directly.
#'
#' @section Namespace imports:
#' Several functions in this package call \pkg{dplyr}, \pkg{tibble} and
#' \pkg{stats} verbs without a package prefix. Declaring the imports here means
#' the package works whether or not the user has attached \pkg{dplyr}; before
#' this block existed, calls such as \code{group_by()} or \code{as_tibble()}
#' resolved only when the caller happened to have those packages on the search
#' path, and failed inside a clean namespace (for example under
#' \code{testthat}).
#'
#' @keywords internal
#'
#' @import ggplot2
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise arrange mutate select filter
#' @importFrom dplyr inner_join left_join desc bind_rows rename distinct
#' @importFrom tibble as_tibble tibble
#' @importFrom purrr map_chr map_dbl
#' @importFrom stringr str_split
#' @importFrom rlang sym .data
#' @importFrom stats aov oneway.test shapiro.test bartlett.test
#' @importFrom stats kruskal.test wilcox.test t.test
#' @importFrom stats p.adjust setNames var sd median quantile dist hclust
#' @importFrom utils combn head
#' @importFrom grDevices colorRampPalette
"_PACKAGE"

## Column names used in non-standard evaluation (ggplot2 aesthetics and dplyr
## verbs). Declaring them here stops R CMD check reporting each one as an
## undefined global variable; it does not change any behaviour.
utils::globalVariables(c(
  ".", ".data", "aa", "count", "data_box", "data_wt", "dd", "group", "groups",
  "id", "ID", "label_abc", "label_x", "label_y", "Mean", "pid", "Rank",
  "Relative_abundance", "Sample", "Sd", "SD", "stat", "value", "variable",
  "y", "ymax", "ymin"
))
