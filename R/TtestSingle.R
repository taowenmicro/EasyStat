#' Two-sample t-test for single variable (independent samples)
#'
#' Designed for two-group comparisons only. Implements three sub-paths:
#' (1) Normal + homogeneous variance  -> Student's t-test (var.equal = TRUE)
#' (2) Normal + heterogeneous variance -> Welch's t-test (var.equal = FALSE)
#' (3) Non-normal                      -> Mann-Whitney U (wilcox.test)
#'
#' Output format is identical to KwWlx2 and aovMcomper2, ensuring compatibility
#' with all downstream visualization functions.
#'
#' @param data a data.frame; first column is sample ID, second column is group
#'   (must be named "group"), subsequent columns are response variables.
#'   Must contain exactly two groups.
#' @param i col index or column name of the variable to test.
#' @param is_normal logical; result of normality check (all groups pass Shapiro-Wilk).
#' @param is_homogen logical; result of variance homogeneity check (Levene/Bartlett p >= 0.05).
#' @param p.adjust.method p-value adjustment for Mann-Whitney path. Default "none".
#' @examples
#' # data(data_wt)
#' # For two-group data only
#' result = TtestSingle(data = data_2group, i = 3,
#'                      is_normal = TRUE, is_homogen = TRUE)
#' result[[1]]
#' @return list: [[1]] CLD data.frame with groups and group columns;
#'               $method character: "t_equal", "t_welch", or "mannwhitney";
#'               $test full test result object
#' @author Contact: Tao Wen \email{2018203048@@njau.edu.cn}
#' @export

TtestSingle = function(data = data_wt, i = 3,
                       is_normal = TRUE, is_homogen = TRUE,
                       p.adjust.method = "none") {

  ss <- data %>%
    dplyr::select("group", count = i)

  ss$group <- as.factor(ss$group)
  groups <- levels(ss$group)

  if (length(groups) != 2) {
    stop("TtestSingle requires exactly two groups. For more than two groups, use SingleStat.")
  }

  g1 <- ss$count[ss$group == groups[1]]
  g2 <- ss$count[ss$group == groups[2]]

  # Three sub-paths
  if (is_normal & is_homogen) {
    # Path 1: Student's t-test (equal variance)
    test_result <- t.test(g1, g2, var.equal = TRUE)
    method_used <- "t_equal"
    p_val <- test_result$p.value
    message("Two-group test: Student's t-test (normal + equal variance)")

  } else if (is_normal & !is_homogen) {
    # Path 2: Welch's t-test (unequal variance)
    test_result <- t.test(g1, g2, var.equal = FALSE)
    method_used <- "t_welch"
    p_val <- test_result$p.value
    message("Two-group test: Welch's t-test (normal + unequal variance)")

  } else {
    # Path 3: Mann-Whitney U test (non-normal)
    test_result <- wilcox.test(g1, g2, exact = FALSE)
    method_used <- "mannwhitney"
    p_val <- test_result$p.value
    # Apply p-value adjustment (for single test this only matters for consistency)
    p_val <- p.adjust(p_val, method = p.adjust.method)
    message("Two-group test: Mann-Whitney U (non-normal)")
  }

  # Build CLD output
  # For two groups: assign "a" to higher mean, "a" or "b" based on significance
  da <- ss %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(mean = mean(count)) %>%
    dplyr::arrange(desc(mean)) %>%
    as.data.frame()

  if (p_val < 0.05) {
    # Significant: different letters
    letters_assigned <- c("a", "b")
  } else {
    # Not significant: same letter
    letters_assigned <- c("a", "a")
  }

  aa <- data.frame(
    groups = letters_assigned,
    group  = da$group,
    stringsAsFactors = FALSE
  )
  row.names(aa) <- aa$group

  return(list(aa, method = method_used, test = test_result))
}
