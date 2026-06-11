#' A complete single-factor difference analysis process for single col data.
#'
#' First checks number of groups, then applies the appropriate decision rule:
#'
#' Two groups:
#' (1) Normal + equal variance    -> Student's t-test
#' (2) Normal + unequal variance  -> Welch's t-test
#' (3) Non-normal                 -> Mann-Whitney U test
#'
#' More than two groups:
#' (1) Normal AND homogeneous variance   -> one-way ANOVA (aovMcomper2)
#' (2) Normal BUT heterogeneous variance -> Welch's ANOVA (WelchAov)
#' (3) Non-normal                        -> Kruskal-Wallis + Wilcoxon (KwWlx2)
#'
#' @param data a data.frame; first column is sample ID, second column is group
#'   (must be named "group"), subsequent columns are response variables.
#' @param i col index or column name of the variable to test.
#' @param method_Mc post-hoc method for ANOVA: "Tukey","LSD","SNK","Duncan","scheffe".
#' @param sig_show significance display: "abc" or "line".
#' @param plot plot type: "bar", "box", or "boxbar".
#' @param p.adjust.method p-value adjustment for non-parametric tests. Default "none".
#' @examples
#' # data(data_wt)
#' result = SingleStat(data = data_wt, i = 4, plot = "boxbar",
#'                     method_Mc = "Tukey", sig_show = "abc")
#' result[[1]]
#' result$table   # CLD result
#' result$method  # method chosen: "aov","welch_anova","wlx","t_equal","t_welch","mannwhitney"
#' @return list: [[1]] plot; $table CLD data.frame; $method character
#' @author Contact: Tao Wen \email{2018203048@@njau.edu.cn}
#' @export

SingleStat = function(data = data_wt, plot = "bar", method_Mc = "Tukey",
                      i = 4, sig_show = "abc", p.adjust.method = "none") {

  data$group <- as.factor(data$group)
  n_groups <- nlevels(data$group)

  # Pre-tests
  NorCV      <- NorNorCVTest(data = data, i = i, method_cv = "leveneTest")
  norm_result <- NorCV[[1]]
  homogen_p   <- NorCV[[2]]

  is_normal  <- length(norm_result$p.value[-nrow(norm_result)][
    norm_result$p.value[-nrow(norm_result)] < 0.05]) == 0
  is_homogen <- homogen_p >= 0.05

  # ── Two-group path ──
  if (n_groups == 2) {

    result <- TtestSingle(data = data, i = i,
                          is_normal = is_normal, is_homogen = is_homogen,
                          p.adjust.method = p.adjust.method)
    A <- result$method
    message(paste("SingleStat method selected:", A))

  # ── Multi-group path ──
  } else {

    if (is_normal & is_homogen) {
      result <- aovMcomper2(data = data, i = i, method_Mc = method_Mc)
      A <- "aov"
      message("Statistical method: one-way ANOVA (normal + homogeneous variance)")

    } else if (is_normal & !is_homogen) {
      result <- WelchAov(data = data, i = i)
      A <- "welch_anova"
      message("Statistical method: Welch's ANOVA (normal + heterogeneous variance)")

    } else {
      result <- KwWlx2(data = data, i = i, p.adjust.method = p.adjust.method)
      A <- "wlx"
      message("Statistical method: Kruskal-Wallis + Wilcoxon (non-normal)")
    }
  }

  # Visualization
  if (plot == "bar") {
    p <- aovMuiBarPlot(data = data, i = i,
                       sig_show = sig_show, result = result[[1]])[[1]]
  } else if (plot == "box") {
    p <- aovMuiBoxP2(data = data, i = i,
                     sig_show = sig_show, result = result[[1]])[[1]]
  } else if (plot == "boxbar") {
    p <- aovMuiBoxBarP(data = data, i = i,
                       sig_show = sig_show, result = result[[1]])[[1]]
  }

  return(list(p, table = result[[1]], method = A))
}
