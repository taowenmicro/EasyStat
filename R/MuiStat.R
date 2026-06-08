#' A complete single-factor difference analysis process for mui col data, including normal test,
#' homogeneity analysis of variance, selection of variance analysis or non-parametric test,
#' selection of candidate visualization method.
#'
#' First checks number of groups, then applies the appropriate decision rule:
#'
#' Two groups:
#' (1) Normal + equal variance    -> Student's t-test
#' (2) Normal + unequal variance  -> Welch's t-test
#' (3) Non-normal                 -> Mann-Whitney U test
#'
#' More than two groups:
#' (1) Normal AND homogeneous variance   -> one-way ANOVA (MuiaovMcomper2)
#' (2) Normal BUT heterogeneous variance -> Welch's ANOVA (MuiWelchAov)
#' (3) Non-normal                        -> Kruskal-Wallis + Wilcoxon (MuiKwWlx2)
#'
#' @param data a data.frame; first column is sample ID, second column is group
#'   (must be named "group"), subsequent columns are response variables.
#' @param num col index vector which need to test
#' @param method_cv method for variance homogeneity test: "leveneTest" or "bartlett.test"
#' @param method_Mc method for ANOVA post-hoc: "Tukey","LSD","SNK","Duncan","scheffe"
#' @param sig_show significance display: "abc" or "line"
#' @param plot plot type: "bar", "box", or "boxbar"
#' @param plottype output type: "single" or "mui"
#' @param ncol columns in faceted display
#' @param path folder path for saving plots when plottype = "single"
#' @param p.adjust.method p-value adjustment for non-parametric path. Default "none".
#' @examples
#' # data(data_wt)
#' result = MuiStat(data = data_wt, num = c(4,5,6), sig_show = "abc",
#'                  ncol = 2, plot = "boxbar", plottype = "mui")
#' result[[1]]
#' result$aov; result$welch; result$wlx
#' @return list: [[1]] plot; $aov $welch $wlx column indices; $table CLD data.frame
#' @author Contact: Tao Wen \email{2018203048@@njau.edu.cn} Jun Yuan \email{junyuan@@njau.edu.cn}
#' @export

MuiStat = function(data = data_wt, num = c(4,5,6), method_cv = "leveneTest",
                   method_Mc = "Tukey", sig_show = "abc", ncol = 2,
                   plot = "bar", plottype = "mui", path = "./output",
                   p.adjust.method = "none") {

  data$group <- as.factor(data$group)
  n_groups   <- nlevels(data$group)

  # Run normality and homogeneity tests for all variables once
  norCv <- MuiNorCV(data = data, num = num, method_cv = method_cv)

  # Classify each variable into one of three paths
  AA <- c()   # ANOVA or Student's t-test
  CC <- c()   # Welch's ANOVA or Welch's t-test
  BB <- c()   # KW/Wilcoxon or Mann-Whitney U

  for (i in 1:length(num)) {
    is_normal  <- norCv[, "cor"][i] == TRUE
    is_homogen <- norCv[, "CV"][i]  == TRUE

    if (is_normal & is_homogen) {
      AA <- c(AA, num[i])
    } else if (is_normal & !is_homogen) {
      CC <- c(CC, num[i])
    } else {
      BB <- c(BB, num[i])
    }
  }

  # ── Two-group path: use t-test family ──
  if (n_groups == 2) {
    message("Two groups detected: using t-test family instead of ANOVA/KW.")

    if (!is.null(AA)) {
      resultAA <- MuiTtest(data = data, num = AA,
                           norCv = norCv, num_all = num,
                           p.adjust.method = p.adjust.method)
    } else { resultAA <- NULL }

    if (!is.null(CC)) {
      resultCC <- MuiTtest(data = data, num = CC,
                           norCv = norCv, num_all = num,
                           p.adjust.method = p.adjust.method)
    } else { resultCC <- NULL }

    if (!is.null(BB)) {
      resultBB <- MuiTtest(data = data, num = BB,
                           norCv = norCv, num_all = num,
                           p.adjust.method = p.adjust.method)
    } else { resultBB <- NULL }

  # ── Multi-group path: use ANOVA / Welch's ANOVA / KW ──
  } else {
    if (!is.null(AA)) {
      resultAA <- MuiaovMcomper2(data = data, num = AA, method_Mc = method_Mc)
    } else { resultAA <- NULL }

    if (!is.null(CC)) {
      resultCC <- MuiWelchAov(data = data, num = CC)
    } else { resultCC <- NULL }

    if (!is.null(BB)) {
      resultBB <- MuiKwWlx2(data = data, num = BB,
                             p.adjust.method = p.adjust.method)
    } else { resultBB <- NULL }
  }

  # Combine all results
  result_list <- Filter(Negate(is.null), list(resultAA, resultCC, resultBB))

  if (length(result_list) == 1) {
    resultall <- result_list[[1]]
  } else {
    resultall <- result_list[[1]]
    for (k in 2:length(result_list)) {
      resultall <- merge(resultall, result_list[[k]],
                         by = "row.names", all = TRUE)
      row.names(resultall) <- resultall$Row.names
      resultall$Row.names  <- NULL
    }
  }

  num_ordered <- c(AA, CC, BB)

  # Visualization
  if (plottype == "single") {
    if (plot == "bar") {
      MuiPlotresultBar(data = data, num = num_ordered, result = resultall,
                       sig_show = sig_show, path = path)
      p <- "Folder"
    }
    if (plot %in% c("box", "boxbar")) {
      MuiPlotresultBox(data = data, num = num_ordered, result = resultall,
                       sig_show = sig_show, path = path)
      p <- "Folder"
    }
  }

  if (plottype == "mui") {
    if (plot == "bar") {
      result1 <- FacetMuiPlotresultBar(data = data, num = num_ordered,
                                        result = resultall, sig_show = sig_show, ncol = ncol)
      p <- result1[[1]]
    }
    if (plot == "box") {
      result1 <- FacetMuiPlotresultBox(data = data, num = num_ordered,
                                        result = resultall, sig_show = sig_show, ncol = ncol)
      p <- result1[[1]]
    }
    if (plot == "boxbar") {
      result1 <- FacetMuiPlotReBoxBar(data = data, num = num_ordered,
                                       result = resultall, sig_show = sig_show, ncol = ncol)
      p <- result1[[1]]
    }
  }

  return(list(p, aov = AA, welch = CC, wlx = BB, table = resultall))
}
