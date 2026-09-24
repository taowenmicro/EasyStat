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
#' @param apply_omnibus_fdr logical; when TRUE (default) the omnibus p-values of
#'   all variables analysed in one call are corrected jointly, and post-hoc
#'   comparisons are computed only for variables that pass \code{fdr_threshold}.
#' @param fdr_threshold numeric; threshold applied to the adjusted omnibus
#'   p-value for post-hoc gating. Default 0.05.
#' @param omnibus_fdr_method method passed to \code{p.adjust} for the
#'   across-variable correction. Default "BH".
#' @param homogeneity_pretest logical; when TRUE (default, the original
#'   behaviour) Levene's test decides between ANOVA and Welch's ANOVA for
#'   normally distributed variables. Set FALSE to route all normal variables
#'   directly to Welch's ANOVA. Levene's test has little power at small n, so
#'   heteroscedastic variables may be routed to ANOVA precisely when that choice
#'   is least safe; FALSE is the safer setting for new analyses with small
#'   groups, and TRUE is kept as the default so that published analyses remain
#'   reproducible.
#' @param normality_adjust multiplicity adjustment for the per-group
#'   Shapiro-Wilk tests: "none" (default, the original behaviour) or
#'   "bonferroni" (alpha = 0.05/k). "bonferroni" removes the misclassification
#'   of genuinely normal variables but also lets skewed variables pass the
#'   normality screen, so it is not recommended for skewed data.
#' @examples
#' # data(data_wt)
#' result = MuiStat(data = data_wt, num = c(4,5,6), sig_show = "abc",
#'                  ncol = 2, plot = "boxbar", plottype = "mui")
#' result[[1]]
#' result$aov; result$welch; result$wlx
#' @return list: [[1]] plot; $aov $welch $wlx column indices; $table CLD
#'   data.frame (letters are NA for variables suppressed by gating); $omnibus a
#'   data.frame with the raw and across-variable adjusted omnibus p-value and
#'   the gating outcome for each variable
#' @author Contact: Tao Wen \email{2018203048@@njau.edu.cn} Jun Yuan \email{junyuan@@njau.edu.cn}
#' @export

MuiStat = function(data = data_wt, num = c(4,5,6), method_cv = "leveneTest",
                   method_Mc = "Tukey", sig_show = "abc", ncol = 2,
                   plot = "bar", plottype = "mui", path = "./output",
                   p.adjust.method = "none",
                   apply_omnibus_fdr = TRUE,
                   fdr_threshold = 0.05,
                   omnibus_fdr_method = "BH",
                   homogeneity_pretest = TRUE,
                   normality_adjust = c("none", "bonferroni")) {

  normality_adjust <- match.arg(normality_adjust)

  data$group <- as.factor(data$group)
  n_groups   <- nlevels(data$group)

  # Run normality and homogeneity tests for all variables once
  norCv <- MuiNorCV(data = data, num = num, method_cv = method_cv,
                    normality_adjust = normality_adjust)

  # Classify each variable into one of three paths
  AA <- c()   # ANOVA or Student's t-test
  CC <- c()   # Welch's ANOVA or Welch's t-test
  BB <- c()   # KW/Wilcoxon or Mann-Whitney U

  # homogeneity_pretest = TRUE  (default, original behaviour)
  #     normal & homogeneous     -> ANOVA
  #     normal & heteroscedastic -> Welch's ANOVA
  #     non-normal               -> Kruskal-Wallis
  # homogeneity_pretest = FALSE
  #     normal                   -> Welch's ANOVA (Levene's test skipped)
  #     non-normal               -> Kruskal-Wallis
  for (i in 1:length(num)) {
    is_normal  <- norCv[, "cor"][i] == TRUE
    is_homogen <- norCv[, "CV"][i]  == TRUE

    if (!is_normal) {
      BB <- c(BB, num[i])
    } else if (homogeneity_pretest && is_homogen) {
      AA <- c(AA, num[i])
    } else {
      CC <- c(CC, num[i])
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

  # ── Across-variable multiplicity control and post-hoc gating ──
  # The omnibus p-values of every variable analysed in this call are corrected
  # jointly; variables whose adjusted omnibus p-value fails fdr_threshold have
  # their compact-letter annotations set to NA, so that "tested and not
  # significant" stays distinguishable from "not tested".
  omnibus_tab <- NULL
  if (apply_omnibus_fdr && n_groups > 2) {

    path_of <- c(stats::setNames(rep("ANOVA", length(AA)), colnames(data)[AA]),
                 stats::setNames(rep("Welch", length(CC)), colnames(data)[CC]),
                 stats::setNames(rep("KW",    length(BB)), colnames(data)[BB]))

    p_raw <- c(attr(resultAA, "omnibus_p_raw"),
               attr(resultCC, "omnibus_p_raw"),
               attr(resultBB, "omnibus_p_raw"))

    if (length(p_raw) > 0) {
      p_raw <- p_raw[!is.na(names(p_raw))]
      p_fdr <- stats::p.adjust(p_raw, method = omnibus_fdr_method)
      failed <- names(p_fdr)[p_fdr > fdr_threshold]

      # gate: blank the letters of variables that did not pass
      for (v in failed) {
        if (v %in% colnames(resultall)) resultall[[v]] <- NA
      }

      omnibus_tab <- data.frame(
        variable        = names(p_raw),
        method          = unname(path_of[names(p_raw)]),
        p_omnibus_raw   = unname(p_raw),
        p_omnibus_fdr   = unname(p_fdr[names(p_raw)]),
        pass_fdr        = unname(p_fdr[names(p_raw)] <= fdr_threshold),
        stringsAsFactors = FALSE
      )

      attr(resultall, "omnibus_p_raw")   <- p_raw
      attr(resultall, "omnibus_fdr")     <- p_fdr
      attr(resultall, "failed_fdr_vars") <- failed

      if (length(failed) > 0) {
        message(sprintf(
          "Across-variable %s correction: %d of %d variables did not pass the %.3f threshold; their post-hoc comparisons were not computed (%s).",
          omnibus_fdr_method, length(failed), length(p_fdr), fdr_threshold,
          paste(failed, collapse = ", ")))
      }
    }
  }

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

  return(list(p, aov = AA, welch = CC, wlx = BB, table = resultall,
              omnibus = omnibus_tab))
}
