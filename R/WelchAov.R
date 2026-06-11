#' Perform Welch's ANOVA and Games-Howell post-hoc test for heterogeneous variance data
#'
#' When data passes normality test but fails homogeneity of variance test,
#' Welch's ANOVA is more appropriate than standard one-way ANOVA.
#' Post-hoc comparisons use Games-Howell test, which does not assume equal variances.
#'
#' @param data a data.frame; first column is sample ID, second column is group,
#'   subsequent columns are response variables.
#' @param i col index or column name of the variable to test.
#' @examples
#' # data(data_wt)
#' result = WelchAov(data = data_wt, i = 5)
#' result[[1]]
#' @return list: [[1]] data.frame of CLD letters with group column;
#'               [[2]] Welch's ANOVA summary;
#'               [[3]] Games-Howell pairwise results
#' @author Contact: Tao Wen \email{2018203048@@njau.edu.cn}
#' @export

WelchAov = function(data = data_wt, i = 3) {

  ss <- data %>%
    dplyr::select("group", count = i)

  ss$group <- as.factor(ss$group)

  # Welch's one-way ANOVA (does not assume equal variances)
  welch_model <- oneway.test(count ~ group, data = ss, var.equal = FALSE)

  # Games-Howell post-hoc test (does not assume equal variances or equal sample sizes)
  gh_result <- rstatix::games_howell_test(ss, count ~ group)

  # Build pairwise p-value vector for multcompLetters
  # Need format: "groupA-groupB" = p.value
  gh_df <- as.data.frame(gh_result)

  # Compute group means for sorting
  da <- ss %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(mean = mean(count)) %>%
    dplyr::arrange(desc(mean)) %>%
    as.data.frame()

  # Build named p-value vector
  p_vals <- gh_df$p.adj
  names(p_vals) <- paste(gh_df$group1, gh_df$group2, sep = "-")

  # Also add reverse direction to ensure multcompLetters can find all pairs
  p_vals_rev <- gh_df$p.adj
  names(p_vals_rev) <- paste(gh_df$group2, gh_df$group1, sep = "-")
  p_vals_all <- c(p_vals, p_vals_rev)

  # Generate compact letter display
  letters_raw <- multcompView::multcompLetters(p_vals_all, threshold = 0.05)["Letters"]
  aa <- data.frame(groups = letters_raw$Letters)
  aa$group <- row.names(aa)

  # Sort letters by group mean (highest mean = "a")
  dat <- da %>%
    dplyr::inner_join(aa, by = "group") %>%
    dplyr::arrange(desc(mean))

  tmp.1 <- dat$groups %>% unique()
  tmp.2 <- data.frame(ori = tmp.1, new = sort(tmp.1, decreasing = FALSE))

  for (k in 1:nrow(dat)) {
    dat[k, "groups"] <- tmp.2$new[match(dat[k, "groups"], tmp.2$ori)]
  }

  dat$mean <- NULL
  dat <- dat[match(dat$group, aa$group), ][, c("groups", "group")] %>%
    as.data.frame()
  row.names(dat) <- dat$group

  return(list(dat, welch = welch_model, posthoc = gh_result))
}
