## Regression tests for the statistical-decision layer of MuiStat().
##
## All fixtures are deterministic (no RNG), so the expected pathway of every
## variable is fixed and a change in the routing rule fails the test rather
## than shifting silently.

make_df <- function(...) {
  cols <- list(...)
  n <- length(cols[[1]])
  out <- data.frame(ID = paste0("s", seq_len(n)),
                    group = rep(c("A", "B", "C"), each = n / 3),
                    stringsAsFactors = FALSE)
  for (nm in names(cols)) out[[nm]] <- cols[[nm]]
  out
}

# symmetric, equal spread in every group -> normal + homogeneous -> ANOVA
sym_homo <- c(10, 11, 12, 13, 14,   20, 21, 22, 23, 24,   30, 31, 32, 33, 34)
# symmetric but spread differs by an order of magnitude -> normal, not homogeneous
sym_hetero <- c(10, 11, 12, 13, 14,  20, 30, 40, 50, 60,  30, 31, 32, 33, 34)
# one extreme value per group -> fails the normality screen -> Kruskal-Wallis
skewed <- c(1, 1, 1, 1, 90,  2, 2, 2, 2, 95,  3, 3, 3, 3, 99)


test_that("a single variable is routed without error (regression: num[-1])", {
  # A pathway holding exactly one variable used to index num[2] = NA.
  d <- make_df(v1 = sym_homo)
  res <- MuiStat(data = d, num = 3, plot = "box", plottype = "mui")

  expect_length(res$aov, 1L)
  expect_length(res$welch, 0L)
  expect_length(res$wlx, 0L)
  expect_true(is.data.frame(res$table))
})


test_that("a pathway holding exactly one variable is routed without error", {
  d <- make_df(v_anova = sym_homo, v_kw1 = skewed, v_kw2 = skewed)
  res <- MuiStat(data = d, num = 3:5, plot = "box", plottype = "mui")

  expect_length(res$aov, 1L)   # only one variable on the ANOVA branch
  expect_length(res$wlx, 2L)
})


test_that("the Welch pathway also works with a single variable", {
  d <- make_df(v_welch = sym_hetero, v_kw1 = skewed, v_kw2 = skewed)
  res <- MuiStat(data = d, num = 3:5, plot = "box", plottype = "mui")

  expect_length(res$welch, 1L)
  expect_length(res$wlx, 2L)
})


test_that("a constant group does not abort the batch", {
  # shapiro.test() errors on zero variance; such a variable must fall through
  # to the non-parametric branch instead of failing the whole call.
  d <- make_df(v_const = c(rep(0, 5), 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
               v_ok    = sym_homo)
  expect_error(
    MuiStat(data = d, num = 3:4, plot = "box", plottype = "mui"),
    NA
  )
})


test_that("homogeneity_pretest = FALSE sends every normal variable to Welch", {
  d <- make_df(v_homo = sym_homo, v_hetero = sym_hetero, v_kw = skewed)

  on  <- MuiStat(data = d, num = 3:5, plot = "box", plottype = "mui",
                 homogeneity_pretest = TRUE)
  off <- MuiStat(data = d, num = 3:5, plot = "box", plottype = "mui",
                 homogeneity_pretest = FALSE)

  expect_length(on$aov, 1L)          # default keeps the ANOVA branch
  expect_length(off$aov, 0L)         # FALSE empties it
  expect_length(off$welch, 2L)       # both normal variables go to Welch
  expect_equal(on$wlx, off$wlx)      # the non-parametric branch is unaffected
})


test_that("across-variable FDR gates the post-hoc letters", {
  d <- make_df(v1 = sym_homo, v2 = sym_homo, v3 = sym_homo)

  res <- MuiStat(data = d, num = 3:5, plot = "box", plottype = "mui",
                 apply_omnibus_fdr = TRUE, fdr_threshold = 0)   # gate everything

  expect_false(is.null(res$omnibus))
  expect_equal(nrow(res$omnibus), 3L)
  expect_true(all(c("p_omnibus_raw", "p_omnibus_fdr", "pass_fdr") %in%
                    names(res$omnibus)))
  expect_true(all(!res$omnibus$pass_fdr))
  # letters of gated variables are NA, not silently dropped
  expect_true(all(is.na(res$table[["v1"]])))
})


test_that("apply_omnibus_fdr = FALSE reproduces the ungated letters", {
  d <- make_df(v1 = sym_homo, v2 = sym_homo, v3 = sym_homo)

  gated  <- MuiStat(data = d, num = 3:5, plot = "box", plottype = "mui",
                    apply_omnibus_fdr = TRUE, fdr_threshold = 0)
  ungated <- MuiStat(data = d, num = 3:5, plot = "box", plottype = "mui",
                     apply_omnibus_fdr = FALSE)

  expect_true(all(is.na(gated$table[["v1"]])))
  expect_false(any(is.na(ungated$table[["v1"]])))
  expect_null(ungated$omnibus)
})
