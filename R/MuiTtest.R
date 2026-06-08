#' Batch two-sample t-test for multiple variables (independent samples)
#'
#' Batch version of TtestSingle. Applies the appropriate two-sample test to
#' multiple columns simultaneously. Reads normality and homogeneity results
#' from norCv (full matrix from MuiNorCV), matching by column index position.
#'
#' @param data a data.frame; first column is sample ID, second column is group,
#'   subsequent columns are response variables. Must contain exactly two groups.
#' @param num integer vector of column indices to test.
#' @param norCv full output of MuiNorCV for all variables being analysed.
#'   Rows correspond to the original num vector in order.
#' @param num_all the original full num vector passed to MuiStat, used to map
#'   positions in norCv. Defaults to num if not provided.
#' @param p.adjust.method p-value adjustment for Mann-Whitney path. Default "none".
#' @return data.frame of CLD letters, one column per variable, rows are groups.
#' @author Contact: Tao Wen \email{2018203048@@njau.edu.cn}
#' @export

MuiTtest = function(data = data_wt, num = c(4:6),
                    norCv = NULL, num_all = NULL,
                    p.adjust.method = "none") {

  if (is.null(norCv)) {
    norCv <- MuiNorCV(data = data, num = num, method_cv = "leveneTest")
    num_all <- num
  }

  if (is.null(num_all)) {
    num_all <- num
  }

  # Helper: find the row in norCv corresponding to column index col_idx
  # norCv rows correspond to num_all in order
  get_norCv_row <- function(col_idx) {
    pos <- which(num_all == col_idx)
    if (length(pos) == 0) stop(paste("Column index", col_idx, "not found in num_all"))
    norCv[pos, , drop = FALSE]
  }

  # Process first variable
  N <- num[1]
  row_i    <- get_norCv_row(N)
  is_normal  <- row_i[, "cor"] == TRUE
  is_homogen <- row_i[, "CV"]  == TRUE

  result <- TtestSingle(data = data, i = N,
                        is_normal = is_normal, is_homogen = is_homogen,
                        p.adjust.method = p.adjust.method)
  aa   <- result[[1]]
  name <- colnames(data[N])
  colnames(aa)[1] <- name
  aa$group <- NULL
  A <- aa

  # Process remaining variables
  if (length(num) > 1) {
    for (N in num[-1]) {
      row_i    <- get_norCv_row(N)
      is_normal  <- row_i[, "cor"] == TRUE
      is_homogen <- row_i[, "CV"]  == TRUE

      result <- TtestSingle(data = data, i = N,
                            is_normal = is_normal, is_homogen = is_homogen,
                            p.adjust.method = p.adjust.method)
      aa   <- result[[1]]
      name <- colnames(data[N])
      colnames(aa)[1] <- name
      aa$group <- NULL

      A <- merge(A, aa, by = "row.names", all = TRUE)
      row.names(A) <- A$Row.names
      A$Row.names <- NULL
    }
  }

  return(A)
}
