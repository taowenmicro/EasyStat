#' Perform Welch's ANOVA and Games-Howell post-hoc test for multiple variables
#'
#' Batch version of WelchAov. Applies Welch's ANOVA and Games-Howell post-hoc
#' test to multiple columns simultaneously. Output format is identical to
#' MuiaovMcomper2, ensuring compatibility with all downstream visualization functions.
#'
#' @param data a data.frame; first column is sample ID, second column is group,
#'   subsequent columns are response variables.
#' @param num integer vector of column indices to test.
#' @examples
#' # data(data_wt)
#' result = MuiWelchAov(data = data_wt, num = c(4:6))
#' result
#' @return data.frame of CLD letters, one column per variable, rows are groups.
#' @author Contact: Tao Wen \email{2018203048@@njau.edu.cn}
#' @export

MuiWelchAov = function(data = data_wt, num = c(4:6)) {

  data$group <- as.factor(data$group)

  N <- num[1]
  result <- WelchAov(data = data, i = N)
  aa <- result[[1]]
  name <- colnames(data[N])
  colnames(aa)[1] <- name
  aa$group <- NULL
  A <- aa

  for (N in num[-1]) {
    result <- WelchAov(data = data, i = N)
    aa <- result[[1]]
    name <- colnames(data[N])

    colnames(aa)[1] <- name
    aa <- aa[match(row.names(A), row.names(aa)), , drop = FALSE]
    aa$group <- NULL

    A <- merge(A, aa, by = "row.names", all = TRUE)
    row.names(A) <- A$Row.names
    A$Row.names <- NULL
  }

  return(A)
}
