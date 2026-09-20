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

  # collect the omnibus p-value of every variable, in the order given by num
  omnibus_p_values <- numeric(length(num))
  names(omnibus_p_values) <- colnames(data)[num]

  N <- num[1]
  result <- WelchAov(data = data, i = N)
  aa <- result[[1]]
  omnibus_p_values[1] <- result$p_omnibus_raw
  name <- colnames(data[N])
  colnames(aa)[1] <- name
  aa$group <- NULL
  A <- aa

  # seq_along(num)[-1] is empty when num holds a single variable;
  # 2:length(num) would expand to c(2, 1) there and index num[2] = NA
  for (idx in seq_along(num)[-1]) {
    N <- num[idx]
    result <- WelchAov(data = data, i = N)
    aa <- result[[1]]
    omnibus_p_values[idx] <- result$p_omnibus_raw
    name <- colnames(data[N])

    colnames(aa)[1] <- name
    aa <- aa[match(row.names(A), row.names(aa)), , drop = FALSE]
    aa$group <- NULL

    A <- merge(A, aa, by = "row.names", all = TRUE)
    row.names(A) <- A$Row.names
    A$Row.names <- NULL
  }

  attr(A, "omnibus_p_raw") <- omnibus_p_values
  return(A)
}
