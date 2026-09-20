#' Test normality and homogeneity of variance of Input data
#'
#' @param data a data.frame contain the input data
#' @param  i col index wtich need to test
#' @param method_cv which method to test  variance, included "leveneTest","bartlett.test".
#' @examples
#' # data(data_wt)
#' NorNorCVTest(data = data_wt, i= 4)
#' @return data frame
#' @author Contact: Tao Wen \email{2018203048@@njau.edu.cn} Jun Yuan \email{junyuan@@njau.edu.cn}
#' @references
#'
#' Yuan J, Zhao J, Wen T, Zhao M, Li R, Goossens P, Huang Q, Bai Y, Vivanco JM, Kowalchuk GA, Berendsen RL, Shen Q
#' Root exudates drive the soil-borne legacy of aboveground pathogen infection
#' Microbiome 2018,DOI: \url{doi: 10.1186/s40168-018-0537-x}
#' @export


NorNorCVTest = function(data = data_wt, i= 4,method_cv = "leveneTest",
                        normality_adjust = c("none","bonferroni"),...){
  normality_adjust <- match.arg(normality_adjust)
  ##----Test normality and homogeneity of variance of Input data-------
  ss <- data %>%
    dplyr::select("group", count = dplyr::all_of(i))
  shapiro.test.multi <- function(
    data,
    value,
    group)
  {
    table(data[,group]) %>%
      data.frame(.) -> a1
    a2 <- as.vector(a1[,1])
    data = data.frame(group = data[,group],  #
                      value = data[,value])  #

    test.result <- data.frame(No=0,        #row number
                              Name=0,      #group name
                              W=0,         #W value
                              p.value=0,   #p value
                              norm.test=0) #result
    # multiplicity of the per-group tests: applying alpha = 0.05 to each of k
    # groups misclassifies a proportion 1-(1-alpha)^k of genuinely normal
    # variables (18.5% with k = 4). "bonferroni" uses alpha = 0.05/k instead.
    alpha <- if (identical(normality_adjust, "bonferroni")) 0.05/length(a2) else 0.05

    for (i in (1:length(a2))){
      # subgroup for shapiro.text
      subset(data,
             group == a2[i],
             select = value) %>%
        .[,1] -> x.i

      # shapiro.test() aborts on a constant group; such a group cannot satisfy
      # the normality assumption, so treat it as non-normal and route the
      # variable to the non-parametric branch rather than failing the batch.
      x.i <- x.i[!is.na(x.i)]
      if (length(x.i) < 3 || stats::var(x.i) == 0) {
        t.r <- list(statistic = NA_real_, p.value = 0)
      } else {
        t.r <- shapiro.test(x.i)
      }
      test.result[i,1] = i              #group number
      test.result[i,2] = a2[i]          #group name
      test.result[i,3] = t.r$statistic  #w value
      test.result[i,4] = t.r$p.value    #p value
      if
      (t.r$p.value > alpha)
        test.result[i,5] = "Norm"
      else
        test.result[i,5] = "Other_situation"
    }
    test.result[nrow( test.result)+1,1] = "Test Method:"  # final col add  the method of text
    test.result[nrow( test.result),2] = "Shapiro-Wilk"    #final col two row add the method
    return(test.result)
  }
  a = shapiro.test.multi(data[c(2,i)],value = colnames(data[c(2,i)][2]),group = "group")
  a
  # selected the method for tast the Homogeneity of variance
  # p2 >=.05:Homogeneity of variance
  # p2 is NOT rounded: rounding to 3 decimals let a single rounding step decide
  # the pathway of variables whose homogeneity p-value sits on the boundary.
  if (method_cv == "leveneTest" ) {
    xc <- car::leveneTest(count~group,data=ss)
    p2 <- xc[[3]][1]
  }
  if (method_cv == "bartlett.test" ) {
    xc <- bartlett.test(count~group,data=ss)
    p2 <- xc[[3]]
  }
  # a constant variable yields NaN here; treat it as non-homogeneous so the
  # variable is not silently routed to ANOVA
  if (length(p2) == 0 || is.na(p2)) p2 <- 0
  return(list(a,p2))
}
