#' Ordered barplots
#'
#' @details Function to make a barplot from a matrix with argument
#'   beside = FALSE with each column ordered according to its value.
#' @param height see ?barplot
#' @param col The colors of the bars. This argument must be provided.
#' @param decr A boolean indicating the decreasing (TRUE) or increasing (FALSE)
#'   order.
#' @param names.arg see ?barplot
#' @param ... Additional arguments, see ?barplot.
#' @return A vector with bar positions like in the original barplot function.
#' @examples
#' set.seed(1234)
#' dataset <- matrix(sample(1:20, 104, replace = TRUE), ncol = 26)
#' myCol <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A")
#' graphics::barplot(height = dataset, col = myCol, names.arg = LETTERS)
#' barplotOr(height = dataset, col = myCol, names.arg = LETTERS)
#' @export
barplotOr <- function(height, col, decr = TRUE, names.arg = "", ...){
  dfi <- data.frame(
    height[,1],
    col[1:nrow(height)])[order(height[,1], decreasing = decr),]
  defBarplot <- graphics::barplot(height = height, plot = FALSE)
  res <- barplot(
    height = matrix(dfi[, 1], ncol = 1),
    col = as.character(dfi[, 2]),
    space = defBarplot[1] - 0.5,
    xlim = c(0, max(defBarplot)),
    ylim = c(0, max(colSums(height))),
    names.arg = names.arg[1], ...)
  res2 <- sapply(2:ncol(height), function(i){
    dfi <- data.frame(
      height[,i],
      col[1:nrow(height)])[order(height[,i], decreasing = decr),]
    graphics::barplot(
      height = matrix(dfi[, 1], ncol = 1),
      col = as.character(dfi[, 2]),
      add = TRUE,
      space = defBarplot[i] - 0.5,
      axes = FALSE,
      names.arg = names.arg[i], ...)
  })
  return(c(res, res2))
}
