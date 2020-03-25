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

# set.seed(1234)
#
# dataset <- matrix(sample(1:20, 13*8, replace = TRUE), ncol = 13)
# myCol <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",
#            "#66A61E", "#E6AB02", "#A6761D", "#666666")
# barplot  (height = dataset, col = myCol, names.arg = LETTERS[1:13])
# barplotOr(height = dataset, col = myCol, names.arg = LETTERS[1:13])
#
# dataset <- matrix(sample(10:20, 13*8, replace = TRUE), ncol = 13)
# myCol <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",
#            "#66A61E", "#E6AB02", "#A6761D", "#666666")
# # number in the barplot
# aBarPlot <- barplotOr(
#   height = dataset,
#   col = myCol, decr = FALSE,
#   names.arg = LETTERS[1:13])
# trash <- lapply(1:ncol(dataset), function(j){
#   sapply(1:nrow(dataset), function(i){
#     text(
#       x = aBarPlot[j],
#       y = cumsum(dataset[order(dataset[,j]), j])[i],
#       labels = dataset[order(dataset[,j])[i], j],
#       pos = 1
#     )
#   })
# })
# # position in the barplot
# aBarPlot <- barplotOr(
#   height = dataset,
#   col = myCol, decr = FALSE,
#   names.arg = LETTERS[1:13])
# trash <- lapply(1:ncol(dataset), function(j){
#   sapply(1:nrow(dataset), function(i){
#     text(
#       x = aBarPlot[j],
#       y = cumsum(dataset[order(dataset[,j]), j])[i],
#       labels = rev(rank(
#         dataset[order(dataset[,j]), j]))[i],
#       pos = 1
#     )
#   })
# })
#
# # number in the barplot
# aBarPlot <- barplotOr(
#   height = dataset,
#   col = myCol, decr = TRUE,
#   names.arg = LETTERS[1:13])
# trash <- lapply(1:ncol(dataset), function(j){
#   sapply(1:nrow(dataset), function(i){
#     text(
#       x = aBarPlot[j],
#       y = cumsum(dataset[order(dataset[,j], decreasing = TRUE), j])[i],
#       labels = dataset[order(dataset[,j], decreasing = TRUE)[i], j],
#       pos = 1
#     )
#   })
# })
# # position in the barplot
# aBarPlot <- barplotOr(
#   height = dataset,
#   col = myCol, decr = TRUE,
#   names.arg = LETTERS[1:13])
# trash <- lapply(1:ncol(dataset), function(j){
#   sapply(1:nrow(dataset), function(i){
#     text(
#       x = aBarPlot[j],
#       y = cumsum(dataset[order(dataset[,j], decreasing = TRUE), j])[i],
#       labels = rev(rank(
#         dataset[order(dataset[,j], decreasing = TRUE), j]))[i],
#       pos = 1
#     )
#   })
# })



# # occurrences of letters
# getOccuLet <- function(myURL){
#   xx <- try(readLines(con = myURL))
#   xx <- paste(xx, collapse = "")
#   xx <- tolower(xx)
#   xx <- trimws(xx)
#   xx <- gsub(pattern = "à|â|ã|ä|å", replacement = "a", x = xx)
#   xx <- gsub(pattern = "é|ê|è", replacement = "e", x = xx)
#   xx <- gsub(pattern = "ð|ø|ù|û|ò|ò|ÿ|ó|á|ü|ú|ì|þ|î|ï|ë|ô|õ|ö|æ|í|ß|ç", replacement = "", x = xx)
#   xx <- gsub(pattern = "ñ", replacement = "n", x = xx)
#   xx <- gsub(pattern = "[^[:alpha:]]", replacement = "", x = xx)
#   xx <- strsplit(xx, "")[[1]]
#   return(table(xx))
# }
# getFR <- as.data.frame(getOccuLet(myURL = "https://fr.wikipedia.org/"))
# getES <- as.data.frame(getOccuLet(myURL = "https://es.wikipedia.org/"))
# getIT <- as.data.frame(getOccuLet(myURL = "https://it.wikipedia.org/"))
# getEN <- as.data.frame(getOccuLet(myURL = "https://en.wikipedia.org/"))
# getPT <- as.data.frame(getOccuLet(myURL = "https://pt.wikipedia.org/"))
# getDE <- as.data.frame(getOccuLet(myURL = "https://de.wikipedia.org/"))
# getPL <- as.data.frame(getOccuLet(myURL = "https://pl.wikipedia.org/"))
#
# all <- list(getFR, getES, getIT, getEN, getPT, getDE, getPL)
# res <- Reduce(function(x, y) merge(x, y, by = "xx"), all)
# myCol <- sample(x = colors(), size = 26)
# barplotOr(
#   height = as.matrix(res[,2:ncol(res)]),
#   col = myCol, width = 1,
#   names.arg = c("FR", "ES", "IT", "EN", "PT", "DE", "PL"))
# legend("topright", legend = res[,1], fill = myCol, ncol = 6)
#
# barplot(
#   height = as.matrix(res[,2:ncol(res)]),
#   col = myCol, width = 1,
#   names.arg = c("FR", "ES", "IT", "EN", "PT", "DE", "PL"))
# legend("topright", legend = res[,1], fill = myCol, ncol = 6)

