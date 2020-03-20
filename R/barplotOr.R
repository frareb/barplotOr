### barplotOr

barplotOr <- function(height, col, decr = TRUE, names.arg = "", ...){
  dfi <- data.frame(
    height[,1],
    col[1:nrow(height)])[order(height[,1], decreasing = decr),]
  res <- barplot(
    height = matrix(dfi[, 1], ncol = 1),
    col = as.character(dfi[, 2]),
    xlim = c(0, ncol(height)),
    ylim = c(0, max(colSums(height))),
    space = 0.1,
    width = 0.8,
    names.arg = names.arg[1], ...)
  res2 <- sapply(2:ncol(height), function(i){
    dfi <- data.frame(
      height[,i],
      col[1:nrow(height)])[order(height[,i], decreasing = decr),]
    barplot(
      height = matrix(dfi[, 1], ncol = 1),
      col = as.character(dfi[, 2]),
      add = TRUE,
      space = (i - 1)*1.2,
      width = 0.8,
      axes = FALSE,
      names.arg = names.arg[i], ...)
  })
  return(c(res, res2))
}

# set.seed(1234)
# dataset <- matrix(sample(1:20, 104, replace = TRUE), ncol = 26)
# myCol <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",
#            "#66A61E", "#E6AB02", "#A6761D", "#666666")
# barplot  (height = dataset, col = myCol, names.arg = LETTERS)
# barplotOr(height = dataset, col = myCol, names.arg = LETTERS)
#
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
