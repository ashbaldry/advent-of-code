input <- readLines("2021/day_13/input.txt")
input_space <- which(input == "")
coordinates <- do.call(rbind, lapply(strsplit(input[seq(input_space - 1)], ","), as.numeric)) + 1
folds <- input[-seq(input_space)]

paper <- matrix(FALSE, nrow = max(coordinates[, 2]), ncol = max(coordinates[, 1]))
paper[as.matrix(coordinates[, c(2, 1)])] <- TRUE

# Part 1
foldPaper <- function(x, fold) {
  position <- as.numeric(sub(".*=", "", fold)) + 1
  if (grepl("x", fold)) {
    x[, seq(position - 1, 1)] | x[, seq(position + 1, ncol(x))]
  } else {
    x[seq(position - 1, 1), ] | x[seq(position + 1, nrow(x)), ]
  }
}

sum(foldPaper(paper, folds[1]))

# Part 2
for (fold in folds) paper <- foldPaper(paper, fold)
matrix(rev(ifelse(paper, "O", "")), nrow = 6)
# Nicer print
cat(rev(t(cbind("\n", ifelse(paper, "O", " ")))), sep = "")
