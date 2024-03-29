col_names <- c(paste0("P", 1:10), "SEP", paste0("I", 1:4))
numbers <- read.delim("2021/day_08/input.txt", sep = " ", header = FALSE, col.names = col_names)

# Part 1
sum(sapply(numbers[, paste0("I", 1:4)], nchar) %in% c(2, 3, 4, 7))

# Part 2
getNumberOrder <- function(x) {
  y <- setNames(character(10), paste0("N", 0:9))
  x <- x[order(nchar(x), x)]
  # 1, 4, 7, 8
  y[c(2, 8, 5, 9)] <- x[c(1, 2, 3, 10)]
  # 2
  y[3] <- x[4:6][sapply(strsplit(x[4:6], ""), \(y) sum(y %in% strsplit(x[3], "")[[1]]) == 2)]
  # 3
  y[4] <- x[4:6][sapply(strsplit(x[4:6], ""), \(y) all(strsplit(x[1], "")[[1]] %in% y))]
  # 5
  y[6] <- setdiff(x[4:6], y[3:4])
  # 6
  y[7] <- x[7:9][sapply(strsplit(x[7:9], ""), \(y) !all(strsplit(x[2], "")[[1]] %in% y))]
  # 9
  y[10] <- x[7:9][sapply(strsplit(x[7:9], ""), \(y) all(strsplit(x[3], "")[[1]] %in% y))]
  # 0
  y[1] <- setdiff(x[7:9], y[c(7, 10)])
  y
}

getNumber <- function(x) {
  y <- match(x[paste0("I", 1:4)], x[paste0("N", 0:9)]) - 1
  as.numeric(paste(y, collapse = ""))
}

numbers <- sapply(numbers, \(x) sapply(lapply(strsplit(x, ""), sort), paste0, collapse = ""))
sorted_numbers <- t(apply(numbers[, paste0("P", 1:10)], 1, getNumberOrder))
numbers <- cbind(numbers, sorted_numbers)
sum(apply(numbers, 1, getNumber))
