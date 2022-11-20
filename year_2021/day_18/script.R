snailfish <- readLines("2021/day_18/input.txt")

findReduction <- function(x) {
  explode_loc <- findExplodeLocations(x)
  split_loc <- findSplitLocations(x)

  if (length(explode_loc) > 0) {
    explodePair(x, explode_loc[1])
  } else if (length(split_loc) > 0) {
    splitPair(x, split_loc[1])
  } else {
    x
  }
}

findExplodeLocations <- function(x, depth = 4) {
  pairs <- gregexpr("\\[\\d+,\\d+\\]", x)[[1]]
  opens <- cumsum((strsplit(x, "")[[1]] == "[") - (strsplit(x, "")[[1]] == "]"))
  pairs[opens[pairs] > depth]
}

findSplitLocations <- function(x) setdiff(as.numeric(gregexpr("\\d{2}", x)[[1]]), -1)

explodePair <- function(x, pair_loc, recurse = TRUE) {
  num1 <- num2 <- 0
  init_str <- substr(x, 1, pair_loc - 1)
  pair_str <- sub("(.?\\]).*", "\\1", substr(x, pair_loc, nchar(x)))
  first_num <- as.numeric(sub("\\[(\\d+),.*", "\\1", pair_str))
  second_num <- as.numeric(sub("\\[.*,(\\d+)\\]", "\\1", pair_str))
  end_str <- sub(pair_str, "", fixed = TRUE, substr(x, pair_loc, nchar(x)))

  if (grepl("\\d", init_str)) {
    num1 <- as.numeric(sub(".*[^0-9](\\d+)(?=[^0-9]+$).*", "\\1", init_str, perl = TRUE)) + first_num
    init_str <- sub("(?!<[^0-9])(\\d+)(?=[^0-9]+$)", num1, init_str, perl = TRUE)
  }

  if (grepl("\\d", end_str)) {
    num2 <- as.numeric(regmatches(end_str, regexpr("\\d+", end_str))) + second_num
    end_str <- sub("\\d+", num2, end_str)
  }

  findReduction(paste0(init_str, 0, end_str))
}

splitPair <- function(x, number_loc, recurse = TRUE) {
  init_str <- substr(x, 1, number_loc - 1)
  end_str <- substr(x, number_loc + 2, nchar(x))
  number <- as.numeric(substr(x, number_loc, number_loc + 1))

  findReduction(paste0(init_str, "[", floor(number / 2), ",", ceiling(number / 2), "]", end_str))
}

sumFish <- function(x) {
  pairs <- gregexpr("\\[\\d+,\\d+\\]", x)
  pair_str <- regmatches(x, pairs)[[1]]
  pair_num <- as.numeric(sub("^.?(\\d+).*", "\\1", pair_str)) * 3 +
    as.numeric(sub(".*,(\\d+)\\]", "\\1", pair_str)) * 2
  new_x <- paste0(strsplit(x, "\\[\\d+,\\d+\\]")[[1]], c(pair_num, ""), collapse = "")
  if (grepl("^\\[", new_x)) new_x <- sumFish(new_x)
  as.numeric(new_x)
}

snailfish <- readLines("2021/day_18/input.txt")

# Part 1
fish_sum <- snailfish[1]
for (i in seq_along(snailfish)[-1]) {
  fish_sum <- findReduction(paste0("[", fish_sum, ",", snailfish[i], "]"))
}
sumFish(fish_sum)


# Part 2
combinations <- cbind(rep(snailfish, each = 100), rep(snailfish, times = 100))
combinations <- combinations[combinations[, 1] != combinations[, 2], ]
combinations_str <- paste0("[", combinations[, 1], ",", combinations[, 2], "]")

differences <- sapply(combinations_str, \(x) sumFish(findReduction(x)))
max(differences)
