snailfish <- readLines("2021/day_18/input.txt")
# snailfish <- paste0("[", 1:6, ",", 1:6, "]")
# snailfish <- readLines("2021/day_18/example.txt")

reduce <- function(x) {
  reduce_info <- findReduction(x)

  while (reduce_info$reducible) {
    locations <- reduce_info$location
    x <- get(reduce_info$type)(x, locations[1])
    reduce_info <- findReduction(x)
  }

  x
}

findReduction <- function(x) {
  explode_loc <- findExplodeLocations(x)
  split_loc <- findSplitLocations(x)

  if (length(explode_loc) > 0) {
    list(reducible = TRUE, type = "explodePair", location = explode_loc)
  } else if (length(split_loc) > 0) {
    list(reducible = TRUE, type = "splitPair", location = split_loc)
  } else {
    list(reducible = FALSE)
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

  x_new <- paste0(init_str, 0, end_str)
  # cat("Explode:", x_new, "\n")

  explode_loc <- findExplodeLocations(x_new)
  split_loc <- findSplitLocations(x_new)
  if (length(explode_loc) > 0) {
    x_new <- explodePair(x_new, explode_loc[1])
  }

  x_new
}

splitPair <- function(x, number_loc, recurse = TRUE) {
  init_str <- substr(x, 1, number_loc - 1)
  end_str <- substr(x, number_loc + 2, nchar(x))
  number <- as.numeric(substr(x, number_loc, number_loc + 1))

  x_new <- paste0(init_str, "[", floor(number / 2), ",", ceiling(number / 2), "]", end_str)
  # cat("Split:  ", x_new, "\n")

  explode_loc <- findExplodeLocations(x_new)
  if (length(explode_loc) > 0) {
    x_new <- explodePair(x_new, explode_loc[1])
  }

  split_loc <- findSplitLocations(x_new)
  if (length(split_loc) > 0) {
    x_new <- splitPair(x_new, split_loc[1])
  }

  x_new
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

# Part 1
fish_sum <- snailfish[1]
for (i in seq_along(snailfish)[-1]) {
  fish_sum <- reduce(paste0("[", fish_sum, ",", snailfish[i], "]"))
}
sumFish(fish_sum)


# Part 2
combinations <- cbind(rep(snailfish, each = 100), rep(snailfish, times = 100))
combinations <- combinations[combinations[, 1] != combinations[, 2], ]
combinations_str <- paste0("[", combinations[, 1], ",", combinations[, 2], "]")

differences <- sapply(combinations_str, \(x) sumFish(reduce(x)))
max(differences)
