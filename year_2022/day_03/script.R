#### Set-Up ####
rucksacks <- readLines("input.txt")

#### Part 1 ####
split_rucksack <- function(x) {
  strsplit(c(substr(x, 1, nchar(x) / 2), substr(x, nchar(x) / 2 + 1, nchar(x))), "")
}
rucksacks_splits <- lapply(rucksacks, split_rucksack)
rucksacks_intersects <- vapply(rucksacks_splits, \(x) intersect(x[[1]], x[[2]]), character(1L))
sum(match(rucksacks_intersects, c(letters, LETTERS)))

#### Part 2 ####
rucksacks_splits2 <- lapply(rucksacks, \(x) unlist(strsplit(x, "")))
rucksacks_intersects2 <- vector("character", 100)
for (i in seq(length(rucksacks_splits2) / 3)) {
  rucksacks_intersects2[i] <- Reduce(intersect, rucksacks_splits2[1:3 + 3 * (i - 1)])
}
sum(match(rucksacks_intersects2, c(letters, LETTERS)))
