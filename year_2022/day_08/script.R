#### Set-Up ####
heights <- readLines("input.txt") |> strsplit("") |> lapply(as.integer) |> do.call(what = rbind)

#### Part 1 ####
visible <- matrix(TRUE, nrow(heights), ncol(heights))
for (i in seq_len(nrow(heights))) {
  for (j in seq_len(ncol(heights))) {
    if (any(c(i, j) %in% c(1, nrow(heights)))) next

    curr_pos <- heights[i, j]
    visible[i, j] <- any(
      all(curr_pos > heights[seq(i - 1), j]),
      all(curr_pos > heights[seq(i + 1, 99), j]),
      all(curr_pos > heights[i, seq(j - 1)]),
      all(curr_pos > heights[i, seq(j + 1, 99)])
    )
  }
}

sum(visible)

#### Part 2 ####
scenic <- matrix(0, nrow(heights), ncol(heights))
for (i in seq_len(nrow(heights))) {
  for (j in seq_len(ncol(heights))) {
    if (any(c(i, j) %in% c(1, nrow(heights)))) next

    curr_pos <- heights[i, j]
    left <- curr_pos <= heights[rev(seq(i - 1)), j]
    right <- curr_pos <= heights[seq(i + 1, 99), j]
    top <- curr_pos <= heights[i, rev(seq(j - 1))]
    bottom <- curr_pos <= heights[i, seq(j + 1, 99)]
    scenic[i, j] <- prod(
      sum(cumsum(left) == 0) + any(left),
      sum(cumsum(right) == 0) + any(right),
      sum(cumsum(top) == 0) + any(top),
      sum(cumsum(bottom) == 0) + any(bottom)
    )
  }
}

max(scenic)
