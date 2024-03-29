input <- readLines("2021/day_05/input.txt")

getNDuplicatedPoints <- function(lines, include_diagonals = FALSE) {
  all_positions <- lapply(lines, \(x) {
    # x1, y1, x2, y2
    line <- as.integer(strsplit(x, ",| -> ")[[1]])
    if (line[1] == line[3] || line[2] == line[4] || include_diagonals) {
      data.frame(x = seq(line[1], line[3]), y = seq(line[2], line[4]))
    } else {
      data.frame(x = integer(0), y = integer(0))
    }
  })

  all_positions_combined <- do.call(rbind, all_positions)
  nrow(unique(all_positions_combined[duplicated(all_positions_combined), ]))
}

# Part 1
getNDuplicatedPoints(input)

# Part 2
getNDuplicatedPoints(input, include_diagonals = TRUE)
