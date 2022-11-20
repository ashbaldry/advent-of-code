bingo_calls <- readLines("2021/day_04/calls.txt")
bingo_calls <- as.numeric(strsplit(bingo_calls, ",")[[1]])

bingo_input <- readLines("2021/day_04/input.txt")
bingo_input <- bingo_input[bingo_input != ""]
bingo_boards <- sapply(split(bingo_input, (seq_along(bingo_input) - 1) %/% 5), \(x) {
  do.call(rbind, lapply(strsplit(trimws(x), " +"), as.numeric))
})
bingo_boards <- array(bingo_boards, dim = c(5, 5, 100))

# Part 1
findBingoWinner <- function(bingo_boards, bingo_calls, bingo_match = array(FALSE, dim = dim(bingo_boards)), i = 1) {
  bingo_match <- bingo_match | bingo_boards == bingo_calls[i]

  full_column <- which(apply(apply(bingo_match, 2, colSums), 1, \(x) any(x == 5)))
  full_row <- which(apply(apply(bingo_match, 1, colSums), 1, \(x) any(x == 5)))
  if (length(full_column) > 0 | length(full_row) > 0) {
    index <- unique(c(full_column, full_row))
    list(board = bingo_boards[, , index], calls = bingo_match[, , index], last_call = bingo_calls[i])
  } else {
    findBingoWinner(bingo_boards, bingo_calls, bingo_match, i = i + 1)
  }
}

winning_board <- findBingoWinner(bingo_boards, bingo_calls)
sum(winning_board$board[!winning_board$calls]) * winning_board$last_call

# Part 2
findBingoLoser <- function(bingo_boards, bingo_calls, bingo_match = array(FALSE, dim = dim(bingo_boards)), i = 1) {
  bingo_match_input <- bingo_match
  bingo_match <- bingo_match | bingo_boards == bingo_calls[i]

  full_column <- sum(apply(apply(bingo_match, 2, colSums), 1, \(x) any(x == 5)))
  full_row <- sum(apply(apply(bingo_match, 1, colSums), 1, \(x) any(x == 5)))

  if (full_column == 100) {
    index <- which(apply(apply(bingo_match_input, 2, colSums), 1, \(x) all(x != 5)))
    list(board = bingo_boards[, , index], calls = bingo_match[, , index], last_call = bingo_calls[i])
  } else if (full_row == 100) {
    index <- which(apply(apply(bingo_match_input, 1, colSums), 1, \(x) all(x != 5)))
    list(board = bingo_boards[, , index], calls = bingo_match[, , index], last_call = bingo_calls[i])
  } else {
    findBingoLoser(bingo_boards, bingo_calls, bingo_match, i = i + 1)
  }
}

losing_board <- findBingoLoser(bingo_boards, bingo_calls)
sum(losing_board$board[!losing_board$calls]) * losing_board$last_call
