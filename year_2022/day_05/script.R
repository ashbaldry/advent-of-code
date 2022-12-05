#### Set-Up ####
blocks <- read.fwf("input.txt", widths = rep(4, 9), n = 8) |>
  lapply(\(x) rev(gsub("\\W", "", x))) |>
  lapply(\(x) x[!x %in% c("", NA)])

operations <- read.table(
  "input.txt", sep = " ", skip = 10,
  col.names = c("x1", "move", "x2", "from", "x3", "to")
)

#### Part 1 ####
for (i in seq(nrow(operations))) {
  blocks[[operations$to[i]]] <- c(
    blocks[[operations$to[i]]],
    rev(tail(blocks[[operations$from[i]]], operations$move[i]))
  )
  blocks[[operations$from[i]]] <- head(blocks[[operations$from[i]]], -operations$move[i])
}
vapply(blocks, tail, character(1L), n = 1) |> paste(collapse = "")

#### Part 2 ####
for (i in seq(nrow(operations))) {
  blocks[[operations$to[i]]] <- c(
    blocks[[operations$to[i]]],
    tail(blocks[[operations$from[i]]], operations$move[i])
  )
  blocks[[operations$from[i]]] <- head(blocks[[operations$from[i]]], -operations$move[i])
}
vapply(blocks, tail, character(1L), n = 1) |> paste(collapse = "")
