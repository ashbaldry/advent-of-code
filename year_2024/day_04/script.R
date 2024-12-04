#### Set-Up ####
input <- readLines("input.txt") |> strsplit("") |> do.call(what = rbind)
collapse <- \(...) sapply(list(...), paste, collapse = "")

#### Step 1 ####
input_size <- dim(input)
check_cell <- \(i, j) {
  max_col <- min(3, input_size[2] - j)
  max_row <- min(3, input_size[1] - i)
  max_dim <- min(max_col, max_row)

  strings <- collapse(
    input[i, j + 0:max_col],
    input[i + 0:max_row, j],
    input[cbind(i + 0:max_dim, j + 0:max_dim)],
    input[cbind(i + 0:max_dim, j + max_dim:0)]
  )

  sum(strings %in% c("XMAS", "SAMX"))
}

sapply(seq(input_size[2L]), \(j) sapply(seq(input_size[1L]), check_cell, j = j)) |> sum()

#### Step 2 ####
check_cell_2 <- \(i, j) {
  max_col <- min(2, input_size[2] - j)
  max_row <- min(2, input_size[1] - i)
  max_dim <- min(max_col, max_row)

  strings <- collapse(
    input[cbind(i + 0:max_dim, j + 0:max_dim)],
    input[cbind(i + 0:max_dim, j + max_dim:0)]
  )

  all(strings %in% c("MAS", "SAM"))
}

sapply(seq(input_size[2L]), \(j) sapply(seq(input_size[1L]), check_cell_2, j = j)) |> sum()
