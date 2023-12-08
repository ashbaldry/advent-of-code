options(scipen = 999)
file_name <- "input.txt"
instructions <- readLines(file_name, n = 1L) |> strsplit("") |> unlist()
path_info <- readLines(file_name)[-1:-2]
paths <- data.frame(
  from = substr(path_info, 1, 3),
  L = substr(path_info, 8, 10),
  R = substr(path_info, 13, 15)
)

#### Part 1 ####
calculate_niters <- function(path, x = "AAA") {
  iter <- 0
  inst_i <- 1
  while (!grepl("..Z", x)) {
    x <- path[path$from == x, instructions[inst_i]]
    inst_i <- if (inst_i == length(instructions)) 1 else inst_i + 1
    iter <- iter + 1
  }
  iter
}

calculate_niters(paths)

#### Part 2 ####
n_steps <- sapply(grep("..A", paths$from, value = TRUE), calculate_niters, path = paths)
Reduce(pracma::Lcm, n_steps)
