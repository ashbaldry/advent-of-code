#### Set-Up ####
instructions <- readLines("example.txt")
valves <- regmatches(instructions, regexpr("\\b[A-Z]{2}\\b", instructions))
valves_to <- regmatches(instructions, gregexpr("\\b[A-Z]{2}\\b", instructions)) |>
  lapply(tail, n = -1) |>
  setNames(valves)
valves_power <- setNames(as.numeric(regmatches(instructions, regexpr("(-|)\\d+", instructions))), valves)
valves_curr_power <- setNames(rep(0, length(valves)), valves)

#### Step 1 ####
max_mem_values <- character(0)

good_valves <- sort(names(valves_power)[valves_power > 0])
good_valves_comb <- lapply(seq(good_valves), \(x) {
  combn(good_valves, x) |> apply(2, \(x) paste(sort(x), collapse = "")) |> unique()
}) |>
  unlist()
first_opens <- matrix(Inf, length(good_valves_comb) + 1, length(valves), dimnames = list(c("temp", good_valves_comb), valves))
best_route <- matrix(NA_character_, length(good_valves_comb) + 1, length(valves), dimnames = list(c("temp", good_valves_comb), valves))

find_potential_power <- function(x = valves[1], i = 1, n = 30, pressure = 0, curr_open = character(0)) {
  pressure <- pressure + sum(valves_power[curr_open])
  if (i == n) return(pressure)
  if (identical(curr_open, good_valves)) {
    return(find_potential_power(values[1], i = i + 1, n = n, curr_open = curr_open, pressure = pressure))
  }

  mem_open <- paste0(curr_open, collapse = "")
  if (identical(mem_open, "")) mem_open <- "temp"
  if (i > (first_opens[mem_open, x] + 2)) {
    return(NA_real_)
  }

  best_valve <- best_route[mem_open, x]
  if (!is.na(best_valve)) {
    if (best_valve == x) curr_open <- sort(union(x, curr_open))
    return(find_potential_power(best_valve, i = i + 1, n = n, curr_open = curr_open, pressure = pressure))
  }

  move_vals <- valves_to[[x]]
  if (valves_power[x] > 0 && !x %in% curr_open) move_vals <- c(x, move_vals)

  end_power <- vapply(
    move_vals,
    \(y) {
      if (y == x) curr_open <- sort(union(curr_open, x))
      find_potential_power(y, i = i + 1, n = n, curr_open = curr_open, pressure = pressure)
    },
    numeric(1L)
  )

  if (any(!is.na(end_power)) && i < first_opens[mem_open, x]) {
    print(paste(x, i, mem_open))
    first_opens[mem_open, x] <<- i
    best_route[mem_open, x] <<- move_vals[which.max(end_power)]
    max(end_power, na.rm = TRUE)
  } else {
    NA_real_
  }
}

find_potential_power()
max_mem_values[grep("_3_", names(max_mem_values))]

#### Step 2 ####
