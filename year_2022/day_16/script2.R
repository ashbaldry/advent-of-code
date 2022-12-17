#### Set-Up ####
instructions <- readLines("example.txt")
valves <- regmatches(instructions, regexpr("\\b[A-Z]{2}\\b", instructions))
valves_to <- regmatches(instructions, gregexpr("\\b[A-Z]{2}\\b", instructions)) |>
  lapply(tail, n = -1) |>
  setNames(valves)
valves_power <- setNames(as.numeric(regmatches(instructions, regexpr("(-|)\\d+", instructions))), valves)

#### Step 1 ####
good_valves <- sort(names(valves_power)[valves_power > 0])
good_valves_comb <- lapply(seq(good_valves), \(x) {
  combn(good_valves, x) |> apply(2, \(x) paste(sort(x), collapse = "")) |> unique()
}) |>
  unlist()
first_opens <- matrix(Inf, length(good_valves_comb), length(valves), dimnames = list(c(good_valves_comb), valves))
max_mem_values <- character(0)

find_potential_power <- function(x = valves[1], i = 1, n = 30, pressure = 0, curr_open = character(0)) {
  pressure <- pressure + sum(valves_power[curr_open])
  if (length(curr_open) > 0 && i > first_opens[paste0(curr_open, collapse = ""), x]) {
    return()
  } else if (i == n) {
    return(pressure)
  }

  mem_name <- paste(paste0(curr_open, collapse = ""), i, x, sep = "_")
  if (mem_name %in% names(max_mem_values)) {
    path_to <- max_mem_values[[mem_name]]
  } else {
    path_to <- valves_to[[x]]
    if (x %in% good_valves && !x %in% curr_open) {
      path_to <- c(x, path_to)
      first_opens[paste0(sort(c(curr_open, x)), collapse = ""), x] <<- i + 1
    }
  }
  path_to <- setNames(path_to, path_to)

  next_paths <- lapply(path_to, \(y) {
    if (x == y) curr_open <- sort(c(curr_open, y))
    find_potential_power(
      y,
      i = i + 1,
      n = n,
      pressure = pressure,
      curr_open = curr_open
    )
  })

  avail_paths <- next_paths[!sapply(next_paths, is.null)] |> unlist()
  if (length(avail_paths)) {
    max_mem_values <<- c(max_mem_values, setNames(names(avail_paths)[which.max(avail_paths)], mem_name))
    max(avail_paths)
  } else {
    NULL
  }
}

find_potential_power()
regmatches(names(max_mem_values), regexpr("\\d+", names(max_mem_values))) |> as.numeric() |> table()
min(first_opens[!is.infinite(first_opens)])

#### Step 2 ####
