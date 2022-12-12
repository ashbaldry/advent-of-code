#### Set-Up ####
elevation <- readLines("input.txt") |> strsplit("") |> do.call(what = rbind)
elevation_num <- matrix(match(elevation, letters), ncol = ncol(elevation))

start <- which(elevation == "S", arr.ind = TRUE)
elevation_num[start] <- 1
end <- which(elevation == "E", arr.ind = TRUE)
elevation_num[end] <- 26

new_pos <- matrix(c(0, 0, 1, -1, 1, -1, 0, 0), ncol = 2)

find_shortest_journey <- function(elevation_num, curr_route = start, finish_pos = end, all_routes = curr_route, counter = 1, p2 = FALSE) {
  new_routes <- apply(curr_route, 1, simplify = FALSE, \(x) {
    curr_pos <- matrix(x, ncol = 2)
    curr_elevation <- elevation_num[curr_pos]
    move_positions <- matrix(rep(curr_pos, each = 4), ncol = 2) + new_pos
    move_positions <- move_positions[
      move_positions[, 1] > 0 & move_positions[, 2] > 0 & move_positions[, 1] <= 41 & move_positions[, 2] <= 95,
      ,
      drop = FALSE
    ]
    if (p2) {
      move_positions[elevation_num[move_positions] - curr_elevation >= -1, , drop = FALSE]
    } else {
      move_positions[elevation_num[move_positions] - curr_elevation <= 1, , drop = FALSE]
    }
  })

  move_positions <- do.call(rbind, new_routes) |> unique()
  move_positions <- move_positions[!apply(move_positions, 1, toString) %in% apply(all_routes, 1, toString), , drop = FALSE]
  all_routes <- rbind(curr_route, move_positions) |> unique()
  print(paste(counter, ":", p2, min(elevation_num[move_positions])))

  if (!p2 && toString(finish_pos) %in% apply(move_positions, 1, toString)) {
    counter
  } else if (p2 && isTRUE(min(elevation_num[move_positions]) == 1)) {
    counter
  }  else {
    find_shortest_journey(elevation_num, move_positions, finish_pos, all_routes = all_routes, counter = counter + 1, p2 = p2)
  }
}

find_shortest_journey(elevation_num, curr_route = start, finish_pos = end)
find_shortest_journey(elevation_num, curr_route = end, p2 = TRUE)
