#### Set-Up ####
packets <- readLines("input.txt")

parse_packets <- function(x) {
  y <- x[x != ""] |>
    gsub(pattern = "\\[", replacement = "list(") |>
    gsub(pattern = "\\]", replacement = ")") |>
    toString()
  eval(parse(file = "", n = NULL, text = paste0("list(", y, ")")))
}

split_packets <- parse_packets(packets)
split_packets <- split(split_packets, (seq_along(split_packets) + 1) %/% 2)

#### Part 1 ####
compare_packets <- function(packets, i = 1, sub = FALSE) {
  p1 <- packets[[1]]
  p2 <- packets[[2]]

  if (any(i > lengths(packets))) {
    if (sub && i > length(p1) && i > length(p2)) return(NA) else return(i > length(p1))
  } else if (identical(p1[[i]], p2[[i]])) {
    compare_packets(packets, i = i + 1, sub = sub)
  } else if (is.list(p1[[i]]) || is.list(p2[[i]])) {
    if (is.list(p1[[i]]) && is.list(p2[[i]])) {
      sub_res <- compare_packets(list(p1[[i]], p2[[i]]), sub = TRUE)
    } else if (is.list(p1[[i]])) {
      sub_res <- compare_packets(list(p1[[i]], list(p2[[i]])), sub = TRUE)
    } else if (is.list(p2[[i]])) {
      sub_res <- compare_packets(list(list(p1[[i]]), p2[[i]]), sub = TRUE)
    }
    if (is.na(sub_res)) {
      compare_packets(packets, i = i + 1, sub = sub)
    } else {
      sub_res
    }
  } else {
    j <- 1
    while (identical(p1[[i]][j], p2[[i]][j])) j <- j + 1
    p1[[i]][j] < p2[[i]][j]
  }
}

which(sapply(split_packets, compare_packets)) |> sum()

#### Part 2 ####
split_packets <- parse_packets(packets)
split_packets <- c(
  split_packets,
  "X" = list(list(list(2))),
  "Y" = list(list(list(6)))
)

correct <- rep(FALSE, length(split_packets) - 1)
j <- 1
while (!all(correct)) {
  correct <- rep(FALSE, length(split_packets) - 1)
  print(paste("Round", j))
  j <- j + 1

  for (i in seq(correct)) {
    if (compare_packets(list(split_packets[[i]], split_packets[[i + 1]]))) {
      correct[i] <- TRUE
    } else {
      split_packets[c(i, i + 1)] <- split_packets[c(i + 1, i)]
      names(split_packets)[c(i, i + 1)] <- names(split_packets)[c(i + 1, i)]
    }
  }
}

prod(which(names(split_packets) %in% c("X", "Y")))
