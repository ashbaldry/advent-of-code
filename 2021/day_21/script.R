start_positions <- read.delim(
  "2021/day_21/input.txt", header = FALSE, sep = " ",
  col.names = c("x", "player", "y", "z", "start")
)

# Part 1
addScore <- function(p1, p2, s1 = 0, s2 = 0, turn = 1, i = 1, cnt = 3) {
  code <- "p1 <- (p1 + sum(i:(i + 2) %% 10)) %% 10; if (p1 == 0) p1 <- 10; s1 <- s1 + p1"
  if (turn == 2) code <- gsub("(p|s)1", "\\12", code)
  eval(parse(text = code))

  if (s1 >= 1000 || s2 >= 1000) {
    c(s1, s2)[c(s1, s2) < 1000] * cnt
  } else {
    addScore(p1, p2, s1, s2, 3 - turn, (i + 3) %/% 101 + (i + 3) %% 101, cnt + 3)
  }
}

addScore(start_positions$start[1], start_positions$start[2])

# Part 2
pot_scores <- expand.grid(1:3, 1:3, 1:3)
pot_values <- table(rowSums(pot_scores))

addScore2 <- function(p1, p2, s1 = 0, s2 = 0, turn = 1, cnt = 1) {
  rowSums(
    sapply(3:9, \(x) {
      if (turn == 1) {
        p1 <- (p1 + x) %% 10; if (p1 == 0) p1 <- 10; s1 <- s1 + p1
      } else {
        p2 <- (p2 + x) %% 10; if (p2 == 0) p2 <- 10; s2 <- s2 + p2
      }

      if (s1 >= 21 || s2 >= 21) {
        cnt * pot_values[[as.character(x)]] * as.numeric(turn == c(1, 2))
      } else  {
        addScore2(p1, p2, s1, s2, 3 - turn, cnt * pot_values[[as.character(x)]])
      }
    })
  )
}

addScore2(8, 2)
