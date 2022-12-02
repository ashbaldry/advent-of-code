# Set-Up
rps <- read.table("input.txt", col.names = c("elf_play", "my_play"))
rps$elf_rps <- with(rps, match(elf_play, LETTERS[1:3]))
rps$my_rps <- with(rps, match(my_play, LETTERS[24:26]))

# Part 1
rps$my_outcome <- with(rps, (my_rps %% 3) - (elf_rps %% 3))
rps$my_outcome <- with(rps, ifelse(my_outcome == 2, -1, ifelse(my_outcome == -2, 1, my_outcome)))
rps$my_score <- with(rps, my_rps + 3 * (1 + my_outcome))
sum(rps$my_score)

# Part 2
rps$my_rps_2 <- with(rps, ifelse(
  my_play == "X",
  ifelse(elf_rps == 1, 3, elf_rps - 1),
  ifelse(
    my_play == "Y",
    elf_rps,
    ifelse(elf_rps == 3, 1, elf_rps + 1)
  )
))
rps$my_score_2 <- with(rps, my_rps_2 + 3 * (my_rps - 1))
sum(rps$my_score_2)

# Part 1 - Cleaned
rps$my_outcome <- with(rps, (1 + my_rps - elf_rps) %% 3)
rps$my_score <- with(rps, my_rps + 3 * my_outcome)
sum(rps$my_score)

# Part 2 - Cleaned
rps$my_rps_2 <- with(rps, (elf_rps + my_rps - 2 - 1) %% 3 + 1)
rps$my_score_2 <- with(rps, my_rps_2 + 3 * (my_rps - 1))
sum(rps$my_score_2)
