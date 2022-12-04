#### Set-Up ####
assignments <- read.csv("input.txt", header = FALSE, col.names = c("A", "B"))
assignments$A1 <- with(assignments, as.numeric(sub("-.*", "", A)))
assignments$A2 <- with(assignments, as.numeric(sub(".*-", "", A)))
assignments$B1 <- with(assignments, as.numeric(sub("-.*", "", B)))
assignments$B2 <- with(assignments, as.numeric(sub(".*-", "", B)))

#### Part 1 ####
with(assignments, sum((A1 >= B1 & A2 <= B2) | (B1 >= A1 & B2 <= A2)))

#### Part 2 ####
with(assignments, sum((A1 <= B2 & A2 >= B2) | (B1 <= A2 & B2 >= A2)))
