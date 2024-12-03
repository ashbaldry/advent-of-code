#### Set-Up ####
input <- paste0(readLines("input.txt"), collapse = "")

#### Step 1 ####
matches <- regmatches(input, gregexec("mul\\((\\d+),(\\d+)\\)", input))[[1L]]
sum(as.numeric(matches[2, ]) * as.numeric(matches[3, ]))

#### Step 2 ####
split_input <- strsplit(input, "don't()", fixed = TRUE) |> unlist()
input2 <- sub(".*?(do\\(\\)|$)", "", split_input)
input2[1] <- split_input[1]
input2 <- paste(input2, collapse = "")

matches <- regmatches(input2, gregexec("mul\\((\\d+),(\\d+)\\)", input2))[[1L]]
sum(as.numeric(matches[2, ]) * as.numeric(matches[3, ]))

input3 <- gsub("don't\\(\\).*?(do\\(\\))", "\\1", input)
matches <- regmatches(input3, gregexec("mul\\((\\d+),(\\d+)\\)", input3))[[1L]]
sum(as.numeric(matches[2, ]) * as.numeric(matches[3, ])) == 100189366
