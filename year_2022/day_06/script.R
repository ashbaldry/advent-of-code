#### Set-Up ####
data_stream <- strsplit(readLines("input.txt"), "")[[1]]

#### Part 1 ####
which(zoo::rollapply(data_stream, 4, \(x) length(unique(x)) == 4))[1] + 3

#### Part 2 ####
which(zoo::rollapply(data_stream, 14, \(x) length(unique(x)) == 14))[1] + 13
