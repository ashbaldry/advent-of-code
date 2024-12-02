#### Set-Up ####
reports <- readLines("input.txt") |> strsplit(" ") |> lapply(as.integer)

#### Step 1 ####
report_diffs <- lapply(reports, diff)
is_valid <- \(x) min(abs(x)) >= 1L && max(abs(x)) <= 3L && length(unique(sign(x))) == 1L
sapply(report_diffs, is_valid) |> sum()

#### Step 2 ####
sapply(reports, \(x) sapply(seq(x), \(y) is_valid(diff(x[-y]))) |> any()) |> sum()

#### Step 2 Cleaner ####
report_diffs_2 <- lapply(report_diffs, diff)
