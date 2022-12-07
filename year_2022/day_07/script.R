#### Set-Up ####
commands <- read.table("input.txt", sep = " ", fill = TRUE, header = FALSE)

#### Part 1 ####
change_dir <- function(files, dir, curr_dir = character(0)) {
  if (dir == "..") head(curr_dir, -1) else if (dir == "/") character(0) else c(curr_dir, dir)
}

follow_command <- function(commands, files = list(), curr_dir = character(0)) {
  command <- commands[1, ]

  if (command$V1 == "$") {
    if (command$V2 == "cd") {
      curr_dir <- change_dir(files, command$V3, curr_dir)
    }
  } else if (command$V1 == "dir") {
    new_folder <- setNames(list(list()), command$V2)
    if (length(curr_dir) > 0) {
      files[[curr_dir]] <- append(files[[curr_dir]], new_folder)
    } else {
      files <- append(files, new_folder)
    }
  } else {
    new_file <- setNames(list(as.numeric(command$V1)), command$V2)
    if (length(curr_dir) > 0) {
      files[[curr_dir]] <- append(files[[curr_dir]], new_file)
    } else {
      files <- append(files, new_file)
    }
  }

  if (nrow(commands) == 1) {
    files
  } else {
    follow_command(commands[-1, ], files = files, curr_dir = curr_dir)
  }
}

files <- follow_command(commands)

get_dir_size <- function(files) {
  dirs <- sapply(files, is.list)
  if (any(dirs)) {
    c(
      lapply(files[dirs], \(x) sum(unlist(files, recursive = TRUE))),
      lapply(files[dirs], get_dir_size)
    )
  } else {
    sum(unlist(files, recursive = TRUE))
  }
}

dir_sizes <- get_dir_size(files)
dir_size_flat <- unlist(dir_sizes, recursive = TRUE)
sum(dir_size_flat[dir_size_flat <= 100000])

#### Part 2 ####
min(dir_size_flat[dir_size_flat >= 5174025])
