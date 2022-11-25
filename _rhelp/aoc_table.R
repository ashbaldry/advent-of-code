library(reactable)

#' @param year_yaml_files Character vector of paths to the year yaml files
#' @param sel_year If `NULL` then covering all years. If defined, then the selected
#' year will be included as the second column name
create_aoc_star_table <- function(year_yaml_files, sel_year = NULL) {
  year_info <- lapply(year_yaml_files, read_year_yaml_index, sel_year = sel_year)
  aoc_completion <- Reduce(function(x, y) merge(x, y, by = "Day", all = TRUE), year_info)

  reactable::reactable(
    aoc_completion,
    columns = list(
      Day = reactable::colDef(
        width = 80,
        cell = \(x, i) x
      )
    ),
    defaultColDef = reactable::colDef(
      cell = \(x, i, year) style_star_column(x, i, year, sel_year)
    ),
    pagination = FALSE,
    theme = reactable::reactableTheme(
      backgroundColor = "inherit"
    )
  )
}

create_aoc_year_table <- function(year_yaml_file, year_results_file, sel_year) {
  aoc_completion <- read_year_yaml_index(year_yaml_file, "Complete")
  results <- read.delim(
    "results.txt", skip = 1, sep = "",
    col.names = c("Day", "Time_1", "Rank_1", "Score_1", "Time_2", "Rank_2", "Score_2")
  )
  results$Day <- sprintf("%02d", results$Day)
  results$Rank_1 <- scales::comma(results$Rank_1)
  results$Rank_2 <- scales::comma(results$Rank_2)

  aoc_completion <- merge(aoc_completion, results, by = "Day", all.x = TRUE)

  reactable::reactable(
    aoc_completion,
    columns = list(
      Day = reactable::colDef(
        width = 80
      ),
      Complete = reactable::colDef(
        cell = \(x, i, year) style_star_column(x, i, year, sel_year)
      ),
      Time_1 = reactable::colDef(
        name = "Time"
      ),
      Rank_1 = reactable::colDef(
        name = "Rank"
      ),
      Score_1 = reactable::colDef(
        name = "Score"
      ),
      Time_2 = reactable::colDef(
        name = "Time"
      ),
      Rank_2 = reactable::colDef(
        name = "Rank"
      ),
      Score_2 = reactable::colDef(
        name = "Score"
      )
    ),
    defaultColDef = reactable::colDef(
      maxWidth = 120,
      align = "right"
    ),
    columnGroups = list(
      reactable::colGroup(
        name = "Part 1",
        columns = c("Time_1", "Rank_1", "Score_1")
      ),
      reactable::colGroup(
        name = "Part 2",
        columns = c("Time_2", "Rank_2", "Score_2")
      )
    ),
    pagination = FALSE,
    theme = reactable::reactableTheme(
      backgroundColor = "inherit"
    )
  )
}

read_year_yaml_index <- function(year_yaml_file, sel_year = NULL) {
  year_yaml <- yaml::read_yaml(year_yaml_file)
  if (is.null(sel_year)) {
    year <- sub("year_", "", dirname(year_yaml_file))
  } else {
    year <- sel_year
  }

  completion <- do.call(
    rbind,
    lapply(year_yaml, \(y) {
      data.frame(
        day = y$title,
        complete = if ("complete" %in% names(y)) y$complete else 0
      )
    })
  )

  names(completion) <- c("Day", year)
  completion
}

style_star_column <- function(x, i, year, sel_year = NULL) {
  if (is.na(x)) {
    ""
  } else {
    if (x > 0) {
      if (is.null(sel_year)) {
        day_solution <- file.path(sprintf("year_%s", year), sprintf("day_%02d", i))
      } else {
        day_solution <- sprintf("day_%02d", i)
      }

      htmltools::a(
        href = day_solution,
        title = paste("Solution to day", i, "for", year),
        htmltools::span(class = "stars", paste(rep("*", x), collapse = "")),
        paste(rep("*", 2 - x), collapse = "")
      )
    } else {
      "**"
    }
  }
}
