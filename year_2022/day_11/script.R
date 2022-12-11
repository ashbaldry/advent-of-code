#### Set-Up ####
monkeys <- readLines("input.txt") |>
  sub(pattern = "  If", replacement = "If") |>
  yaml::yaml.load()

extract_number <- \(x) as.numeric(gsub("\\D", "", x))
pass_monkey_func <- \(x) paste0("\\(x) monkey_", extract_number(x) + 1, "$receiveItem(x)")

Monkey <- R6::R6Class(
  classname = "monkey",
  public = list(
    n_checks = 0,
    divisor = numeric(0),
    pass_monkey = NULL,
    fail_monkey = NULL,

    initialize = \(x) {
      private$initial_items <- as.numeric(strsplit(as.character(x[["Starting items"]]), ", ")[[1]])
      private$received_items <- private$initial_items
      private$operation <- eval(parse(text = sub("new = ", "\\\\(old) ", x[["Operation"]])))
      self$divisor <- extract_number(x[["Test"]])
      private$test <- eval(parse(text = paste("\\(x) x %%",  self$divisor, "== 0")))
      self$pass_monkey <- extract_number(x[["If true"]]) + 1
      self$fail_monkey <- extract_number(x[["If false"]]) + 1
      private$pass <- eval(parse(text = pass_monkey_func(x[["If true"]])))
      private$fail <- eval(parse(text = pass_monkey_func(x[["If false"]])))
    },
    checkItems = \(modulus = 0) {
      private$items <- private$received_items
      private$received_items <- numeric(0)
      lapply(private$items, private$checkItem, modulus = modulus)
      self$n_checks <- self$n_checks + length(private$items)
    },
    receiveItem = \(x) private$received_items <- append(private$received_items, x),
    reset = \() {
      private$items <- numeric(0)
      private$received_items <- private$initial_items
      self$n_checks <- 0
    }
  ),
  private = list(
    checkItem = \(old, modulus = 0) {
      if (modulus) {
        new <- private$operation(old) %% modulus
      } else {
        new <- private$operation(old) %/% 3
      }
      if (private$test(new)) private$pass(new) else private$fail(new)
    },
    items = numeric(0),
    initial_items = numeric(0),
    received_items = numeric(0),
    operation = NULL,
    test = NULL,
    pass = NULL,
    fail = NULL
  )
)

for (i in seq(8)) assign(paste0("monkey_", i), Monkey$new(monkeys[[i]]))

#### Part 1 ####
for (round in seq(20)) {
  for (i in seq(8)) {
    monkey <- get(paste0("monkey_", i))
    monkey$checkItems()
    if (round == 20) {
      print(paste("Monkey", i, "inspected items", monkey$n_checks, "times."))
    }
  }
}

#### Part 2 ####
MODULUS <- 1
for (i in seq(8)) {
  monkey <- get(paste0("monkey_", i))
  monkey$reset()
  MODULUS <- MODULUS * monkey$divisor
}

for (round in seq(10000)) {
  for (i in seq(8)) {
    monkey <- get(paste0("monkey_", i))
    monkey$checkItems(modulus = MODULUS)
    if (round == 10000) {
      print(paste("Monkey", i, "inspected items", monkey$n_checks, "times."))
    }
  }
}
