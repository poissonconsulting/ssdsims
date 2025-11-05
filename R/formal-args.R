formal_args_simulate_data <- function(x, ...) {
  UseMethod("formal_args_simulate_data")
}

formal_args_simulate_data.data.frame <- function(x, ...) {
  methods::formalArgs(utils::argsAnywhere("ssd_simulate_data.data.frame"))
}

formal_args_simulate_data.fitdists <- function(x, ...) {
  methods::formalArgs(utils::argsAnywhere("ssd_simulate_data.fitdists"))
}

formal_args_simulate_data.tmbfit <- function(x, ...) {
  methods::formalArgs(utils::argsAnywhere("ssd_simulate_data.tmbfit"))
}

formal_args_simulate_data.character <- function(x, ...) {
  methods::formalArgs(utils::argsAnywhere("ssd_simulate_data.character"))
}

formal_args_simulate_data.function <- function(x, ...) {
  methods::formalArgs(utils::argsAnywhere("ssd_simulate_data.function"))
}
