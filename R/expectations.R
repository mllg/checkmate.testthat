# 1: name of x
# 2: name of function to call
# 3: function arguments, as collapsed string
template = '{
    if (missing(%1$s)) {
        stop(sprintf("Argument \'%%s\' is missing", label))
    }
    res = %2$s(%3$s)

    testthat::expect_true(
      object = res,
      info = if (is.null(info)) res else sprintf("%%s\nAdditional info: %%s", res, label),
      label = sprintf("Check on %%s", info)
    )
    invisible(%1$s)
}'

as_expectation = function(check.fun, env = parent.frame()) {
  fun.name = if (!is.character(check.fun)) deparse(substitute(check.fun)) else check.fun
  check.fun = match.fun(check.fun)
  fun.args = formals(args(check.fun))
  x.name = names(fun.args[1L])
  x = NULL
  body = sprintf(template, x.name, fun.name, paste0(names(fun.args), collapse = ", "))

  new.fun = function() TRUE
  formals(new.fun) = c(fun.args, alist(info = NULL, label = checkmate::vname(x)))
  body(new.fun) = parse(text = body)
  environment(new.fun) = env

  return(new.fun)
}

#' @title Expectations for 'testthat'
#'
#' @description
#' Expectations for package \CRANpkg{testthat}.
#' See the respective help page in \CRANpkg{checkmate} (replace `expect_` with `check_`) for a
#' description of the function arguments.
#'
#' @param info [character(1)]\cr
#'  Extra information to be included in the message for the \CRANpkg{testthat} reporter.
#'  See \code{\link[testthat]{expect_that}}.
#' @param label [\code{character(1)}]\cr
#'  Name of the checked object to print in messages. Defaults to
#'  the heuristic implemented in \code{\link{vname}}.
#' @keywords internal
#' @name expect_testthat
NULL

#' @rdname expect_testthat
#' @export
expect_access = as_expectation(checkmate::check_access)

#' @rdname expect_testthat
#' @export
expect_array = as_expectation(checkmate::check_array)

#' @rdname expect_testthat
#' @export
expect_atomic = as_expectation(checkmate::check_atomic)

#' @rdname expect_testthat
#' @export
expect_atomic_vector = as_expectation(checkmate::check_atomic_vector)

#' @rdname expect_testthat
#' @export
expect_bit = as_expectation(checkmate::check_bit)

#' @rdname expect_testthat
#' @export
expect_character = as_expectation(checkmate::check_character)

#' @rdname expect_testthat
#' @export
expect_choice = as_expectation(checkmate::check_choice)

#' @rdname expect_testthat
#' @export
expect_class = as_expectation(checkmate::check_class)

#' @rdname expect_testthat
#' @export
expect_complex = as_expectation(checkmate::check_complex)

#' @rdname expect_testthat
#' @export
expect_count = as_expectation(checkmate::check_count)

#' @rdname expect_testthat
#' @export
expect_data_frame = as_expectation(checkmate::check_data_frame)

#' @rdname expect_testthat
#' @export
expect_data_table = as_expectation(checkmate::check_data_table)

#' @rdname expect_testthat
#' @export
expect_date = as_expectation(checkmate::check_date)

#' @rdname expect_testthat
#' @export
expect_directory_exists = as_expectation(checkmate::check_directory_exists)

#' @rdname expect_testthat
#' @export
expect_disjunct = as_expectation(checkmate::check_disjunct)

#' @rdname expect_testthat
#' @export
expect_double = as_expectation(checkmate::check_double)

#' @rdname expect_testthat
#' @export
expect_environment = as_expectation(checkmate::check_environment)


#' @rdname expect_testthat
#' @export
expect_factor = as_expectation(checkmate::check_factor)

#' @rdname expect_testthat
#' @export
expect_file_exists = as_expectation(checkmate::check_file_exists)

#' @rdname expect_testthat
#' @export
expect_flag = as_expectation(checkmate::check_flag)

#' @rdname expect_testthat
#' @export
expect_formula = as_expectation(checkmate::check_formula)

#' @rdname expect_testthat
#' @export
expect_function = as_expectation(checkmate::check_function)

#' @rdname expect_testthat
#' @export
expect_int = as_expectation(checkmate::check_int)

#' @rdname expect_testthat
#' @export
expect_integer = as_expectation(checkmate::check_integer)

#' @rdname expect_testthat
#' @export
expect_integerish = as_expectation(checkmate::check_integerish)

#' @rdname expect_testthat
#' @export
expect_list = as_expectation(checkmate::check_list)

#' @rdname expect_testthat
#' @export
expect_logical = as_expectation(checkmate::check_logical)

#' @rdname expect_testthat
#' @export
expect_matrix = as_expectation(checkmate::check_matrix)

#' @rdname expect_testthat
#' @export
expect_multi_class = as_expectation(checkmate::check_multi_class)

#' @rdname expect_testthat
#' @export
expect_named = as_expectation(checkmate::check_named)

#' @rdname expect_testthat
#' @export
expect_names = as_expectation(checkmate::check_names)

#' @rdname expect_testthat
#' @export
expect_number = as_expectation(checkmate::check_number)

#' @rdname expect_testthat
#' @export
expect_numeric = as_expectation(checkmate::check_numeric)

#' @rdname expect_testthat
#' @export
expect_path_for_output = as_expectation(checkmate::check_path_for_output)

#' @rdname expect_testthat
#' @export
expect_posixct = as_expectation(checkmate::check_posixct)

#' @rdname expect_testthat
#' @export
expect_r6 = as_expectation(checkmate::check_r6)

#' @rdname expect_testthat
#' @export
expect_raw = as_expectation(checkmate::check_raw)

#' @rdname expect_testthat
#' @export
expect_scalar = as_expectation(checkmate::check_scalar)

#' @rdname expect_testthat
#' @export
expect_scalar_na = as_expectation(checkmate::check_scalar_na)

#' @rdname expect_testthat
#' @export
expect_set_equal = as_expectation(checkmate::check_set_equal)

#' @rdname expect_testthat
#' @export
expect_string = as_expectation(checkmate::check_string)

#' @rdname expect_testthat
#' @export
expect_subset = as_expectation(checkmate::check_subset)

#' @rdname expect_testthat
#' @export
expect_tibble = as_expectation(checkmate::check_tibble)
