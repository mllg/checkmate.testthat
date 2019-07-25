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
#' @include makeExpectation.R
#' @keywords internal
#' @name expect_testthat
NULL

#' @rdname expect_testthat
#' @export
expect_access = makeExpectationFunction(checkmate::check_access)

#' @rdname expect_testthat
#' @export
expect_array = makeExpectationFunction(checkmate::check_array)

#' @rdname expect_testthat
#' @export
expect_atomic = makeExpectationFunction(checkmate::check_atomic)

#' @rdname expect_testthat
#' @export
expect_atomic_vector = makeExpectationFunction(checkmate::check_atomic_vector)

#' @rdname expect_testthat
#' @export
expect_bit = makeExpectationFunction(checkmate::check_bit)

#' @rdname expect_testthat
#' @export
expect_character = makeExpectationFunction(checkmate::check_character)

#' @rdname expect_testthat
#' @export
expect_choice = makeExpectationFunction(checkmate::check_choice)

#' @rdname expect_testthat
#' @export
expect_class = makeExpectationFunction(checkmate::check_class)

#' @rdname expect_testthat
#' @export
expect_complex = makeExpectationFunction(checkmate::check_complex)

#' @rdname expect_testthat
#' @export
expect_count = makeExpectationFunction(checkmate::check_count)

#' @rdname expect_testthat
#' @export
expect_data_frame = makeExpectationFunction(checkmate::check_data_frame)

#' @rdname expect_testthat
#' @export
expect_data_table = makeExpectationFunction(checkmate::check_data_table)

#' @rdname expect_testthat
#' @export
expect_date = makeExpectationFunction(checkmate::check_date)

#' @rdname expect_testthat
#' @export
expect_directory_exists = makeExpectationFunction(checkmate::check_directory_exists)

#' @rdname expect_testthat
#' @export
expect_disjunct = makeExpectationFunction(checkmate::check_disjunct)

#' @rdname expect_testthat
#' @export
expect_double = makeExpectationFunction(checkmate::check_double)

#' @rdname expect_testthat
#' @export
expect_environment = makeExpectationFunction(checkmate::check_environment)


#' @rdname expect_testthat
#' @export
expect_factor = makeExpectationFunction(checkmate::check_factor)

#' @rdname expect_testthat
#' @export
expect_file_exists = makeExpectationFunction(checkmate::check_file_exists)

#' @rdname expect_testthat
#' @export
expect_flag = makeExpectationFunction(checkmate::check_flag)

#' @rdname expect_testthat
#' @export
expect_formula = makeExpectationFunction(checkmate::check_formula)

#' @rdname expect_testthat
#' @export
expect_function = makeExpectationFunction(checkmate::check_function)

#' @rdname expect_testthat
#' @export
expect_int = makeExpectationFunction(checkmate::check_int)

#' @rdname expect_testthat
#' @export
expect_integer = makeExpectationFunction(checkmate::check_integer)

#' @rdname expect_testthat
#' @export
expect_integerish = makeExpectationFunction(checkmate::check_integerish)

#' @rdname expect_testthat
#' @export
expect_list = makeExpectationFunction(checkmate::check_list)

#' @rdname expect_testthat
#' @export
expect_logical = makeExpectationFunction(checkmate::check_logical)

#' @rdname expect_testthat
#' @export
expect_matrix = makeExpectationFunction(checkmate::check_matrix)

#' @rdname expect_testthat
#' @export
expect_multi_class = makeExpectationFunction(checkmate::check_multi_class)

#' @rdname expect_testthat
#' @export
expect_named = makeExpectationFunction(checkmate::check_named)

#' @rdname expect_testthat
#' @export
expect_names = makeExpectationFunction(checkmate::check_names)

#' @rdname expect_testthat
#' @export
expect_number = makeExpectationFunction(checkmate::check_number)

#' @rdname expect_testthat
#' @export
expect_numeric = makeExpectationFunction(checkmate::check_numeric)

#' @rdname expect_testthat
#' @export
expect_path_for_output = makeExpectationFunction(checkmate::check_path_for_output)

#' @rdname expect_testthat
#' @export
expect_posixct = makeExpectationFunction(checkmate::check_posixct)

#' @rdname expect_testthat
#' @export
expect_r6 = makeExpectationFunction(checkmate::check_r6)

#' @rdname expect_testthat
#' @export
expect_raw = makeExpectationFunction(checkmate::check_raw)

#' @rdname expect_testthat
#' @export
expect_scalar = makeExpectationFunction(checkmate::check_scalar)

#' @rdname expect_testthat
#' @export
expect_scalar_na = makeExpectationFunction(checkmate::check_scalar_na)

#' @rdname expect_testthat
#' @export
expect_set_equal = makeExpectationFunction(checkmate::check_set_equal)

#' @rdname expect_testthat
#' @export
expect_string = makeExpectationFunction(checkmate::check_string)

#' @rdname expect_testthat
#' @export
expect_subset = makeExpectationFunction(checkmate::check_subset)

#' @rdname expect_testthat
#' @export
expect_tibble = makeExpectationFunction(checkmate::check_tibble)
