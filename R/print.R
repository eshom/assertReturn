#' @rdname assert_return
#' @param x An `asserted_function` object
#' @param ... Passed to [base::print.function()]
#' @param wrapper Logical. If `TRUE`, prints the wrapper
#' instead of the original function
#' @export
print.asserted_function <- function(x, ..., wrapper = FALSE) {
        if (wrapper)
                print.function(x, ...)

        cat("Return value: `",
            paste(class(environment(x)$expected_obj), collapse = ", "),
            "`\n", sep = "")
        print.function(environment(x)$f, ...)
}
