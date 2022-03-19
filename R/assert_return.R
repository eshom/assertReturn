#' `assert_return` returns a function that checks the return value
#' every time it runs.
#' This can be useful for debugging, or for general safety when you
#' want to make sure a function returns a specific kind of object.
#'
#' The functions generates an `asserted_function` object, which inherits
#' from the regular `function` class. The original function is bound
#' to the environment of returned `asserted_function`. So to access the
#' original function you could use `[base::environment()]`. To pass
#' anonymous functions to `%assert%`, you need to surround `func` in round
#' brackets (see examples).
#' @title Assert Function Return Value
#' @param func Function to wrap
#' @param expected_obj The expected return value.
#' Can be an expression evaluated (lazily) to an R object
#' @return A function of class "asserted_function"
#'
#' @author Erez Shomron
#'
#' @examples
#' f <- (function(x, char = FALSE) {
#'         if (char)
#'                 return(as.character(x ^ 2 - x))
#'
#'         x ^ 2 - x
#' }) %assert% numeric()
#'
#' f(1:10)
#' \dontrun{
#' f(1:10, char = TRUE)
#' #Error in f(1:10, char = TRUE) :
#' #  Return value is `character`. Expecting `numeric`
#' }
#'
#' sum2 <- assert_return(sum, integer())
#' sum2(1:10)
#' \dontrun{
#' sum2(c(0.5, 1, 1.5))
#' #Error in sum2(c(0.5, 1, 1.5)) :
#' #  Return value is `numeric`. Expecting `integer`
#' }
#'
#' print(f)
#' print(sum2)
#' @export
assert_return <- function(func, expected_obj) {
        ## Note the new enclosing environment for the function
        ## So to access original function run `environment(<func name>)$f`
        f <- match.fun(func)

        func_out <- function(...) {
                ret <- f(...)

                ## Informative error message tells you return value classes
                ## versus the expected object classes
                if (!inherits(expected_obj, class(ret))) {
                        stop("Return value is `",
                             paste(class(ret), collapse = ", "),
                             "`. Expecting `",
                             paste(class(expected_obj), collapse = ", "),
                             "`")
                }

                ret
        }

        ## Class used for the print method and possibly other methods
        class(func_out) <- c("asserted_function", class(func_out))
        func_out
}

#' @rdname assert_return
#' @export
`%assert%` <- assert_return
