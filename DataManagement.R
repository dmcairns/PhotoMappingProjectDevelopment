###############################################
#     Data Management Tools                   #
###############################################


byRef <- function(..., envir=parent.frame(), inherits=TRUE) {
  ###############################
  # Function to create pass by  #
  # reference variables. (cool!)#
  ###############################
  
  #originally posted by codeola at https://stackoverflow.com/questions/2603184/can-you-pass-by-reference-in-r
  
  cl <- match.call(expand.dots = TRUE)
  cl[c(1, match(c("envir", "inherits"), names(cl), 0L))] <- NULL
  for (x in as.list(cl)) {
    s <- substitute(x)
    sx <- do.call(substitute, list(s), envir=envir)
    dx <- deparse(sx)
    expr <- substitute(assign(dx, s, envir=parent.frame(), inherits=inherits))
    do.call(on.exit, list(expr, add=TRUE), envir=envir)
  }
}