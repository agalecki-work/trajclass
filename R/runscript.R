## -> runScript function
#' Execute scripts from trajclass package
#'
#' Executes scripts from trajclass package. If called without arguments, it prints a list of available scripts.
#'
#' @export
#' @param script character string containing the name of the script to be executed. By default, \code{NA}.
#' @param package character string containing the package name. By default, \code{"trajclass"}.
#' @param subdir subdirectory containing scripts. By default, \code{"scripts"}.
#' @param echo logical. If \code{TRUE}, the script is executed with output printed. Used by \code{source()}. By default, \code{TRUE}.
#' @return The script is executed, and results are printed. If \code{script} is \code{NA}, a list of available scripts is printed.
#' @examples
#' runScript()
runScript <- function(script = NA, package = "trajclass", 
                      subdir = "scripts", echo = TRUE,
                      max.deparse = 10000
  ) {
  pkgDir <- find.package(package)
  scriptsDir <- system.file(subdir, package = package)
  scriptsList <- list.files(scriptsDir, pattern = "[[:alnum:]][.][R]$")
  scriptFile <- file.path(scriptsDir, script)
  if (!(script %in% scriptsList)) {
    if (is.na(script)) {
      errFun <- message
      errMsg <- paste("Scripts in ", scriptsDir, " are: \n", paste("\"", scriptsList, 
                                                                collapse = "\", \n", sep = ""), "\"")
    } else {
      errFun <- stop
      errMsg <- paste("Script", script, "does not exist. ")
    }
    errFun(errMsg)
  }
    if (!is.na(script)) source(scriptFile, echo = echo, max.deparse.length = max_deparse) else invisible()
}
