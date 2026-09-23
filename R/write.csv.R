#' @title Write Comma-Separated Values (CSV) file
#'
#' @description Save summaries of partitioned genetic values to CSV files on disk for further
#' analyses of processing with other software or just for saving (backing up)
#' results.
#'
#' Function \code{\link[utils]{write.csv}} from the \pkg{utils} package works
#' when the object is a \code{\link[base]{data.frame}} or a
#' \code{\link[base]{matrix}}. This is an attempt to make this function generic
#' so that one can define \code{write.csv} methods for other objects.
#'
#' @seealso \code{\link[utils]{write.csv}} help page on the default \code{write.csv}
#' and \code{write.csv2} methods in the \pkg{utils} package;
#' \code{\link[AlphaPart]{summary.AlphaPart}} and \code{\link[AlphaPart]{AlphaPart}}
#' help pages on \code{summaryAlphaPart} and \code{AlphaPart} classes.
#'
#' @param x AlphaPart or summaryAlphaPart, objects returned from
#'   \code{\link[AlphaPart]{AlphaPart}} or \code{\link[AlphaPart]{summary.AlphaPart}}
#' @param file character, file name with or without .csv extension,
#'  that is, both \code{"file"} and \code{"file.csv"} are valid
#' @param traitsAsDir logical, should results be saved within trait folders;
#'   named as \code{file.path(dirname(file), trait, basename(file))};
#'   folders are created if they do not exist.
#' @param csv2 logical, export using \code{\link[utils]{write.csv2}} or
#'   \code{\link[utils]{write.csv}}.
#' @param row.names Logical, export row names as well?
#' @param ... other options passed to \code{\link[utils]{write.csv2}} or
#'   \code{\link[utils]{write.csv}}.
#'
#' @example inst/examples/examples_write.csv.R
#'
#' @return It contains:
#'
#' * \code{write.csv} - see \code{\link[utils]{write.csv}} for details.
#' * \code{write.csv.AlphaPart} - for each trait (list component in \code{x})
#'   a file is saved on disk with name \code{"AlphaPart_trait.csv"},
#'   where the file will hold original data and genetic value partitions.
#'   With \code{traitsAsDir=TRUE} files are saved as \code{"trait/file_trait.csv"}.
#'   File names are printed on screen during the process of export and at the end
#'   invisibly returned.
#' * \code{`write.csv.summaryAlphaPart`} - for each trait (list component in \code{x})
#'   a file partitions named \code{"file_trait.csv"} is saved on disk.
#'   With \code{traitsAsDir=TRUE} files are saved as \code{"trait/file_trait_*.csv".}
#'   File names are printed on screen during the process of export and at the end
#'   invisibly returned.
#'
#' @export
write.csv <- function(...) {
  UseMethod("write.csv")
}

#' @describeIn write.csv Default \code{write.csv} method.
#' @export
write.csv.default <- function(...) {
  utils::write.csv(...)
}

#' @describeIn write.csv Save partitioned genetic values to CSV files on disk on disk
#' results.
#' @export
write.csv.AlphaPart <- function(
  x,
  file,
  traitsAsDir = FALSE,
  csv2 = TRUE,
  row.names = FALSE,
  ...
) {
  # --- Setup ---

  if (length(file) > 1) stop("'file' argument must be of length one")
  if (!inherits(x, "AlphaPart")) stop("'x' must be of a 'AlphaPart' class")
  fileOrig <- sub(pattern = ".csv$", replacement = "", x = file)
  ret <- NULL

  # --- Code ---

  for (i in 1:(length(x) - 1)) {
    # loop over traits
    if (traitsAsDir) {
      dir.create(
        path = file.path(dirname(fileOrig), x$info$lT[i]),
        recursive = TRUE,
        showWarnings = FALSE
      )
      file <- file.path(dirname(fileOrig), x$info$lT[i], basename(fileOrig))
    }
    fileA <- paste(file, "_", x$info$lT[i], ".csv", sep = "")
    ret <- c(ret, fileA)
    cat(fileA, "\n")

    if (csv2) {
      write.csv2(x = x[[i]], file = fileA, row.names = row.names, ...)
    } else {
      write.csv(x = x[[i]], file = fileA, row.names = row.names, ...)
    }
  }

  # --- Return ---

  invisible(ret)
}

#' @describeIn write.csv Save summaries of partitioned genetic values to CSV files on disk
#' @export
write.csv.summaryAlphaPart <- function(
  x,
  file,
  traitsAsDir = FALSE,
  csv2 = TRUE,
  row.names = FALSE,
  ...
) {
  # --- Setup ---

  if (length(file) > 1) stop("'file' argument must be of length one")
  if (!inherits(x, "summaryAlphaPart"))
    stop("'x' must be of a 'summaryAlphaPart' class")
  fileOrig <- sub(pattern = ".csv$", replacement = "", x = file)
  ret <- NULL

  # --- Code ---

  for (i in 1:(length(x) - 1)) {
    # loop over traits
    if (traitsAsDir) {
      dir.create(
        path = file.path(dirname(fileOrig), x$info$lT[i]),
        recursive = TRUE,
        showWarnings = FALSE
      )
      file <- file.path(dirname(fileOrig), x$info$lT[i], basename(fileOrig))
    }
    fileA <- paste(file, x$info$lT[i], ".csv", sep = "_")
    ret <- c(ret, fileA)
    cat(fileA, "\n")

    if (csv2) {
      write.csv2(x = x[[i]], file = fileA, row.names = row.names, ...)
    } else {
      write.csv(x = x[[i]], file = fileA, row.names = row.names, ...)
    }
  }

  # --- Return ---
  invisible(ret)
}
