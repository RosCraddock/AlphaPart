#' @title Change given unknown value to NA and vice versa.
#' @description Unknown or missing values (\code{NA} in \code{R}) can be represented in various ways (as 0, 999, etc.) in different programs. \code{isUnknown}, \code{unknownToNA}, and \code{NAToUnknown} can help to change unknown values to \code{NA} and vice versa.
#' @name UnknownFuns
#' @usage isUnknown(x, unknown=NA, \dots)
#' @usage unknownToNA(x, unknown, warning=FALSE, \dots)
#' @usage NAToUnknown(x, unknown, force=FALSE, call.=FALSE, \dots)
#' @aliases isUnknown
#' @aliases isUnknown.default
#' @aliases isUnknown.POSIXlt
#' @aliases isUnknown.list
#' @aliases isUnknown.data.frame
#' @aliases isUnknown.matrix
#' @aliases unknownToNA
#' @aliases unknownToNA.default
#' @aliases unknownToNA.factor
#' @aliases unknownToNA.list
#' @aliases unknownToNA.data.frame
#' @aliases NAToUnknown
#' @aliases NAToUnknown.default
#' @aliases NAToUnknown.factor
#' @aliases NAToUnknown.list
#' @aliases NAToUnknown.data.frame
#' @param x generic, object with unknown value(s)
#' @param unknown generic, value used instead of \code{NA}
#' @param warning logical, issue warning if \code{x} already has \code{NA}
#' @param force logical, force to apply already existing value in \code{x}
#' @param ... arguments pased to other methods (as.character for POSIXlt in case of isUnknown)
#' @param call. logical, look in \code{\link{warning}}
#' @seealso
#' \code{\link[AlphaPart]{AlphaPart}}
#'
#' @author Gregor Gorjanc
#'
#' @rdname UnknownFuns
#' @export
isUnknown <- function(x, unknown = NA, ...) UseMethod("isUnknown")

#' @rdname UnknownFuns
#' @method isUnknown default
#' @usage \method{isUnknown}{default}(x, unknown, ...)
#' @export
isUnknown.default <- function(x, unknown = NA, ...) {
  if (is.list(unknown)) unknown <- unlist(unknown)
  ret <- x %in% unknown
  if (any(is.na(unknown))) ret <- ret | is.na(x)
  ret
}

#' @rdname UnknownFuns
#' @method isUnknown POSIXlt
#' @usage \method{isUnknown}{POSIXlt}(x, unknown, ...)
#' @export
isUnknown.POSIXlt <- function(x, unknown = NA, ...) {
  ## FIXME: codetools say
  ## isUnknown.POSIXlt: wrong number of arguments to as.character
  if (is.list(unknown) && !inherits(x = unknown, what = "POSIXlt")) {
    unknown <- lapply(unknown, FUN = as.character, ...)
  } else {
    unknown <- as.character(x = unknown, ...)
  }

  if (is.list(x) && !inherits(x = x, what = "POSIXlt")) {
    x <- lapply(x, FUN = as.character, ...)
  } else {
    x <- as.character(x = x, ...)
  }

  isUnknown.default(x = as.character(x), unknown = as.character(unknown))
}

#' @rdname UnknownFuns
#' @method isUnknown list
#' @usage \method{isUnknown}{list}(x, unknown, ...)
#' @export
isUnknown.list <- function(x, unknown = NA, ...) {
  unknown <- .unknownList(x = x, unknown = unknown)
  x <- mapply(
    FUN = "isUnknown",
    x = x,
    unknown = unknown,
    ...,
    SIMPLIFY = FALSE
  )
  x
}

#' @rdname UnknownFuns
#' @method isUnknown data.frame
#' @usage \method{isUnknown}{data.frame}(x, unknown, ...)
#' @export
isUnknown.data.frame <- function(x, unknown = NA, ...) {
  x[] <- isUnknown.list(x, unknown = unknown, ...)
  x
}

#' @rdname UnknownFuns
#' @method isUnknown matrix
#' @usage \method{isUnknown}{matrix}(x, unknown, ...)
#' @export
isUnknown.matrix <- function(x, unknown = NA, ...)
  apply(
    X = x,
    MARGIN = ifelse(ncol(x) > nrow(x), 1, 2),
    FUN = isUnknown,
    unknown = unknown
  )

#' @rdname UnknownFuns
#' @export
unknownToNA <- function(x, unknown, warning = FALSE, ...)
  UseMethod("unknownToNA")

#' @rdname UnknownFuns
#' @method unknownToNA default
#' @usage \method{unknownToNA}{default}(x, unknown, warning, ...)
#' @export
unknownToNA.default <- function(x, unknown, warning = FALSE, ...) {
  if (warning) {
    if (any(is.na(x))) warning("'x' already has NA")
  }
  is.na(x) <- isUnknown(x = x, unknown = unknown)
  x
}

#' @rdname UnknownFuns
#' @method unknownToNA factor
#' @usage \method{unknownToNA}{factor}(x, unknown, warning, ...)
#' @export
unknownToNA.factor <- function(x, unknown, warning = FALSE, ...) {
  ## could put this func into default method, but I need unlisted unknown
  ## for levels handling
  if (warning) {
    if (any(is.na(x))) warning("'x' already has NA")
  }
  if (is.list(unknown)) unknown <- unlist(unknown)
  ## Levels handling - read help page on this
  levs <- levels(x)
  levs <- levs[!(levs %in% unknown)]
  factor(x, levels = levs)
}

#' @rdname UnknownFuns
#' @method unknownToNA list
#' @usage \method{unknownToNA}{list}(x, unknown, warning, ...)
#' @export
unknownToNA.list <- function(x, unknown, warning = FALSE, ...) {
  unknown <- .unknownList(x = x, unknown = unknown)
  x <- mapply(
    FUN = "unknownToNA",
    x = x,
    unknown = unknown,
    warning = warning,
    SIMPLIFY = FALSE
  )
  return(x)
}

#' @rdname UnknownFuns
#' @method unknownToNA data.frame
#' @usage \method{unknownToNA}{data.frame}(x, unknown, warning, ...)
#' @export
unknownToNA.data.frame <- function(x, unknown, warning = FALSE, ...) {
  x[] <- unknownToNA.list(x = x, unknown = unknown, warning = warning)
  x
}

#' @rdname UnknownFuns
#' @export
NAToUnknown <- function(x, unknown, force = FALSE, call. = FALSE, ...)
  UseMethod("NAToUnknown")

#' @method NAToUnknown default
#' @usage \method{NAToUnknown}{default}(x, unknown, force, call., ...)
#' @export
#' @rdname UnknownFuns
NAToUnknown.default <- function(x, unknown, force = FALSE, call. = FALSE, ...) {
  if (length(as.character(unknown)) != 1)
    # as.character allows also POSIXlt
    stop("'unknown' must be a single value")
  if (any(isUnknown(x, unknown = unknown)) && !force)
    stop(sprintf("'x' already has value %s", dQuote(unknown)))
  classX <- class(x)[1]
  classUnk <- class(unknown)[1]
  if (classX != classUnk) {
    tmp <- c("integer", "numeric")
    if (!(classX %in% tmp && classUnk %in% tmp)) {
      warning(
        sprintf(
          "'unknown' should be %s for %s 'x' - will try to coerce",
          dQuote(classX),
          dQuote(classX)
        ),
        call. = call.
      )
    }
    unknown <- do.call(paste("as.", classX, sep = ""), args = list(unknown))
  }
  x[is.na(x)] <- unknown
  x
}

#' @rdname UnknownFuns
#' @method NAToUnknown factor
#' @usage \method{NAToUnknown}{factor}(x, unknown, force, call., ...)
#' @export
NAToUnknown.factor <- function(x, unknown, force = FALSE, call. = FALSE, ...) {
  if (length(unknown) != 1) stop("'unknown' must be a single value")
  if (any(isUnknown(x, unknown = unknown))) {
    if (!force) stop(sprintf("'x' already has level %s", dQuote(unknown)))
  } else {
    mapLevels(x) <- c(
      mapLevels(x, codes = FALSE),
      mapLevels(as.character(unknown), codes = FALSE)
    )
  }
  x[is.na(x)] <- unknown
  if (!force)
    warning(sprintf("new level is introduced: %s", unknown), call. = call.)
  x
}

#' @rdname UnknownFuns
#' @method NAToUnknown list
#' @usage \method{NAToUnknown}{list}(x, unknown, force, call., ...)
#' @export
NAToUnknown.list <- function(x, unknown, force = FALSE, call. = FALSE, ...) {
  unknown <- .unknownList(x = x, unknown = unknown)
  x <- mapply(
    FUN = "NAToUnknown",
    x = x,
    unknown = unknown,
    force = force,
    call. = call.,
    SIMPLIFY = FALSE
  )
  x
}

#' @rdname UnknownFuns
#' @method NAToUnknown data.frame
#' @usage \method{NAToUnknown}{data.frame}(x, unknown, force, call., ...)
#' @export
NAToUnknown.data.frame <- function(
  x,
  unknown,
  force = FALSE,
  call. = FALSE,
  ...
) {
  x[] <- NAToUnknown.list(
    x = x,
    unknown = unknown,
    force = force,
    call. = call.
  )
  x
}

#' @rdname UnknownFuns
#' @export
.unknownList <- function(x, unknown) {
  ## --- Setup ---

  n <- length(x)
  unkN <- length(unknown)
  namesX <- names(x)
  namesXNullTest <- is.null(namesX)
  unkNames <- names(unknown)
  unkNamesNullTest <- is.null(unkNames)
  defInNames <- ".default" %in% unkNames
  defInd <- unkNames %in% ".default"
  def <- unknown[defInd]

  if (defInNames) {
    ## Remove default
    unkN <- unkN - 1
    unkNames <- unkNames[!defInd]
    unknown <- unknown[!defInd]
  }

  if (!namesXNullTest) {
    ## Check for nonexistent name
    test <- !(unkNames %in% namesX)
    if (any(test))
      stop(sprintf(
        "name(s) %s not in names of 'x'",
        paste(sQuote(unkNames[test]), collapse = " ")
      ))
  }

  ## --- Recycle ---

  if (unkN < n) {
    if (unkNamesNullTest | defInNames) {
      if (defInNames) {
        # handling .default
        names(def) <- NULL
        unknownDef <- rep(def, length = (n - unkN))
        names(unknownDef) <- namesX[!(namesX %in% unkNames)]
        unknown <- c(unknownDef, unknown)
      } else {
        unknownDef <- unknown
        unknown <- rep(unknownDef, length = n)
      }
    } else {
      stop("can not propely recycle named 'unknown'")
    }
  }

  ## --- Names ---

  if (!namesXNullTest) {
    ## no need if namesX NULL
    if (unkNamesNullTest) {
      ## missing unkNames
      names(unknown) <- namesX
    } else {
      ## unkNames known
      unknown <- unknown[match(namesX, names(unknown))]
    }
  }

  unknown
}

#' @title Report the Space Allocated for Objects
#' @description Provides an estimate of the memory that is being used to store \code{R} objects.
#' @name object.size
#' @usage NULL
#' @aliases object.size
#' @aliases c.object_sizes
#' @aliases as.object_sizes
#' @aliases is.object_sizes
#' @aliases format.object_sizes
#' @aliases print.object_sizes
#' @seealso
#' \code{\link[AlphaPart]{AlphaPart}}
#'
#' @rdname object.size
#' @export
object.size <- function(...) {
  structure(
    sapply(list(...), utils::object.size),
    class = c("object_sizes", "numeric")
  )
}

#' @export
#' @rdname object.size
print.object_sizes <- function(
  x,
  quote = FALSE,
  humanReadable = getOption("humanReadable"),
  standard = "IEC",
  units,
  digits = 1,
  width = NULL,
  sep = " ",
  justify = c("right", "left"),
  ...
) {
  print(
    format(
      x,
      humanReadable = humanReadable,
      standard = standard,
      units = units,
      digits = digits,
      width = width,
      sep = sep,
      justify = justify
    ),
    quote = quote,
    ...
  )

  invisible(x)
}

#' @export
#' @rdname object.size
format.object_sizes <- function(
  x,
  humanReadable = getOption("humanReadable"),
  standard = "IEC",
  units,
  digits = 1,
  width = NULL,
  sep = " ",
  justify = c("right", "left"),
  ...
) {
  if (!missing(units)) {
    if (units == "bytes") paste(x, "bytes") else
      humanReadable(
        x,
        standard = standard,
        units = units,
        digits = digits,
        width = width,
        sep = sep,
        justify = justify
      )
  } else if (is.null(humanReadable) || humanReadable == FALSE)
    paste(x, "bytes") else
    humanReadable(
      x,
      standard = standard,
      units = units,
      digits = digits,
      width = width,
      sep = sep,
      justify = justify
    )
}

#' @export
#' @rdname object.size
is.object_sizes <- function(x) inherits(x, what = "object_sizes")

#' @export
#' @rdname object.size
as.object_sizes <- function(x) {
  if (!is.numeric(x) || any(x < 0))
    stop("'x' must be a positive numeric vector")

  class(x) <- c("object_sizes", "numeric")
  x
}

#' @export
#' @rdname object.size
c.object_sizes <- function(..., recursive = FALSE) {
  x <- NextMethod()
  if (is.numeric(x)) class(x) <- c("object_sizes", "numeric")
  x
}

#' @title Mapping levels
#' @description \code{mapLevels} produces a map with information on levels and/or internal integer codes. As such can be conveniently used to store level mapping when one needs to work with internal codes of a factor and later transfrorm back to factor or when working with several factors that should have the same levels and therefore the same internal coding.
#' @name mapLevels
#' @usage mapLevels(x, codes=TRUE, sort=TRUE, drop=FALSE, combine=FALSE, \dots)
#' @usage mapLevels(x) <- value
#' @aliases mapLevels
#' @aliases mapLevels.default
#' @aliases mapLevels.character
#' @aliases mapLevels.list
#' @aliases mapLevels.data.frame
#' @aliases print.levelsMap
#' @aliases print.listLevelsMap
#' @aliases is.levelsMap
#' @aliases is.listLevelsMap
#' @aliases .checkLevelsMap
#' @aliases .checkListLevelsMap
#' @aliases "[.levelsMap"
#' @aliases "[.listLevelsMap"
#' @aliases c.levelsMap
#' @aliases c.listLevelsMap
#' @aliases unique.levelsMap
#' @aliases sort.levelsMap
#' @aliases mapLevels<-
#' @aliases mapLevels<-.default
#' @aliases mapLevels<-.factor
#' @aliases mapLevels<-.character
#' @aliases mapLevels<-.list
#' @aliases mapLevels<-.data.frame
#' @param x object whose levels will be mapped, look into details `codes` boolean,
#' create integer levelsMap (with internal codes) or character levelsMap (with level names)
#' @param codes boolean, create integer levelsMap (with internal codes) or character levelsMap (with level names)
#' @param sort boolean, sort levels of character \code{x}, look into details
#' @param drop boolean, drop unused levels
#' @param combine boolean, combine levels, look into details
#' @param ... additional arguments for \code{sort}
#' @param value levelsMap or listLevelsMap, output of \code{mapLevels} methods or constructed by user, look into details
#' @seealso
#' \code{\link[AlphaPart]{AlphaPart}}
#'
#' @author Gregor Gorjanc
#'
#' @rdname mapLevels
#' @export
mapLevels <- function(
  x,
  codes = TRUE,
  sort = TRUE,
  drop = FALSE,
  combine = FALSE,
  ...
) {
  UseMethod("mapLevels")
}

#' @rdname mapLevels
#' @method mapLevels default
#' @usage \method{mapLevels}{default}(x, codes, sort, drop, combine, ...)
#' @export
mapLevels.default <- function(
  x,
  codes = TRUE,
  sort = TRUE,
  drop = FALSE,
  combine = FALSE,
  ...
) {
  stop(sprintf(
    "mapLevels can only be used on %s and %s atomic 'x'",
    dQuote("factor"),
    dQuote("character")
  ))
}

#' @rdname mapLevels
#' @method mapLevels character
#' @usage \method{mapLevels}{character}(x, codes, sort, drop, combine, ...)
#' @export
mapLevels.character <- function(
  x,
  codes = TRUE,
  sort = TRUE,
  drop = FALSE,
  combine = FALSE,
  ...
) {
  mapLevels.factor(x = x, codes = codes, sort = sort, drop = drop, ...)
}

## Could coerce character to factor and then use factor method, but that
## is more expensive than simple unique and length used bellow in factor
## method

#' @rdname mapLevels
#' @method mapLevels factor
#' @usage \method{mapLevels}{factor}(x, codes, sort, drop, combine, ...)
#' @export
mapLevels.factor <- function(
  x,
  codes = TRUE,
  sort = TRUE,
  drop = FALSE,
  combine = FALSE,
  ...
) {
  ## --- Argument actions ----

  if (is.factor(x)) {
    # factor
    if (drop) x <- factor(x)
    nlevs <- nlevels(x)
    levs <- levels(x)
  } else {
    # character
    levs <- unique(x)
    nlevs <- length(levs)
    if (sort) levs <- sort(levs, ...)
  }

  ## --- Create a map ---

  map <- vector(mode = "list", length = nlevs)
  names(map) <- levs
  if (codes) {
    map[seq_len(nlevs)] <- seq_len(nlevs)
  } else {
    map[seq_len(nlevs)] <- levs
  }
  class(map) <- "levelsMap"
  map
}

#' @rdname mapLevels
#' @method mapLevels list
#' @usage \method{mapLevels}{list}(x, codes, sort, drop, combine, ...)
#' @export
mapLevels.list <- function(
  x,
  codes = TRUE,
  sort = TRUE,
  drop = FALSE,
  combine = FALSE,
  ...
) {
  map <- lapply(x, mapLevels, codes = codes, sort = sort, drop = drop, ...)
  class(map) <- "listLevelsMap"
  if (combine) {
    if (!codes) {
      return(c(map, sort = sort, recursive = TRUE))
    } else {
      stop(sprintf("can not combine integer %s", dQuote("levelsMaps")))
    }
  }
  map
}

#' @rdname mapLevels
#' @method mapLevels data.frame
#' @usage \method{mapLevels}{data.frame}(x, codes, sort, drop, combine, ...)
#' @export
mapLevels.data.frame <- function(
  x,
  codes = TRUE,
  sort = TRUE,
  drop = FALSE,
  combine = FALSE,
  ...
) {
  mapLevels.list(
    x,
    codes = codes,
    sort = sort,
    drop = drop,
    combine = combine,
    ...
  )
}

#' @rdname mapLevels
#' @export
.unlistLevelsMap <- function(x, ind = FALSE) {
  y <- unlist(x, use.names = FALSE)
  len <- sapply(x, FUN = length)
  names(y) <- rep(names(x), times = len)
  if (ind) {
    return(list(y, rep(seq_len(length(x)), times = len), len))
  } else {
    return(y)
  }
}


#' @rdname mapLevels
#' @export
print.levelsMap <- function(x, ...) {
  x <- .unlistLevelsMap(x)
  print(x, ...)
}


#' @rdname mapLevels
#' @export
print.listLevelsMap <- function(x, ...) {
  class(x) <- "list"
  print(x, ...)
}


## We need these two since [.list method drops class

#' @rdname mapLevels
#' @export
"[.levelsMap" <- function(x, i) {
  classX <- class(x)
  class(x) <- "list"
  x <- x[i]
  class(x) <- classX
  x
}


#' @rdname mapLevels
#' @export
"[.listLevelsMap" <- function(x, i) {
  classX <- class(x)
  class(x) <- "list"
  x <- x[i]
  class(x) <- classX
  x
}

#' @rdname mapLevels
#' @export
is.levelsMap <- function(x) inherits(x = x, what = "levelsMap")


#' @rdname mapLevels
#' @export
is.listLevelsMap <- function(x) inherits(x = x, what = "listLevelsMap")


#' @rdname mapLevels
#' @export
.isCharacterMap <- function(x) {
  if (is(x) == "levelsMap") {
    return(inherits(x = unlist(x), what = "character"))
  } else {
    stop(sprintf("can be used only on %s", dQuote("levelsMap")))
  }
}

#' @rdname mapLevels
#' @export
as.levelsMap <- function(x, check = TRUE, ...) {
  if (check) .checkLevelsMap(x, method = "raw")
  class(x) <- "levelsMap"
  unique(x, ...)
}

#' @rdname mapLevels
#' @export
as.listLevelsMap <- function(x, check = TRUE) {
  if (check) .checkListLevelsMap(x, method = "raw")
  class(x) <- "listLevelsMap"
  x
}

#' @rdname mapLevels
#' @export
.checkLevelsMap <- function(x, method) {
  xLab <- deparse(substitute(x))
  also <- "\b"
  if (method == "class") {
    also <- "also"
    if (!is.levelsMap(x))
      stop(sprintf("'%s' must be a %s", xLab, dQuote("levelsMap")))
  }
  if (!is.list(x) || is.null(names(x)))
    stop(sprintf("'%s' must be %s a named list", xLab, also))

  ## Components can be of different length
  ##  if(!all(sapply(x, FUN=length) == 1))
  ##  stop(sprintf("all components of '%s' must have length 1", xLab))
}

#' @rdname mapLevels
#' @export
.checkListLevelsMap <- function(x, method) {
  xLab <- deparse(substitute(x))
  also <- "\b"
  if (method == "class") {
    also <- "also"
    if (!is.listLevelsMap(x))
      stop(sprintf("'%s' must be a %s", xLab, dQuote("listLevelsMap")))
  }
  if (!is.list(x) || any(!sapply(x, FUN = is.levelsMap)))
    stop(sprintf(
      "'%s' must be %s a list of %s",
      xLab,
      also,
      dQuote("levelsMap")
    ))
  lapply(x, FUN = .checkLevelsMap, method = method)
}

#' @rdname mapLevels
#' @export
c.levelsMap <- function(..., sort = TRUE, recursive = FALSE) {
  x <- list(...)
  class(x) <- "listLevelsMap"
  ## we use recursive=TRUE here because ... is a lists of lists
  c(x, sort = sort, recursive = TRUE)
}

#' @rdname mapLevels
#' @export
c.listLevelsMap <- function(..., sort = TRUE, recursive = FALSE) {
  x <- list(...)
  lapply(x, FUN = .checkListLevelsMap, method = "class")
  x <- unlist(x, recursive = FALSE)
  if (!recursive) {
    class(x) <- "listLevelsMap"
  } else {
    if (any(!sapply(x, FUN = .isCharacterMap)))
      stop(sprintf("can not combine integer %s", dQuote("levelsMaps")))
    if (!is.null(names(x))) names(x) <- NULL
    x <- unlist(x, recursive = FALSE)
    ## how to merge components with the same name?
    class(x) <- "levelsMap"
    if (sort) x <- sort(x)
    x <- unique(x)
  }
  x
}

#' @rdname mapLevels
#' @export
sort.levelsMap <- function(x, decreasing = FALSE, na.last = TRUE, ...)
  x[order(names(x), na.last = na.last, decreasing = decreasing)]

#' @rdname mapLevels
#' @export
unique.levelsMap <- function(x, incomparables = FALSE, ...) {
  ## Find duplicates
  y <- .unlistLevelsMap(x, ind = TRUE)
  ## Duplicates for values and names combinations
  test <- duplicated(
    cbind(y[[1]], names(y[[1]])),
    incomparables = incomparables,
    ...
  )
  if (any(test)) {
    if (any(y[[3]] > 1)) {
      # work with the same structure as in x
      j <- 1
      k <- y[[3]][1]
      empty <- NULL
      for (i in seq(along = x)) {
        # how slow is this loop?
        tmp <- !test[j:k]
        if (all(!tmp)) {
          # these components will be empty
          empty <- c(empty, i)
        } else {
          x[[i]] <- x[[i]][tmp]
        }
        j <- j + y[[3]][i]
        k <- k + y[[3]][i + 1]
      }
      if (!is.null(empty)) x[empty] <- NULL
    } else {
      # simple one-length components
      x <- x[!test]
    }
  }
  x
}

#' @rdname mapLevels
#' @export
"mapLevels<-" <- function(x, value) UseMethod("mapLevels<-")

#' @rdname mapLevels
#' @method mapLevels<- default
#' @export
"mapLevels<-.default" <- function(x, value) {
  ## --- Checks ---

  classX <- c("integer", "character", "factor")
  if (any(!(class(x) %in% classX)))
    stop(sprintf(
      "'x' must be either: %s",
      paste(dQuote(classX), collapse = ", ")
    ))

  .checkLevelsMap(x = value, method = "class")

  ## --- Mapping levels in x ---

  char <- all(sapply(value, is.character))
  int <- all(sapply(value, is.integer))

  if (int) {
    # codes=TRUE
    if (is.integer(x)) x <- factor(x)
    if (is.factor(x)) levels(x) <- value
    if (is.character(x))
      stop(sprintf(
        "can not apply integer %s to %s",
        dQuote("levelsMap"),
        dQuote("character")
      ))
  } else {
    # codes=FALSE
    if (!char) stop("all components of 'value' must be of the same class")
    if (is.character(x)) x <- factor(x)
    if (is.factor(x)) levels(x) <- value
    if (is.integer(x))
      stop(sprintf(
        "can not apply character %s to %s",
        dQuote("levelsMap"),
        dQuote("integer")
      ))
  }
  x
}

#' @rdname mapLevels
#' @method mapLevels<- list
#' @export
"mapLevels<-.list" <- function(x, value) {
  if (!is.listLevelsMap(value)) {
    if (is.levelsMap(value)) {
      value <- as.listLevelsMap(list(value), check = FALSE)
      ## no need for check as default method does checking anyway
    } else {
      stop(sprintf(
        "'x' must be either %s or %s",
        dQuote("listLevelsMap"),
        dQuote("levelsMap")
      ))
    }
  }
  x <- mapply(FUN = "mapLevels<-", x = x, value = value, SIMPLIFY = FALSE)
  x
}

#' @rdname mapLevels
#' @method mapLevels<- data.frame
#' @export
"mapLevels<-.data.frame" <- function(x, value) {
  x[] <- "mapLevels<-.list"(x, value)
  x
}
