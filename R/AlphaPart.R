#' @useDynLib AlphaPart, .registration = TRUE
#' @importFrom directlabels direct.label last.qp
#' @importFrom dplyr group_by do
#' @import ggplot2
#' @importFrom grDevices dev.cur dev.off
#' @importFrom magrittr %>%
#' @importFrom methods is
#' @importFrom pedigree orderPed
#' @importFrom Rcpp sourceCpp
#' @importFrom reshape melt
#' @importFrom stats aggregate cov sd var
#' @importFrom utils head str tail write.csv2
#' @importFrom tibble is_tibble

#' @description
#' AlphaPart partitions genetic values and their summaries to
#' quantify the sources of genetic change in multi-generational pedigrees.
#' The partitioning method is described in Garcia-Cortes et al. (2008)
#' <doi:10.1017/S175173110800205X>. This method enables a retrospective genetic
#' analysis of past changes in a population and is as such a counterpart to
#' prospective genetic methods describing genetic change .
#' The package includes the main function AlphaPart for
#' partitioning genetic values and auxiliary functions for manipulating data and
#' summarizing, visualizing, and saving results.
#' Genetic values can be breeding values, allele dosages, or similar quantities.
#'
#' See the introductory vignette for instructions on using this package.
#' The vignette can be viewed using the following command:
#' \code{vignette("intro", package = "AlphaPart")}
#' @keywords internal
"_PACKAGE"

#' @title AlphaPart
#'
#' @description A function to partition genetic values by a path
#'   variable. The partition method is described in García-Cortés et
#'   al. (2008): Partition of the genetic trend to validate multiple
#'   selection decisions. Animal: an international journal of animal
#'   bioscience. DOI: \doi{10.1017/S175173110800205X}
#'
#' @param x data.frame, with (at least) the following columns:
#'   individual, father, and mother identification, and year of birth;
#'   see arguments \code{colId}, \code{colFid}, \code{colMid},
#'   \code{colPath}, and \code{colBV}; see also details about the
#'   validity of pedigree.
#' @param pathNA Logical, set dummy path (to "UNKNOWN") where path
#'   information is unknown (missing).
#' @param recode Logical, internally recode individual, father and,
#'   mother identification to \code{1:n} codes, while missing parents
#'   are defined with \code{0}; this option must be used if identifications in
#'   \code{x} are not already given as \code{1:n} codes, see also \code{sort}.
#' @param unknown Value(s) used for representing unknown (missing)
#'   parent in \code{x}; this options has an effect only when
#'   \code{recode=FALSE} as it is only needed in that situation.
#' @param sort Logical, initially sort \code{x} using \code{orderPed()}
#'   so that children follow parents in order to make imputation as
#'   optimal as possible (imputation is performed within a loop from the
#'   first to the last unknown birth year); at the end original order is
#'   restored.
#' @param verbose Numeric, print additional information: \code{0} -
#'   print nothing, \code{1} - print some summaries about the data.
#' @param pedType Character, pedigree type: the most common form is
#'   \code{"IPP"} for Individual, Parent 1 (say father), and Parent 2
#'   (say mother) data; the second form is \code{"IPG"} for Individual,
#'   Parent 1 (say father), and one of Grandparents of Parent 2 (say
#'   maternal grandfather).
#' @param colId Numeric or character, position or name of a column
#'   holding individual identification.
#' @param colFid Numeric or character, position or name of a column
#'   holding father identification.
#' @param colMid Numeric or character, position or name of a column
#'   holding mother identification or maternal grandparent identif
#'   ication if \code{pedType="IPG"} .
#' @param colPath Numeric or character, position or name of a column
#'   holding path information.
#' @param colBV Numeric or character, position(s) or name(s) of
#'   column(s) holding genetic Values.
#' @param colBy Numeric or character, position or name of a column
#'   holding group information (see details).
#'
#' @details Pedigree in \code{x} must be valid in a sense that there
#'   are:
#'
#'   * no directed loops (the simplest example is that the individual
#'   identification is equal to the identification of a father or mother)
#'   * no bisexuality, e.g., fathers most not appear as mothers
#'   * father and/or mother can be unknown (missing) - defined with
#'   any "code" that is different from existing identifications
#'
#' Unknown (missing) values for genetic values are propagated down the
#' pedigree to provide all available values from genetic
#' evaluation. Another option is to cut pedigree links - set parents to
#' unknown and remove them from pedigree prior to using this function -
#' see \code{\link[AlphaPart]{pedSetBase}} function. Warning is issued
#' in the case of unknown (missing) values.
#'
#' In animal breeding literature the model with the underlying
#' pedigree type \code{"IPP"} is often called animal model, while the
#' model for pedigree type \code{"IPG"} is often called sire - maternal
#' grandsire model. With a combination of \code{colFid} and
#' \code{colMid} mother - paternal grandsire model can be accomodated as
#' well.
#'
#' Argument \code{colBy} can be used to directly perform a summary
#' analysis by group, i.e., \code{summary(AlphaPart(...),
#' by="group")}. See \code{\link[AlphaPart]{summary.AlphaPart}} for
#' more. This can save some CPU time by skipping intermediate
#' steps. However, only means can be obtained, while \code{summary}
#' method gives more flexibility.
#'
#' @seealso \code{\link[AlphaPart]{summary.AlphaPart}} for summary
#'   method that works on output of \code{AlphaPart},
#'   \code{\link[AlphaPart]{pedSetBase}} for setting base population,
#'   \code{\link[AlphaPart]{pedFixBirthYear}} for imputing unknown
#'   (missing) birth years, \code{\link[pedigree]{orderPed}} in
#'   \pkg{pedigree} package for sorting pedigree
#'
#' @references Garcia-Cortes, L. A. et al. (2008) Partition of the
#'   genetic trend to validate multiple selection decisions. Animal,
#'   2(6):821-824. \doi{10.1017/S175173110800205X}
#'
#' @return An object of class \code{AlphaPart}, which can be used in
#'   further analyses - there is a handy summary method
#'   (\code{\link[AlphaPart]{summary.AlphaPart}} works on objects of
#'   \code{AlphaPart} class) and a plot method for its output
#'   (\code{\link[AlphaPart]{plot.summaryAlphaPart}} works on objects of
#'   \code{summaryAlphaPart} class).  Class \code{AlphaPart} is a
#'   list. The first \code{length(colBV)} components (one for each trait
#'   and named with trait label, say trt) are data frames. Each
#'   data.frame contains:
#'
#'   * `x` columns from initial data `x`
#'   * `trt_pa` parent average
#'   * `trt_ms` Mendelian sampling term
#'   * `trt_path1, trt_path2, ...` genetic value partitions
#'
#' The last component of returned object is also a list named
#' \code{info} with the following components holding meta information
#' about the analysis:
#'
#' * `path` column name holding path information
#' * `nP` number of paths
#' * `lP` path labels
#' * `nT` number of traits
#' * `lT` trait labels
#' * `warn` potential warning messages associated with this object
#'
#' If \code{colBy!=NULL} the resulting object is of a class
#' \code{summaryAlphaPart}, see
#' \code{\link[AlphaPart]{summary.AlphaPart}} for details.
#'
#' @example inst/examples/examples_AlphaPart.R
#' @export
AlphaPart <- function(
  x,
  pathNA = FALSE,
  recode = TRUE,
  unknown = NA,
  sort = TRUE,
  verbose = 1,
  pedType = "IPP",
  colId = 1,
  colFid = 2,
  colMid = 3,
  colPath = 4,
  colBV = 5:ncol(x),
  colBy = NULL
) {
  ## Test if the data is a data.frame
  if (is_tibble(x)) {
    x <- as.data.frame(x)
  }
  ## --- Setup ---
  test <- (length(colId) > 1 |
    length(colFid) > 1 |
    length(colMid) > 1 |
    length(colPath) > 1 |
    length(colBy) > 1)
  if (test) {
    stop(
      "arguments 'colId', 'colFid', 'colMid', 'colPath', and 'colBy' must be of length 1"
    )
  }

  if (is.null(colBy)) {
    groupSummary <- FALSE
  } else {
    groupSummary <- TRUE
  }

  test <- pedType %in% c("IPP", "IPG")
  if (any(!test)) {
    stop("'pedType' must be either 'IPP' or 'IPG'")
  }

  # -- Test identification
  if (!is.numeric(colId)) {
    colId <- which(colnames(x) %in% colId)
    if (length(colId) == 0) {
      stop("Identification not valid for 'colId' column name", call. = FALSE)
    }
  }
  if (!is.numeric(colMid)) {
    colMid <- which(colnames(x) %in% colMid)
    if (length(colMid) == 0) {
      stop("Identification not valid for 'colMid' column name", call. = FALSE)
    }
  }
  if (!is.numeric(colFid)) {
    colFid <- which(colnames(x) %in% colFid)
    if (length(colFid) == 0) {
      stop("Identification not valid for 'colFid' column name", call. = FALSE)
    }
  }
  if (!is.numeric(colPath)) {
    testN <- length(colPath)
    colPath <- which(colnames(x) %in% colPath)
    if (length(colPath) != testN) {
      stop("Identification not valid for 'colPath' column name", call. = FALSE)
    }
    testN <- NULL # not needed anymore
  }
  if (!is.numeric(colBy)) {
    testN <- length(colBy)
    colByOriginal <- colBy
    colBy <- which(colnames(x) %in% colBy)
    if (length(colBy) != testN) {
      stop("Identification not valid for 'colBy' column name", call. = FALSE)
    }
    testN <- NULL # not needed anymore
  }
  if (!is.numeric(colBV)) {
    testN <- length(colBV)
    colBV <- which(colnames(x) %in% colBV)
    if (length(colBV) != testN) {
      stop("Identification not valid for 'colBV' column(s) name", call. = FALSE)
    }
    testN <- NULL # not needed anymore
  }

  ## --- Sort and recode pedigree ---
  ## Make sure that identifications are numeric if  recode=FALSE
  test <- !sapply(x[, c(colId, colFid, colMid)], is.numeric) & !recode
  if (any(test)) {
    stop(
      "argument 'recode' must be 'TRUE' when identifications in 'x' are not numeric"
    )
  }

  ## Make sure that colBV columns are numeric
  test <- !sapply(x[, c(colBV)], is.numeric)
  if (any(test)) {
    stop("colBV columns must be numeric!")
    str(x)
  }

  ## Sort so that parents precede children
  if (sort) {
    recode <- TRUE
    x <- x[order(orderPed(ped = x[, c(colId, colFid, colMid)])), ]
  }

  # TODO: Strip out all centering and scaling in favour of
  #       the incoming metafounder / UPG code plans #22
  #       https://github.com/AlphaGenes/AlphaPart/issues/22
  # We should likely just remove all this code in the if (FALSE) block
  if (FALSE) {
    # Centering  to make founders has mean zero
    controlvals <- getScale()
    if (!missing(scaleEBV)) {
      controlvals[names(scaleEBV)] <- scaleEBV
    }
    if (controlvals$center == TRUE | controlvals$scale == TRUE) {
      x[, colBV] <- sGV(
        y = x[, c(colId, colFid, colMid, colBV)],
        center = controlvals$center,
        scale = controlvals$scale,
        recode = recode,
        unknown = unknown
      )
    }
  }

  ## Recode all ids to 1:n
  if (recode) {
    y <- cbind(
      id = seq_len(nrow(x)),
      fid = match(x[, colFid], x[, colId], nomatch = 0),
      mid = match(x[, colMid], x[, colId], nomatch = 0)
    )
    colnames(y) <- c(colId, colFid, colMid)
  } else {
    y <- as.matrix(x[, c(colId, colFid, colMid)])
    ## Make sure we have 0 when recoded data is provided
    if (is.na(unknown)) {
      y[, c(colFid, colMid)] <- NAToUnknown(
        x = y[, c(colFid, colMid)],
        unknown = 0
      )
    } else {
      if (unknown != 0) {
        y[, c(colFid, colMid)] <-
          NAToUnknown(
            x = unknownToNA(x = y[, c(colFid, colMid)], unknown = unknown),
            unknown = 0
          )
      }
    }
  }
  y <- cbind(y, as.matrix(x[, colBV]))

  ## Test if father and mother codes precede children code -
  ## computational engine needs this
  test <- y[, 2] >= y[, 1]
  if (any(test)) {
    print(x[test, ])
    print(sum(test))
    stop(
      "sorting/recoding problem: parent (father in this case) code must preceede children code - use arguments 'sort' and/or 'recode'"
    )
  }

  test <- y[, 3] >= y[, 1]
  if (any(test)) {
    print(x[test, ])
    print(sum(test))
    stop(
      "sorting/recoding problem: parent (mother in this case) code must preceede children code - use arguments 'sort' and/or 'recode'"
    )
  }

  ## --- Dimensions and Paths ---
  ## Pedigree size
  nI <- nrow(x)
  ## Traits
  lT <- colnames(x[, colBV, drop = FALSE])
  nT <- length(lT) # number of traits
  colnames(y)[4:ncol(y)] <- lT
  ## Missing values
  nNA <- apply(x[, colBV, drop = FALSE], 2, function(z) sum(is.na(z)))
  names(nNA) <- lT
  ## Paths - P matrix
  test <- is.na(x[, colPath])
  if (any(test)) {
    if (pathNA) {
      x[, colPath] <- as.character(x[, colPath])
      x[test, colPath] <- "UNKNOWN"
    } else {
      stop("unknown (missing) value for path not allowed; use 'pathNA=TRUE'")
    }
  }
  if (!is.factor(x[, colPath])) {
    x[, colPath] <- factor(x[, colPath])
  }
  lP <- levels(x[, colPath])
  nP <- length(lP) # number of paths
  P <- as.integer(x[, colPath]) - 1

  ## Groups
  if (groupSummary) {
    test <- is.na(x[, colBy])
    if (any(test)) {
      if (pathNA) {
        x[, colBy] <- as.character(x[, colBy])
        x[test, colBy] <- "UNKNOWN"
      } else {
        stop("unknown (missing) value for group not allowed; use 'pathNA=TRUE'")
      }
    }
    if (!is.factor(x[, colBy])) {
      x[, colBy] <- factor(x[, colBy])
    }
    lG <- levels(x[, colBy])
    nG <- length(lG)
    g <- as.integer(x[, colBy])
  }

  if (verbose > 0) {
    cat("\nSize:\n")
    cat(" - individuals:", nI, "\n")
    cat(
      " - traits: ",
      nT,
      " (",
      paste(lT, collapse = ", "),
      ")",
      "\n",
      sep = ""
    )
    cat(" - paths: ", nP, " (", paste(lP, collapse = ", "), ")", "\n", sep = "")
    if (groupSummary) {
      cat(
        " - groups: ",
        nG,
        " (",
        paste(lG, collapse = ", "),
        ")",
        "\n",
        sep = ""
      )
    }
    cat(" - unknown (missing) values:\n")
    print(nNA)
  }

  if (any(nNA > 0))
    stop(
      "unknown (missing) values are propagated through the pedigree and therefore not allowed"
    )
  nNA <- NULL # not needed anymore

  ## --- Compute ---

  ## Prepare stuff for C++
  c1 <- c2 <- 0.5
  if (pedType == "IPG") c2 <- 0.25

  ## Add "zero" row (simplif ies computations with missing parents!)
  y <- rbind(y[1, ], y)
  y[1, ] <- 0
  rownames(x) <- NULL
  P <- c(0L, P)
  if (groupSummary) g <- c(0L, g)

  ## Compute
  if (!groupSummary) {
    tmp <- AlphaPartDrop(
      c1 = c1,
      c2 = c2,
      nI = nI,
      nP = nP,
      nT = nT,
      ped = y,
      P = as.integer(P),
      Px = as.integer(cumsum(c(0, rep(nP, nT - 1))))
    )
  } else {
    N <- aggregate(x = y[-1, -c(1:3)], by = list(by = x[, colBy]), FUN = length)
    tmp <- vector(mode = "list", length = 3)
    names(tmp) <- c("pa", "ms", "xa")
    tmp$pa <- tmp$ms <- matrix(data = 0, nrow = nG + 1, ncol = nT)
    tmp$xa <- AlphaPartDropGroup(
      c1 = c1,
      c2 = c2,
      nI = nI,
      nP = nP,
      nT = nT,
      nG = nG,
      ped = y,
      P = as.integer(P),
      Px = as.integer(cumsum(c(0, rep(nP, nT - 1))))
    )
  }

  ## Assign nice column names
  colnames(tmp$pa) <- paste(lT, "_pa", sep = "")
  colnames(tmp$ms) <- paste(lT, "_ms", sep = "")
  colnames(tmp$xa) <- c(t(outer(lT, lP, paste, sep = "_")))

  ## --- Massage results ---

  ## Put partitions for one trait in one object (-1 is for removal of
  ## the "zero" row)
  ret <- vector(mode = "list", length = nT + 1)
  t <- 0
  colP <- colnames(tmp$pa)
  colM <- colnames(tmp$ms)
  colX <- colnames(tmp$xa)

  for (j in 1:nT) {
    ## j <- 1
    Py <- seq(t + 1, t + nP)
    ret[[j]] <- cbind(tmp$pa[-1, j], tmp$ms[-1, j], tmp$xa[-1, Py])
    colnames(ret[[j]]) <- c(colP[j], colM[j], colX[Py])
    t <- max(Py)
  }
  tmp <- NULL # not needed anymore

  ## Add initial data

  if (!groupSummary) {
    for (i in 1:nT) {
      ## Hassle in order to get all columns and to be able to work with
      ##   numeric or character column "names"
      colX <- colX2 <- colnames(x)
      names(colX) <- colX
      names(colX2) <- colX2
      ## ... put current agv in the last column in original data
      colX <- c(colX[!(colX %in% colX[colBV[i]])], colX[colBV[i]])
      ## ... remove other traits
      colX <- colX[
        !(colX %in%
          colX2[(colX2 %in% colX2[colBV]) & !(colX2 %in% colX2[colBV[i]])])
      ]
      ret[[i]] <- cbind(x[, colX], as.data.frame(ret[[i]]))
      rownames(ret[[i]]) <- NULL
    }
  }

  ## Additional (meta) info. on number of traits and paths for other
  ## methods
  tmp <- colnames(x)
  names(tmp) <- tmp
  ret[[nT + 1]] <- list(
    path = tmp[colPath],
    nP = nP,
    lP = lP,
    nT = nT,
    lT = lT,
    warn = NULL
  )
  ## names(ret)[nT+1] <- "info"
  names(ret) <- c(lT, "info")

  ## --- Return ---

  class(ret) <- c("AlphaPart", class(ret))
  if (groupSummary) {
    ret$by <- colByOriginal
    ret$N <- N
    summary(object = ret, sums = TRUE)
  } else {
    ret
  }
}
