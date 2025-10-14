#' @title AlphaPart.R
#'
#' @description A function to partition breeding values by a path
#'   variable. The partition method is described in García-Cortés et
#'   al., 2008: Partition of the genetic trend to validate multiple
#'   selection decisions.  Animal : an international journal of animal
#'   bioscience. DOI:  \doi{10.1017/S175173110800205X}
#'
#' @usage
#' AlphaPart(x, pathNA, recode, unknown, sort, verbose, profile,
#'   printProfile, pedType, colId, colFid, colMid, colPath, colBV,
#'   colBy, center, scaleEBV)
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
#' Unknown (missing) values for breeding values are propagated down the
#' pedigree to provide all available values from genetic
#' evaluation. Another option is to cut pedigree links - set parents to
#' unknown and remove them from pedigree prior to using this function -
#' see \code{\link[AlphaPart]{pedSetBase}} function.  Warning is issued
#' in the case of unknown (missing) values.
#'
#' In animal breeding/genetics literature the model with the underlying
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
#' @param x data.frame , with (at least) the following columns:
#'   individual, father, and mother identification, and year of birth;
#'   see arguments \code{colId}, \code{colFid}, \code{colMid},
#'   \code{colPath}, and \code{colBV}; see also details about the
#'   validity of pedigree. For optional columns see arguments \code{colBy}, 
#'   \code{colPaternalBV}, and \code{colMaternalBV}.
#' @param pathNA Logical, set dummy path (to "XXX") where path
#'   information is unknown (missing).
#' @param recode Logical, internally recode individual, father and,
#'   mother identification to \code{1:n} codes, while missing parents
#'   are defined with \code{0}; this option must be used if identif
#'   ications in \code{x} are not already given as \code{1:n} codes, see
#'   also argument \code{sort}.
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
#' @param profile Logical, collect timings and size of objects.
#' @param printProfile Character, print profile info on the fly
#'   (\code{"fly"}) or at the end (\code{"end"}).
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
#'   column(s) holding breeding Values.
#' @param colPaternalBV Numeric or character, position(s) or name(s) of a
#'  column holding paternal breeding values calculated from the phased 
#'  genotype probabilities. If provided along with \code{colMaternalBV}, these
#'  values will be used to calculate the gametic partitioning.
#' @param colMaternalBV Numeric or character, position(s) or name(s) of a
#' column holding maternal breeding values calculated from the phased
#' genotype probabilities. If provided along with \code{colPaternalBV}, these
#' values will be used to calculate the gametic partitioning.
#' @param colBy Numeric or character, position or name of a column
#'   holding group information (see details).
#' @param center Logical, if \code{center=TRUE} detect a shift in base
#'   population mean and attributes it as parent average effect rather
#'   than Mendelian sampling effect, otherwise, if center=FALSE, the base
#'   population values are only accounted as Mendelian sampling
#'   effect. Default is \code{center = TRUE}.
#' @param upgValues A dataframe with the following cloumns: the named unknown 
#'   parent group (UPG) starting with "UPG" and a column for each trait with the 
#'   corresponding UPG value. These are used only where UPGs are present in the 
#'   pedigree. If not provided and UPGs are present in the pedigree, the UPG 
#'   values are estimated from the mean of the founders for each trait.
#' @param scaleEBV a list with two arguments defining whether is 
#' appropriate to center and/or scale the \code{colBV} columns in respect to 
#' the base population. The list may contain the following components:
#' 
#' * `center`: a logical value 
#' * `scale`: a logical value. If `center = TRUE` and `scale = TRUE` then the 
#'  base population is set to have zero mean and unit variance.
#'
#' @example inst/examples/examples_AlphaPart.R
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
#'   * `trt_w`Mendelian sampling term
#'   * `trt_path1, trt_path2, ...` breeding value partitions
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
#' * `upgPresent` logical, whether unknown parent groups (UPG) are present in the pedigree
#' * `warn` potential warning messages associated with this object
#'
#' If \code{colBy!=NULL} the resulting object is of a class
#' \code{summaryAlphaPart}, see
#' \code{\link[AlphaPart]{summary.AlphaPart}} for details.
#'
#' If \code{profile=TRUE}, profiling info is printed on screen to spot
#' any computational bottlenecks.
#'
#' @useDynLib AlphaPart, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#'
#' @importFrom utils str
#' @importFrom pedigree orderPed
#' @importFrom stats aggregate
#' @importFrom tibble is_tibble
#'
#' @export

AlphaPart <- function (x, pathNA=FALSE, recode=TRUE, unknown= NA,
                       sort=TRUE, verbose=1, profile=FALSE,
                       printProfile="end", pedType="IPP", colId=1,
                       colFid=2, colMid=3, colPath=4, colBV=5:ncol(x),
                       colPaternalBV=NULL, colMaternalBV=NULL, colBy=NULL, 
                       center = TRUE, upgValues = NULL, 
                       scaleEBV = list()) {
  ## Test if the data is a data.frame
  if(is_tibble(x)){
    x <- as.data.frame(x)
  }
  ## --- Setup ---
  test <- (length(colId) > 1 | length(colFid) > 1 | length(colMid) > 1 | length(colPath) > 1 | length(colBy) > 1)
  if (test) {
    stop("arguments 'colId', 'colFid', 'colMid', 'colPath', and 'colBy' must be of length 1")
  }

  if (is.null(colBy)) {
    groupSummary <- FALSE
  } else {
    groupSummary <- TRUE
  }
  
  #TODO: Add user-warnings if only one of colPaternalBV or colMaternalBV is provided
  if(is.null(colPaternalBV) | is.null(colMaternalBV)){
    gameticPartition <- FALSE
  } else{
    gameticPartition <- TRUE
  }

  test <- pedType %in% c("IPP", "IPG")
  if (any(!test)) {
    stop("'pedType' must be either 'IPP' or 'IPG'")
  }
  #=====================================================================
  if (profile) {
    time0 <- Sys.time()
    cat("\nStart:", format(time0), "\n")
    timeRet <- data.frame(task="Start", timeP=time0, time=0, timeCum=0,
                          memory=0, memoryCum=0,
                          stringsAsFactors=FALSE)
    .profilePrint <- function(x, task, printProfile, time, mem, update=
                                                                  FALSE)
    {
      i <- nrow(x)
      x[i + 1, "task"]        <- task
      x[i + 1, "timeP"]       <- time
      x[i + 1, "time"]        <- timeTMP1 <- round(time - x[i, "timeP"], digits=1L)
      x[i + 1, "timeCum"]     <- timeTMP2 <- round(time - x[1, "timeP"], digits=1L)
      if (!update) {
        x[i + 1, "memory"]    <- memTMP1  <- round(mem/1024^2, digits=1L)
        x[i + 1, "memoryCum"] <- memTMP2  <- round(mem/1024^2, digits=1L) + x[i, "memoryCum"]
      } else {
        x[i + 1, "memory"]    <- memTMP1  <- round(mem/1024^2, digits=1L)
        x[i + 1, "memory"]    <- abs(x[i + 1, "memory"] - x[i, "memory"])
        x[i + 1, "memoryCum"] <- memTMP2  <- x[i + 1, "memory"] + x[i, "memoryCum"]
      }
      if (printProfile == "fly") {
        cat("\n", task, ":\n", sep="")
        cat(" - time (this task):",     format(timeTMP1),     "\n")
        cat(" - time (all tasks):",     format(timeTMP2),     "\n")
        cat(" - memory (this object):", paste(memTMP1, "Mb"), "\n")
        cat(" - memory (all objects):", paste(memTMP2, "Mb"), "\n")
      }
      x
    }
  }
  #=======================================================================
  # -- Test identification
  #=======================================================================
  if(!is.numeric(colId)){
    colId <- which(colnames(x) %in% colId)
    if (length(colId)==0) {
      stop("Identification not valid for 'colId' column name", call. = FALSE)
    } 
  }
  if(!is.numeric(colMid)){
    colMid <- which(colnames(x) %in% colMid)
    if (length(colMid)==0) {
      stop("Identification not valid for 'colMid' column name", call. = FALSE)
    }
  }
  if(!is.numeric(colFid)){
    colFid <- which(colnames(x) %in% colFid)
    if (length(colFid)==0) {
      stop("Identification not valid for 'colFid' column name", call. = FALSE)
    }
  }
  if(!is.numeric(colPath)){
    testN <- length(colPath)
    colPath <- which(colnames(x) %in% colPath)
    if (length(colPath)!=testN) {
      stop("Identification not valid for 'colPath' column name", call. = FALSE)
    }
    testN <-  NULL # not needed anymore
  }
  if(!is.numeric(colBy)){
    testN <- length(colBy)
    colByOriginal <- colBy
    colBy <- which(colnames(x) %in% colBy)
    if (length(colBy)!=testN) {
      stop("Identification not valid for 'colBy' column name", call. = FALSE)
    }
    testN <- NULL # not needed anymore
  }
  if(!is.numeric(colBV)){
    testN <- length(colBV)
    colBV <- which(colnames(x) %in% colBV)
    if (length(colBV) != testN) {
      stop("Identification not valid for 'colBV' column(s) name", call. = FALSE)
    }
    testN <- NULL # not needed anymore
  }
  if(gameticPartition & !is.numeric(colPaternalBV)){
    testN <- length(colPaternalBV)
    colBV <- which(colnames(x) %in% colPaternalBV)
    if (length(colPaternalBV) != testN) {
      stop("Identification not valid for 'colPaternalBV' column(s) name", call. = FALSE)
    }
    testN <- NULL # not needed anymore
  }
  if(gameticPartition & !is.numeric(colMaternalBV)){
    testN <- length(colMaternalBV)
    colBV <- which(colnames(x) %in% colMaternalBV)
    if (length(colMaternalBV) != testN) {
      stop("Identification not valid for 'colMaternalBV' column(s) name", call. = FALSE)
    }
    testN <- NULL # not needed anymore
  }
  #=====================================================================
  ## --- Sort and recode pedigree ---
  #=====================================================================
  ## Make sure that identifications are numeric if  recode=FALSE
  ## Exceptions for inclusion of UPG
  tmp <- x[!(substr(x[,colMid], 1, 3) == "UPG" | substr(x[,colFid], 1, 3) == "UPG"),]
  test <- !sapply(tmp[, c(colId, colFid, colMid)], is.numeric) & !recode
  if (any(test)) {
    stop("argument 'recode' must be 'TRUE' when identifications in 'x' are not numeric")
  }
  ## Noting whether UPGs are present
  upgPresent <- nrow(tmp) != nrow(x)
  #---------------------------------------------------------------------
  ## Make sure that colBV columns are numeric
  test <- !sapply(x[, c(colBV)], is.numeric)
  if (any(test)) {
    stop("colBV columns must be numeric!")
    str(x)
  }
  #---------------------------------------------------------------------
  ## If gametic partitioning make sure that colPaternalBV and colMaternalBV:
  ## columns are numeric,
  ## the same length as colBV,
  ## the columns sum to the columns of colBV.
  
  if (gameticPartition){
    test <- !sapply(x[, c(colPaternalBV)], is.numeric)
    if (any(test)) {
      stop("colPaternalBV columns must be numeric!")
      str(x)
    }
    test <- !sapply(x[, c(colMaternalBV)], is.numeric)
    if (any(test)) {
      stop("colMaternalBV columns must be numeric!")
      str(x)
    } 
    test <- length(colPaternalBV) != length(colMaternalBV)
    if (any(test)){
      stop(paste("colPaternalBV has length ", length(colPaternalBV), 
                  ", while colMaternalBV has length ", length(colMaternalBV),
                  ". Hence, gametic partitioning cannot be performed.", sep = ""))
    }
    test <- length(colPaternalBV) != length(colBV)
    if (any(test)){
      stop(paste("colPaternalBV and colMaternalBV has length ", length(colPaternalBV), 
                  ", while colBV has length ", length(colBV),
                  ". Hence, gametic partitioning cannot be performed.", sep = ""))
    }
    test <- any(x[, c(colPaternalBV)] + x[, c(colMaternalBV)] != x[, c(colBV)])
    if (any(test)){
      stop("The sum of colPaternalBV and colMaternalBV must be equal to colBV for each individual.")
    }
  }
  #---------------------------------------------------------------------
  ## tests for when upg in pedigree
  if (upgPresent) {
    # Test assigned upgs are the same in both sire and dam
    founders <- x[substr(x[,colMid], 1, 3) == "UPG" | substr(x[,colFid], 1, 3) == "UPG", ]
    test <- founders[, colMid] != founders[, colFid]
    if (any(test)) {
      stop("When UPGs are present, the same UPG must be assigned to both parents")
    }
    # Test there are no missing values in sire or dams
    test <- is.na(x[,colMid]) | x[, colMid] == "" | x[, colMid] == 0 |
      is.na(x[,colFid]) | x[, colFid] == "" | x[, colFid] == 0
    if (any(test)) {
      stop("When UPGs are present, there cannot be missing values in sire or dam")
    }
  }
  #---------------------------------------------------------------------
  ## Sort so that parents precede children
  if (sort) {
    recode <- TRUE
    x <- x[order(orderPed(ped=x[, c(colId, colFid, colMid)])), ]
  }
  #=======================================================================
  # Centering  to make founders have mean zero
  #=======================================================================
  controlvals <- getScale()
  if (!missing(scaleEBV)) {
    controlvals[names(scaleEBV)] <- scaleEBV
  }
  if(controlvals$center == TRUE | controlvals$scale == TRUE){
    x[, colBV] <- sEBV(y = x[,c(colId, colFid, colMid, colBV)], 
                       center = controlvals$center, 
                       scale = controlvals$scale, 
                       recode = recode, unknown = unknown)
  }
  #=======================================================================
  #---------------------------------------------------------------------
  ## Recode all ids to 1:n
  if (recode) {
    # Add another conditional, as only need to collect upg if upgPresent
    y <- cbind(id=seq_len(nrow(x)),
               fid=match(x[, colFid], x[, colId], nomatch=0),
               mid=match(x[, colMid], x[, colId], nomatch=0),
               upg=x[,colFid]) # Only need one as same UPG assigned to both parents
    upg <- y[,4] # contains the upg
    upg[!(substr(upg, 1, 3) == "UPG")] <- 0
    if (is.na(unknown)){
      upg[is.na(upg)] <- 0
    } else if (unknown != 0) {
      upg[upg == unknown] <- 0
    }
    y <- cbind(as.numeric(y[,1]), as.numeric(y[,2]), as.numeric(y[,3]))
    colnames(y) <- c(colId,colFid,colMid)
  } else {
    y <- as.matrix(x[, c(colId, colFid, colMid)])
    upg <- y[,2] # contains the upg
    upg[!(substr(upg, 1, 3) == "UPG")] <- 0
    y[substr(y[,2], 1, 3) == "UPG", c(2,3)] <- 0
    y <- cbind(as.numeric(y[,1]), as.numeric(y[,2]), as.numeric(y[,3]))
    ## Make sure we have 0 when recoded data is provided
    if (is.na(unknown)) {
      y[, c(colFid, colMid)] <- NAToUnknown(x=y[, c(colFid, colMid)],
                                            unknown=0)
    } else {
      if (unknown != 0)  {
        y[, c(colFid, colMid)] <-
          NAToUnknown(x=unknownToNA(x=y[, c(colFid, colMid)],
                                    unknown=unknown), unknown=0)
      }
    }
  }
  if (!gameticPartition){
    y <- cbind(y, as.matrix(x[, colBV]))
    nGP <- 1 # Number of genetic partitions: total
  } else {
    y <- cbind(y, as.matrix(x[, colBV]), as.matrix(x[, colPaternalBV]), as.matrix(x[, colMaternalBV]))
    nGP <- 3 # Number of genetic partitions: total, paternal, maternal
  }
  
  #=====================================================================
  ## Test if father and mother codes precede children code -
  ## computational engine needs this
  #=====================================================================
  test <- y[, 2] >= y[, 1]
  if (any(test)) {
    print(x[test, ])
    print(sum(test))
    stop("sorting/recoding problem: parent (father in this case) code must precede children code - use arguments 'sort' and/or 'recode'")
  }
  #---------------------------------------------------------------------
  test <- y[, 3] >= y[, 1]
  if (any(test)) {
    print(x[test, ])
    print(sum(test))
    stop("sorting/recoding problem: parent (mother in this case) code must precede children code - use arguments 'sort' and/or 'recode'")
  }
  #---------------------------------------------------------------------
  if (profile) {
    timeRet <- .profilePrint(x=timeRet, task="Sort and/or recode pedigree", printProfile=printProfile,
                             time=Sys.time(), mem=(object.size(x) + object.size(y)))
  }
  #=====================================================================
  ## --- Dimensions and Paths ---
  #=====================================================================
  ## Pedigree size
  nI <- nrow(x)
  #---------------------------------------------------------------------
  ## Traits
  lT <- colnames(x[, colBV, drop=FALSE])
  nT <- length(lT) # number of traits
  colnames(y)[4:ncol(y)] <- lT
  #---------------------------------------------------------------------
  ## Missing values
  nNA <- apply(x[, colBV, drop=FALSE], 2, function(z) sum(is.na(z)))
  names(nNA) <- lT
  #---------------------------------------------------------------------
  ## Paths - P matrix
  test <- is.na(x[, colPath])
  if (any(test)) {
    if (pathNA) {
      x[, colPath] <- as.character(x[, colPath])
      x[test, colPath] <- "XXX"
    } else {
      stop("unknown (missing) value for path not allowed; use 'pathNA=TRUE'")
    }
  }
  if (!is.factor(x[, colPath])) x[, colPath] <- factor(x[, colPath])
  lP <- levels(x[, colPath])
  nP <- length(lP) # number of paths
  P <- as.integer(x[, colPath]) - 1
  #---------------------------------------------------------------------
  ## Groups
  if (groupSummary) {
    test <- is.na(x[, colBy])
    if (any(test)) {
      if (pathNA) {
        x[, colBy] <- as.character(x[, colBy])
        x[test, colBy] <- "XXX"
      } else {
        stop("unknown (missing) value for group not allowed; use 'pathNA=TRUE'")
      }
    }
    if (!is.factor(x[, colBy])) x[, colBy] <- factor(x[, colBy])
    lG <- levels(x[, colBy])
    nG <- length(lG)
    g <- as.integer(x[, colBy])
  }

  if (verbose > 0) {
    cat("\nSize:\n")
    cat(" - individuals:", nI, "\n")
    cat(" - traits: ", nT, " (", paste(lT, collapse=", "), ")", "\n", sep="")
    cat(" - paths: ",  nP, " (", paste(lP, collapse=", "), ")", "\n", sep="")
    if (groupSummary) {
      cat(" - groups: ", nG, " (", paste(lG, collapse=", "), ")", "\n", sep="")
    }
    cat(" - unknown (missing) values:\n")
    print(nNA)
  }

  if (any(nNA > 0)) stop("unknown (missing) values are propagated through the pedigree and therefore not allowed")
  nNA <- NULL # not needed anymore
  
  if (profile) {
    timeRet <- .profilePrint(x=timeRet, task="Dimensions and Matrices P", printProfile=printProfile,
                             time=Sys.time(), mem=object.size(P))
  }
  #=====================================================================
  ## Unknown parent group set up
  #=====================================================================
  ## Set up matrix to hold UPG values for each trait in each individual
  upgCon <- matrix(nrow = nI, ncol = nT, data = 0)
  
  if (upgPresent) {
    presentUPG <- unique(upg[substr(upg, 1, 3) == "UPG"])
    if (!is.null(upgValues) & !center) {
      test <- !presentUPG %in% upgValues[,1]
      if (any(test)) {
        stop("Not all UPGs in the pedigree has a value in the upgValues argument")
      }
    } else if (is.null(upgValues) | center) {
      if (is.null(upgValues)) {
        print("upgValues have not been provided. This will be estimated for each trait using the mean of the founders.")
      } else if (center) {
        print("upgValues have been provided but will not be used as centering is TRUE. The UPG values will be estimated for each trait using the mean of the founders.")
        upgValues <- NULL
      }
      
      yUPG <- cbind(y, upg)
      yUPG <- data.frame(yUPG)
      nCol <- ncol(y)
      yUPG[,4:nCol] <- sapply(yUPG[,4:nCol], as.numeric)
      meanUPG <- aggregate(y[,4:nCol], by = list(yUPG$upg), FUN = mean, na.rm = TRUE)
      upgValues <- data.frame(meanUPG[match(presentUPG, meanUPG[,1]),])
      colnames(upgValues) <- c("UPG", lT)
    }
    # Add values to the founders in upgCon
    upg_mask <- substr(upg, 1, 3) == "UPG"
    upg_subset <- upg[upg_mask]
    match_indices <- match(upg_subset, upgValues[,1])
    upgCon[upg_mask, ] <- as.matrix(upgValues[match_indices, 2:ncol(upgValues)])
  }
  
  # Add a "zero" row to upgCon
  upgCon <- rbind(upgCon[1,], upgCon)
  upgCon[1,] <- 0
  
  #=====================================================================
  ## --- Compute ---
  #=====================================================================
  ## Prepare stuff for C++
  c1 <- c2 <- 0.5
  if (pedType == "IPG") c2 <- 0.25
  #---------------------------------------------------------------------
  ## Add "zero" row (simplifies computations with missing parents!)
  y <- rbind(y[1, ], y)
  y[1, ] <- 0
  rownames(x) <- NULL
  P <- c(0, P)
  if (groupSummary) g <- c(0, g)
  #---------------------------------------------------------------------
  ## Compute
  if (!groupSummary) {
    tmp <- .Call("AlphaPartDrop",
                 c1_=c1, c2_=c2,
                 nI_=nI, nP_=nP, nT_=nT,
                 y_=y, 
                 P_=P, Px_=cumsum(c(0, rep(nP, nT-1))),
                 upgCon_=upgCon, nGP_=nGP,
                 PACKAGE="AlphaPart")
  } else {
    N <- aggregate(x=y[-1, -c(1:3)], by=list(by=x[, colBy]), FUN=length)
    tmp <- vector(mode="list", length=3)
    names(tmp) <- c("pa", "w", "xa")
    tmp$pa <- tmp$w <- matrix(data=0, nrow=nG+1, ncol=nT)
    tmp$xa <- .Call("AlphaPartDropGroup",
                 c1_=c1, c2_=c2,
                 nI_=nI, nP_=nP, nT_=nT, nG_=nG,
                 y_=y, P_=P, Px_=cumsum(c(0, rep(nP, nT-1))), g_=g,
                 PACKAGE="AlphaPart")
  }
  #---------------------------------------------------------------------
  ## Assign nice column names
  if (gameticPartition){
    lPT <- colnames(x[, c(colBV, colPaternalBV, colMaternalBV), drop=FALSE])
    colnames(tmp$pa) <- paste(lPT, "_pa", sep="")
    colnames(tmp$w)  <- paste(lPT, "_w", sep="")
  } else {
    colnames(tmp$pa) <- paste(lT, "_pa", sep="")
    colnames(tmp$w)  <- paste(lT, "_w", sep="")
  }
  colnames(tmp$xa) <- c(t(outer(lT, lP, paste, sep="_")))
  if (upgPresent) {
    colnames(tmp$upgCon) <- paste(lT, "_upg", sep="")
  }

  if (profile) {
    timeRet <- .profilePrint(x=timeRet, task="Computing",
                             printProfile=printProfile,
                             time=Sys.time(), mem=object.size(tmp))
  }
  #=====================================================================
  ## --- Massage results ---
  #=====================================================================
  ## Put partitions for one trait in one object (-1 is for removal of
  ## the "zero" row)
  ret <- vector(mode="list", length=nT+1)
  t <- 0
  colP <- colnames(tmp$pa)
  colW <- colnames(tmp$w)
  colX <- colnames(tmp$xa)
  colUPG <- colnames(tmp$upgCon)
  #=====================================================================
  # Original Values 
  #=====================================================================
  if (center){
    tmp <- centerPop(y = y[-1,], colBV = colBV, path = tmp)    
  }

  #=====================================================================
  if (upgPresent){
    if (gameticPartition) {
      for (j in 1:nT) { ## j <- 1
        Py <- seq(t+1, t+nP)
        gP <- c(j, j+nT, j+2*nT)
        ret[[j]] <- cbind(tmp$pa[-1, gP], tmp$w[-1, gP], tmp$upgCon[-1,j], tmp$xa[-1, Py])
        colnames(ret[[j]]) <- c(colP[gP], colW[gP], colUPG[j], colX[Py])
        t <- max(Py)
      }
      tlP <- c(lP, "upg")
      lP <- levels(as.factor(tlP))
      nP <- length(lP)
    } else {
      for (j in 1:nT) { ## j <- 1
        Py <- seq(t+1, t+nP)
        ret[[j]] <- cbind(tmp$pa[-1, j], tmp$w[-1, j], tmp$upgCon[-1,j], tmp$xa[-1, Py])
        colnames(ret[[j]]) <- c(colP[j], colW[j], colUPG[j], colX[Py])
        t <- max(Py)
      }
      tlP <- c(lP, "upg")
      lP <- levels(as.factor(tlP))
      nP <- length(lP)
    }
    
  } else {
    if (gameticPartition) {
      for (j in 1:nT) { ## j <- 1
        Py <- seq(t+1, t+nP)
        gP <- c(j, j+nT, j+2*nT)
        ret[[j]] <- cbind(tmp$pa[-1, gP], tmp$w[-1, gP], tmp$xa[-1, Py])
        colnames(ret[[j]]) <- c(colP[gP], colW[gP], colX[Py])
        t <- max(Py)
      }
    } else
    for (j in 1:nT) { ## j <- 1
      Py <- seq(t+1, t+nP)
      ret[[j]] <- cbind(tmp$pa[-1, j], tmp$w[-1, j], tmp$xa[-1, Py])
      colnames(ret[[j]]) <- c(colP[j], colW[j], colX[Py])
      t <- max(Py)
    }
  }
  tmp <- NULL # not needed anymore
  #---------------------------------------------------------------------
  if (profile) {
    timeRet <- .profilePrint(x=timeRet, task="Massage results",
                             printProfile=printProfile,
                             time=Sys.time(), mem=object.size(ret))
  }
  #=====================================================================
  ## Add initial data
  #=====================================================================
  if (!groupSummary) {
    for (i in 1:nT) {
      ## Hassle in order to get all columns and to be able to work with
      ##   numeric or character column "names"
      colX <- colX2 <- colnames(x)
      names(colX) <- colX; names(colX2) <- colX2
      ## ... put current agv in the last column in original data
      colX <- c(colX[!(colX %in% colX[colBV[i]])], colX[colBV[i]])
      ## ... remove other traits
      colX <- colX[!(colX %in% colX2[(colX2 %in% colX2[colBV]) & !
                                       (colX2 %in% colX2[colBV[i]])])]
      ret[[i]] <- cbind(x[, colX], as.data.frame(ret[[i]]))
      rownames(ret[[i]]) <- NULL
    }
  }
  #---------------------------------------------------------------------
  ## Additional (meta) info. on number of traits and paths for other
  ## methods
  tmp <- colnames(x); names(tmp) <- tmp
  ret[[nT+1]] <- list(path=tmp[colPath], nP=nP, lP=lP, nT=nT, lT=lT, 
                      upgPresent=upgPresent, warn=NULL)
  ## names(ret)[nT+1] <- "info"
  names(ret) <- c(lT, "info")

  if (profile) {
    timeRet <- .profilePrint(x=timeRet, task="Finalizing returned object + adding initial data", printProfile=printProfile,
                             time=Sys.time(), mem=object.size(ret), update=TRUE)
  }
  #---------------------------------------------------------------------
  # Profile
  #---------------------------------------------------------------------
  if (profile){
    ret$info$profile <- timeRet
    if (printProfile == "end") {
      print(timeRet)
    }
  }
  #=====================================================================
  ## --- Return ---
  #=====================================================================
  class(ret) <- c("AlphaPart", class(ret))
  if (groupSummary) {
    ret$by <- colByOriginal
    ret$N <- N
    summary(object=ret, sums=TRUE)
  } else {
    ret
  }
}
