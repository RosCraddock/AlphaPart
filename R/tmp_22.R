# TODO: Strip out all centering and scaling in favour of
#       the incoming metafounder / UPG code plans #22
#       https://github.com/AlphaGenes/AlphaPart/issues/22
# We should likely just remove all these functions

#' @title Calculate parent average for base population.
#' @description This is an internally called functions used to calculate 
#' parent average for base population.
#' 
#' @usage NULL
#' 
#' @seealso
#' \code{\link[AlphaPart]{AlphaPart}}
#'
#' @author Thiago de Paula Oliveira
#' 
#' @importFrom stats lm confint
#' @export
centerPop <- function(y, colBV, path){
  # Selecting founders and missing pedigree animals
  colBV <- (ncol(y)-length(colBV)+1):ncol(y)
  if(length(colBV)==1){
    tmp <- as.matrix(y[c(y[, 2]==0 & y[,3]==0), colBV])
  }else{
    tmp <- y[c(y[, 2]==0 & y[,3]==0), colBV]
  }
  baseMean <- colMeans(tmp, na.rm = TRUE)
  # Decision criteria
  basePop <- apply(y[,c(2,3)]==0,1,all)
  for (i in seq_len(ncol(tmp))){
    if(all(confint(lm(tmp[,i] ~ 1), level=0.95)>0)){
      path$ms[-1,i] <- path$ms[-1, i] - basePop * baseMean[i]
      path$pa[-1, i] <- path$pa[-1, i] + basePop * y[, colBV[i]] -
        path$ms[-1, i] * basePop
    }
  }
  return(path)
}

#' @title Scale genetic values
#' @description This is an internally called function used to scale genetic values
#' 
#' @usage NULL
#'
#' @author Thiago de Paula Oliveira
#' @export
sGV <- function(y, center, scale, recode, unknown){
  id. <- 1
  fid. <- 2
  mid. <- 3
  if (recode) {
    y[,id.:mid.] <- cbind(id=seq_len(nrow(y)),
                          fid=match(y[, fid.], y[, id.], nomatch=0),
                          mid=match(y[, mid.], y[, id.], nomatch=0))
  } else {
    ## Make sure we have 0 when recoded data is provided
    if (is.na(unknown)) {
      y[, c(fid., mid.)] <- NAToUnknown(x=y[, c(fid., mid.)], unknown=0)
    } else {
      if (unknown != 0)  {
        y[, c(fid., mid.)] <-
          NAToUnknown(x=unknownToNA(x=y[, c(fid., mid.)],
                                    unknown=unknown), unknown=0)
      }
    }
  }
  # Selecting founders and missing pedigree animals
  if(ncol(y)==(mid.+1)){
    tmp <- as.matrix(y[c(y[, fid.]==0 & y[, mid.]==0), -c(id.:mid.)])
    y <- as.matrix(y[, -c(id.:mid.)])    
  }else{
    tmp <- y[c(y[, fid.]==0 & y[, mid.]==0), -c(id.:mid.)]
    y <- y[, -c(id.:mid.)]
  }
  # Centering
  if(is.logical(center)){
    if(center){
      center <- colMeans(tmp, na.rm = TRUE)
      y <- y - 
        rep(center, rep.int(nrow(y), ncol(y)))
    }
  }
  # Scaling
  if(is.logical(scale)){
    if(scale) {
      f <- function(x) {
        sd(x, na.rm = TRUE)
      }
      scale <- apply(tmp, 2L, f)
      y  <- y / 
        rep(scale, rep.int(nrow(y), ncol(y)))
    }
  }
  return(y)
}  

#' @title Get scale information
#' @description This is an internally called function
#' 
#' @usage NULL
#' 
#' @seealso
#' \code{\link[AlphaPart]{AlphaPart}}
#'
#' @author Thiago de Paula Oliveira
#' 
#' @export
getScale <- function(center = FALSE, scale = FALSE, ...){
  list(center = center, scale = scale, ...)
}