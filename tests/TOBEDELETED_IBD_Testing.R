dataExample <- data.frame(matrix(nrow = 7, ncol = 7))
dataExample[1,] <- c(1, 0, 0, 0, 0.5, 0.5, 0)
dataExample[2,] <- c(2, 0, 0, 0, 0, 0, 1)
dataExample[3,] <- c(3, 1, 2, 0, 0.5, 0, 0.5)
dataExample[4,] <- c(4, 0, 0, 1, 0, 0, 0)
dataExample[5,] <- c(5, 0, 0, 0, 0, 0, 1)
dataExample[6,] <- c(6, 4, 5, 0, 1, 0, 0)
dataExample[7,] <- c(7, 3, 6, 0, 0.5, 0.25, 0.25)
colnames(dataExample) <- c("id", "sid", "mid", "00", "01", "10", "11")
dataExample$bv <- 0*dataExample$`00` + 1*dataExample$`01` + 1*dataExample$`10` + 2*dataExample$`11`


dataExample$bv_f <- 0
dataExample$bv_f <- dataExample$`10` +
  dataExample$`11`
dataExample$bv_m <- 0
dataExample$bv_m <- dataExample$`01` +
  dataExample$`11`
dataExample$group <- c(1,1,1,2,2,2,2)

x <- dataExample[-c(4,5,6,7)]
pathNA=FALSE
recode=TRUE
unknown= NA
sort=TRUE
verbose=1
profile=FALSE
printProfile="end" 
pedType="IPP"
colId=1
colFid=2
colMid=3
colPath=7
colBV=4
colPaternalBV=5
colMaternalBV=6 
colBy=NULL 
center = FALSE 
upgValues = NULL 
scaleEBV = list()

output <- AlphaPart(x = x,
                    pathNA = pathNA,
                    recode = recode,
                    unknown = unknown,
                    sort = sort,
                    verbose = verbose,
                    profile = profile,
                    printProfile = printProfile,
                    pedType = pedType,
                    colId = colId,
                    colFid = colFid,
                    colMid = colMid,
                    colPath = colPath,
                    colBV = colBV,
                    colPaternalBV = colPaternalBV,
                    colMaternalBV = colMaternalBV,
                    colBy = colBy,
                    center = center,
                    upgValues = upgValues,
                    scaleEBV = scaleEBV)
output$bv$gen <- c(1,1,2,1,1,2,3)
sumOutput <- summary(output, by = "gen")
plot(sumOutput)
