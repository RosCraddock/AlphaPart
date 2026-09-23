context("test-plotSummary-AlphaPart")

test_that("Test plotSummary.AlphaPart", {
  ped <- data.frame(  id=c(  1,   2,   3,   4,   5,   6),
                     fid=c(  0,   0,   2,   0,   4,   0),
                     mid=c(  0,   0,   1,   0,   3,   0),
                     loc=c("A", "A", "C", "B", "C", "C"),
                    trt1=c(100, 120, 115, 130, 125, 125),
                    trt2=c(100, 110, 105, 100,  85, 110),
                     gen=c(  1,   1,   2,   2,   3,   3))

  ## Partition additive genetic values
  tmp <- AlphaPart(x=ped, colBV=c("trt1", "trt2"))
  sum <- summary(tmp)
  expect_error(plot.summaryAlphaPart(sum), "output is provided only when the 'by' argument is defined on the 'summary' function")
  
  sum <- summary(tmp, by="gen")
  p1 <- plot.summaryAlphaPart(sum)
  expect_s3_class(p1, "plotSummaryAlphaPart")
  expect_equal(is.list(p1), TRUE)
})
