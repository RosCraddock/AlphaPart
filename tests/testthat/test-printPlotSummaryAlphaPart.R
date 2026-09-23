context("test-printPlotSummary-AlphaPart")

test_that("Test print.PlotSummary.AlphaPart", {
  ped <- data.frame(  id=c(  1,   2,   3,   4,   5,   6),
                     fid=c(  0,   0,   2,   0,   4,   0),
                     mid=c(  0,   0,   1,   0,   3,   0),
                     loc=c("A", "A", "C", "B", "C", "C"),
                    trt1=c(100, 120, 115, 130, 125, 125),
                     gen=c(  1,   1,   2,   2,   3,   3))

  ## Partition additive genetic values
  tmp <- AlphaPart(x=ped, colBV="trt1")
  sum <- summary(tmp, by="gen")
  k <- print(plot.summaryAlphaPart(sum))
  expect_equal(k,NULL)
})
