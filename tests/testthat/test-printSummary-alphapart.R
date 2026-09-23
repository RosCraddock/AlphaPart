context("test-Printsummary-alphapart")

test_that("Test Printsummary.AlphaPart", {

  ## Small pedigree with additive genetic (=breeding) values
  ped <- data.frame(  id=c(  1,   2,   3,   4,   5,   6),
                     fid=c(  0,   0,   2,   0,   4,   0),
                     mid=c(  0,   0,   1,   0,   3,   0),
                     loc=c("A", "A", "C", "B", "C", "C"),
                    trt1=c(100, 120, 115, 130, 125, 125),
                    trt2=c(100, 110, 105, 100,  85, 110),
                     gen=c(  1,   1,   2,   2,   3,   3))

  ## Partition additive genetic values
  tmp <- AlphaPart(x=ped, colBV=c("trt1", "trt2"))
  ## Test summary for trt1
  expect_equal(print.AlphaPart(summary(tmp, by="gen")), NULL)
})
