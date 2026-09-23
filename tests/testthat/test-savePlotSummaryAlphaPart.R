context("test-savePlotSummary-AlphaPart")

test_that("Test savePlotSummary.AlphaPart", {
  ped <- data.frame(  id=c(  1,   2,   3,   4,   5,   6),
                     fid=c(  0,   0,   2,   0,   4,   0),
                     mid=c(  0,   0,   1,   0,   3,   0),
                     loc=c("A", "A", "C", "B", "C", "C"),
                    trt1=c(100, 120, 115, 130, 125, 125),
                    trt2=c(100, 110, 105, 100,  85, 110),
                     gen=c(  1,   1,   2,   2,   3,   3))

  ## Partition additive genetic values
  m <- AlphaPart(x=ped, colBV=c("trt1", "trt2"))
  sum <- summary(m, by="gen")
  p1 <- plot.summaryAlphaPart(sum)
  
  # pdf
  tmp <- savePlot(p1, type = "pdf")
  expect_equal(file.exists("Rplot.pdf_trt1.pdf"), TRUE)
  expect_equal(file.exists("Rplot.pdf_trt2.pdf"), TRUE)
  unlink(tmp)
  expect_equal(file.exists("Rplot.pdf_trt2.pdf"), FALSE)
  expect_equal(file.exists("Rplot.pdf_trt2.pdf"), FALSE)
  
  # png
  tmp <- savePlot(p1, type = "png")
  expect_equal(file.exists("Rplot.png_trt1.png"), TRUE)
  expect_equal(file.exists("Rplot.png_trt2.png"), TRUE)
  unlink(tmp)
  expect_equal(file.exists("Rplot.png_trt2.png"), FALSE)
  expect_equal(file.exists("Rplot.png_trt2.png"), FALSE)
  
  # jpeg
  tmp <- savePlot(p1, type = "jpeg")
  expect_equal(file.exists("Rplot.jpeg_trt1.jpeg"), TRUE)
  expect_equal(file.exists("Rplot.jpeg_trt2.jpeg"), TRUE)
  unlink(tmp)
  expect_equal(file.exists("Rplot.jpeg_trt2.jpeg"), FALSE)
  expect_equal(file.exists("Rplot.jpeg_trt2.jpeg"), FALSE)

  # tiff
  tmp <- savePlot(p1, type = "tiff")
  expect_equal(file.exists("Rplot.tiff_trt1.tiff"), TRUE)
  expect_equal(file.exists("Rplot.tiff_trt2.tiff"), TRUE)
  unlink(tmp)
  expect_equal(file.exists("Rplot.tiff_trt2.tiff"), FALSE)
  expect_equal(file.exists("Rplot.tiff_trt2.tiff"), FALSE)
  
  # bmp
  tmp <- savePlot(p1, type = "bmp")
  expect_equal(file.exists("Rplot.bmp_trt1.bmp"), TRUE)
  expect_equal(file.exists("Rplot.bmp_trt2.bmp"), TRUE)
  unlink(tmp)
  expect_equal(file.exists("Rplot.bmp_trt2.bmp"), FALSE)
  expect_equal(file.exists("Rplot.bmp_trt2.bmp"), FALSE)
})
