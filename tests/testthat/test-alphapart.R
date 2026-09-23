context("test-alphapart")

test_that("Test input for AlphaPart ped", {
  # Example - not really interesting but set up in a way that we can test behaviour of AlphaPart()
  ped <- data.frame(
    id = c("A", "B", "C", "F", "G", "J", "K", "E", "D", "I", "H", "L", "M"),
    fid = c(NA, NA, NA, "A", "C", "F", "F", "A", "A", "A", "C", "K", NA),
    mid = c(NA, NA, NA, "B", "B", "G", "G", "B", "B", "B", "B", "H", "H"),
    by = c(0, 0, 0, 1, 1, 2, 2, 1, 1, 1, 2, 3, 3) + 2000,
    dum1 = 13:1,
    pat = c("A", "A", "B", "A", "A", "A", "A", "A", "B", "C", NA, "B", "B"),
    trt1 = c(
      0.56,
      0.04,
      -0.60,
      0.47,
      -0.31,
      0.03,
      0,
      1.00,
      1.00,
      1.00,
      0.10,
      0.50,
      0.00
    ),
    dum2 = (1:13) + 6,
    trt2 = c(0.10, 0.24, -0.30, 0.17, -0.21, 0.13, 0, 0, 0, 0, 0.20, 0, 0.00)
  )

  # ... to test unknown argument
  ped2 <- ped
  ped2$fid <- as.character(ped2$fid)
  ped2[is.na(ped2$fid), "fid"] <- "0"
  ped2$fid <- as.factor(ped2$fid)
  # TODO: Finish test for unknown argument #53
  #       https://github.com/AlphaGenes/AlphaPart/issues/53

  # ... to test recode argument
  ped3 <- ped[order(orderPed(ped = ped[, c("id", "fid", "mid")])), ]
  ped3$idI <- seq_len(nrow(ped3))
  ped3$fidI <- match(ped3$fid, ped3$id)
  ped3$midI <- match(ped3$mid, ped3$id)
  # TODO: Finish test for recode argument #54
  #       https://github.com/AlphaGenes/AlphaPart/issues/54

  # ... to test recode and unknown argument
  ped4 <- ped[order(orderPed(ped = ped[, c("id", "fid", "mid")])), ]
  ped4$idI <- seq_len(nrow(ped4))
  ped4$fidI <- match(ped4$fid, ped3$id, nomatch = 99)
  ped4$midI <- match(ped4$mid, ped3$id, nomatch = 99)
  expect_error(AlphaPart(
    x = ped4[, c("idI", "fidI", "midI", "pat", "trt1")],
    pathNA = TRUE,
    recode = FALSE,
    verbose = 0
  ))
  # TODO: Finish test for unknown argument #53
  #       https://github.com/AlphaGenes/AlphaPart/issues/53
  #       The above should throw an error, but maybe we need to rip out
  #       all the unknown handling and just stick with NA
  #       for all the unknowns!!!!

  expect_no_error(AlphaPart(
    x = ped4[, c("idI", "fidI", "midI", "pat", "trt1")],
    pathNA = TRUE,
    recode = FALSE,
    unknown = 99,
    verbose = 0
  ))

  # Error if recode=FALSE and input is not numeric
  expect_error(AlphaPart(
    x = ped[, c("id", "fid", "mid", "pat", "trt1", "trt2")],
    pathNA = TRUE,
    recode = FALSE,
    verbose = 0
  ))

  # Path column must not contain unknown/NA values
  expect_error(AlphaPart(
    x = ped5[, c("id", "fid", "mid", "pat", "trt1")],
    verbose = 0
  ))
  tmp <- AlphaPart(
    x = ped[, c("id", "fid", "mid", "pat", "trt1")],
    pathNA = TRUE,
    verbose = 0
  )
  expect_true("UNKNOWN" %in% tmp$info$lP)

  # colBV columns must be numeric
  ped5 <- ped
  ped5$trt1 <- as.character(ped5$trt1)
  expect_error(AlphaPart(
    x = ped5[, c("id", "fid", "mid", "pat", "trt1")],
    verbose = 0
  ))

  # Error if NAs are present in the trait
  pedX <- ped
  pedX[1, "trt1"] <- NA
  expect_error(AlphaPart(
    x = pedX[, c("id", "fid", "mid", "pat", "trt1", "trt2")],
    pathNA = TRUE,
    verbose = 0
  ))
})

test_that("Test the output of AlphaPart function", {
  ped <- data.frame(
    id = c("A", "B", "C", "F", "G", "J", "K", "E", "D", "I", "H", "L", "M"),
    fid = c(NA, NA, NA, "A", "C", "F", "F", "A", "A", "A", "C", "K", NA),
    mid = c(NA, NA, NA, "B", "B", "G", "G", "B", "B", "B", "B", "H", "H"),
    by = c(0, 0, 0, 1, 1, 2, 2, 1, 1, 1, 2, 3, 3) + 2000,
    dum1 = 13:1,
    pat = c("A", "A", "B", "A", "A", "A", "A", "A", "B", "C", NA, "B", "B"),
    trt1 = c(
      0.56,
      0.04,
      -0.60,
      0.47,
      -0.31,
      0.03,
      0,
      1.00,
      1.00,
      1.00,
      0.10,
      0.50,
      0.00
    ),
    dum2 = (1:13) + 6,
    trt2 = c(0.10, 0.24, -0.30, 0.17, -0.21, 0.13, 0, 0, 0, 0, 0.20, 0, 0.00)
  )

  # ... to test unknown argument
  ped2 <- ped
  ped2$fid <- as.character(ped2$fid)
  ped2[is.na(ped2$fid), "fid"] <- "0"
  ped2$fid <- as.factor(ped2$fid)

  # ... to test recode argument
  ped3 <- ped[order(orderPed(ped = ped[, c("id", "fid", "mid")])), ]
  ped3$idI <- seq_len(nrow(ped3))
  ped3$fidI <- match(ped3$fid, ped3$id)
  ped3$midI <- match(ped3$mid, ped3$id)
  # ... to test recode and unknown argument

  ret <- AlphaPart(
    x = ped[, c("id", "fid", "mid", "pat", "trt1", "trt2")],
    pathNA = TRUE,
    verbose = 0
  )
  ret2 <- AlphaPart(
    x = ped,
    pathNA = TRUE,
    verbose = 0,
    colId = 1,
    colFid = 2,
    colMid = 3,
    colPath = 6,
    colBV = c(7, 9)
  )
  ret3 <- AlphaPart(
    x = ped,
    pathNA = TRUE,
    verbose = 0,
    colId = "id",
    colFid = "fid",
    colMid = "mid",
    colPath = "pat",
    colBV = c("trt1", "trt2")
  )
  ped$idI <- ped$id
  ret3a <- AlphaPart(
    x = ped,
    pathNA = TRUE,
    verbose = 0,
    colId = "idI",
    colFid = "fid",
    colMid = "mid",
    colPath = "pat",
    colBV = c("trt1", "trt2")
  )
  ped$idI <- NULL
  ret4 <- AlphaPart(
    x = ped[, c("id", "fid", "mid", "pat", "trt1", "trt2")],
    pathNA = TRUE,
    verbose = 0,
    colId = "id",
    colFid = "fid",
    colMid = "mid",
    colPath = "pat",
    colBV = c("trt1", "trt2")
  )

  # ... to test recode argument
  ret5 <- AlphaPart(
    x = ped3,
    pathNA = TRUE,
    verbose = 0,
    colId = "idI",
    colFid = "fidI",
    colMid = "midI",
    colPath = "pat",
    colBV = c("trt1", "trt2")
  )
  ret5.1 <- AlphaPart(
    x = ped3,
    pathNA = TRUE,
    verbose = 0,
    colId = "idI",
    colFid = "fidI",
    colMid = "midI",
    colPath = "pat",
    colBV = c("trt1", "trt2")
  )

  ret6 <- AlphaPart(
    x = ped3,
    pathNA = TRUE,
    verbose = 0,
    colId = "idI",
    colFid = "fidI",
    colMid = "midI",
    colPath = "pat",
    colBV = c("trt1", "trt2"),
    recode = FALSE
  )
  ret6.1 <- AlphaPart(
    x = ped3,
    pathNA = TRUE,
    verbose = 0,
    colId = "idI",
    colFid = "fidI",
    colMid = "midI",
    colPath = "pat",
    colBV = c("trt1", "trt2"),
    recode = FALSE
  )

  # ... to test recode and unknown argument
  ret7 <- AlphaPart(
    x = ped3,
    pathNA = TRUE,
    verbose = 0,
    colId = "idI",
    colFid = "fidI",
    colMid = "midI",
    colPath = "pat",
    colBV = c("trt1", "trt2"),
    recode = FALSE,
    unknown = 99
  )
  ret7.1 <- AlphaPart(
    x = ped3,
    pathNA = TRUE,
    verbose = 0,
    colId = "idI",
    colFid = "fidI",
    colMid = "midI",
    colPath = "pat",
    colBV = c("trt1", "trt2"),
    recode = FALSE,
    unknown = 99
  )

  ret8 <- AlphaPart(
    x = ped3,
    pathNA = TRUE,
    verbose = 0,
    colId = "idI",
    colFid = "fidI",
    colMid = "midI",
    colPath = "pat",
    colBV = "trt1",
    recode = FALSE,
    unknown = 99
  )
  ret8.1 <- AlphaPart(
    x = ped3,
    pathNA = TRUE,
    verbose = 0,
    colId = "idI",
    colFid = "fidI",
    colMid = "midI",
    colPath = "pat",
    colBV = 7,
    recode = FALSE,
    unknown = 99
  )

  # --- Overall result ---

  # List component names
  expect_equal(names(ret), c(ret$info$lT, "info"))

  # The same input (so should be the output), but different column definitions
  expect_equal(ret2, ret3)
  ret3a$trt1$"idI" <- NULL
  expect_equal(ret3$trt1, ret3a$trt1)
  expect_equal(ret, ret4)

  # Use of argument recode and unknown
  expect_equal(ret5, ret6)
  expect_equal(ret6, ret7)
  expect_equal(ret5.1, ret6.1)
  expect_equal(ret6.1, ret7.1)
  expect_equal(ret8, ret8.1)

  # Use of recode=FALSE
  tmp <- ret6$trt1[order(ret6$trt1$id), ]
  tmp$idI <- tmp$fidI <- tmp$midI <- NULL
  expect_equal(tmp, ret3$trt1[order(ret3$trt1$id), ])

  # --- Check the meta info component ---

  expect_equal(as.character(ret$info$path), "pat", ignore_attr = FALSE)
  expect_equal(ret$info$nP, 4)
  expect_equal(ret$info$lP, c("A", "B", "C", "UNKNOWN"))
  expect_equal(ret$info$nT, 2)
  expect_equal(ret$info$lT, c("trt1", "trt2"))

  # --- Check partition components ---

  # Make sure we have all individuals for all traits
  expect_equal(nrow(ret$trt1), 13)
  expect_equal(nrow(ret$trt2), 13)

  # Test column names
  expect_equal(
    colnames(ret$trt1),
    c(
      "id",
      "fid",
      "mid",
      "pat",
      "trt1",
      "trt1_pa",
      "trt1_ms",
      "trt1_A",
      "trt1_B",
      "trt1_C",
      "trt1_UNKNOWN"
    )
  )
})

test_that("Test computation", {
  ped <- data.frame(
    id = c("A", "B", "C", "F", "G", "J", "K", "E", "D", "I", "H", "L", "M"),
    fid = c(NA, NA, NA, "A", "C", "F", "F", "A", "A", "A", "C", "K", NA),
    mid = c(NA, NA, NA, "B", "B", "G", "G", "B", "B", "B", "B", "H", "H"),
    by = c(0, 0, 0, 1, 1, 2, 2, 1, 1, 1, 2, 3, 3) + 2000,
    dum1 = 13:1,
    pat = c("A", "A", "B", "A", "A", "A", "A", "A", "B", "C", NA, "B", "B"),
    trt1 = c(
      0.56,
      0.04,
      -0.60,
      0.47,
      -0.31,
      0.03,
      0,
      1.00,
      1.00,
      1.00,
      0.10,
      0.50,
      0.00
    ),
    dum2 = (1:13) + 6,
    trt2 = c(0.10, 0.24, -0.30, 0.17, -0.21, 0.13, 0, 0, 0, 0, 0.20, 0, 0.00)
  )

  ret <- AlphaPart(
    x = ped[, c("id", "fid", "mid", "pat", "trt1", "trt2")],
    pathNA = TRUE,
    verbose = 0
  )

  # --- Check computations ---

  # ret   <- AlphaPart(x=ped[, c("id", "fid", "mid", "pat", "trt1", "trt2")], pathNA=TRUE, verbose=2)
  # Gene flow (T)
  #  [1,] 1.000 .    .     .    .    . .   . . . .   . .
  #  [2,] .     1.00 .     .    .    . .   . . . .   . .
  #  [3,] .     .    1.000 .    .    . .   . . . .   . .
  #  [4,] 0.500 0.50 .     1.00 .    . .   . . . .   . .
  #  [5,] .     0.50 0.500 .    1.00 . .   . . . .   . .
  #  [6,] 0.250 0.50 0.250 0.50 0.50 1 .   . . . .   . .
  #  [7,] 0.250 0.50 0.250 0.50 0.50 . 1.0 . . . .   . .
  #  [8,] 0.500 0.50 .     .    .    . .   1 . . .   . .
  #  [9,] 0.500 0.50 .     .    .    . .   . 1 . .   . .
  # [10,] 0.500 0.50 .     .    .    . .   . . 1 .   . .
  # [11,] .     0.50 0.500 .    .    . .   . . . 1.0 . .
  # [12,] 0.125 0.50 0.375 0.25 0.25 . 0.5 . . . 0.5 1 .
  # [13,] .     0.25 0.250 .    .    . .   . . . 0.5 . 1
  # Gene flow inverse (inv(T))
  # 1   1.0  .    .    .    .   .  .   . . .  .   . .
  # 2   .    1.0  .    .    .   .  .   . . .  .   . .
  # 3   .    .    1.0  .    .   .  .   . . .  .   . .
  # 4  -0.5 -0.5  .    1.0  .   .  .   . . .  .   . .
  # 5   .   -0.5 -0.5  .    1.0 .  .   . . .  .   . .
  # 6   .    .    .   -0.5 -0.5 1  .   . . .  .   . .
  # 7   .    .    .   -0.5 -0.5 .  1.0 . . .  .   . .
  # 8  -0.5 -0.5  .    .    .   .  .   1 . .  .   . .
  # 9  -0.5 -0.5  .    .    .   .  .   . 1 .  .   . .
  # 10 -0.5 -0.5  .    .    .   .  .   . . 1  .   . .
  # 11  .   -0.5 -0.5  .    .   .  .   . . .  1.0 . .
  # 12  .    .    .    .    .   . -0.5 . . . -0.5 1 .
  # 13  .    .    .    .    .   .  .   . . . -0.5 . 1
  # Selected gene flow (TP) for path 1
  #  [1,] 1.000 .    . .    .    . .   . . . . . .
  #  [2,] .     1.00 . .    .    . .   . . . . . .
  #  [3,] .     .    0 .    .    . .   . . . . . .
  #  [4,] 0.500 0.50 . 1.00 .    . .   . . . . . .
  #  [5,] .     0.50 0 .    1.00 . .   . . . . . .
  #  [6,] 0.250 0.50 0 0.50 0.50 1 .   . . . . . .
  #  [7,] 0.250 0.50 0 0.50 0.50 . 1.0 . . . . . .
  #  [8,] 0.500 0.50 . .    .    . .   1 . . . . .
  #  [9,] 0.500 0.50 . .    .    . .   . 0 . . . .
  # [10,] 0.500 0.50 . .    .    . .   . . 0 . . .
  # [11,] .     0.50 0 .    .    . .   . . . 0 . .
  # [12,] 0.125 0.50 0 0.25 0.25 . 0.5 . . . 0 0 .
  # [13,] .     0.25 0 .    .    . .   . . . 0 . 0
  # Selected gene flow (TP) for path 2
  #  [1,] 0 . .     . . . . . . . . . .
  #  [2,] . 0 .     . . . . . . . . . .
  #  [3,] . . 1.000 . . . . . . . . . .
  #  [4,] 0 0 .     0 . . . . . . . . .
  #  [5,] . 0 0.500 . 0 . . . . . . . .
  #  [6,] 0 0 0.250 0 0 0 . . . . . . .
  #  [7,] 0 0 0.250 0 0 . 0 . . . . . .
  #  [8,] 0 0 .     . . . . 0 . . . . .
  #  [9,] 0 0 .     . . . . . 1 . . . .
  # [10,] 0 0 .     . . . . . . 0 . . .
  # [11,] . 0 0.500 . . . . . . . 0 . .
  # [12,] 0 0 0.375 0 0 . 0 . . . 0 1 .
  # [13,] . 0 0.250 . . . . . . . 0 . 1
  # Selected gene flow (TP) for path 3
  #  [1,] 0 . . . . . . . . . . . .
  #  [2,] . 0 . . . . . . . . . . .
  #  [3,] . . 0 . . . . . . . . . .
  #  [4,] 0 0 . 0 . . . . . . . . .
  #  [5,] . 0 0 . 0 . . . . . . . .
  #  [6,] 0 0 0 0 0 0 . . . . . . .
  #  [7,] 0 0 0 0 0 . 0 . . . . . .
  #  [8,] 0 0 . . . . . 0 . . . . .
  #  [9,] 0 0 . . . . . . 0 . . . .
  # [10,] 0 0 . . . . . . . 1 . . .
  # [11,] . 0 0 . . . . . . . 0 . .
  # [12,] 0 0 0 0 0 . 0 . . . 0 0 .
  # [13,] . 0 0 . . . . . . . 0 . 0
  # Selected gene flow (TP) for path 4
  #  [1,] 0 . . . . . . . . . .   . .
  #  [2,] . 0 . . . . . . . . .   . .
  #  [3,] . . 0 . . . . . . . .   . .
  #  [4,] 0 0 . 0 . . . . . . .   . .
  #  [5,] . 0 0 . 0 . . . . . .   . .
  #  [6,] 0 0 0 0 0 0 . . . . .   . .
  #  [7,] 0 0 0 0 0 . 0 . . . .   . .
  #  [8,] 0 0 . . . . . 0 . . .   . .
  #  [9,] 0 0 . . . . . . 0 . .   . .
  # [10,] 0 0 . . . . . . . 0 .   . .
  # [11,] . 0 0 . . . . . . . 1.0 . .
  # [12,] 0 0 0 0 0 . 0 . . . 0.5 0 .
  # [13,] . 0 0 . . . . . . . 0.5 . 0

  # ret$trt1 --> trait 1
  #    id  fid  mid   path  trt1 trt1_pa trt1_ms trt1_A trt1_B trt1_C trt1_UNKNOWN
  # 1   A <NA> <NA>      A  0.56    0.00   0.56   0.56   0.00    0.0    0.000
  # --> base animal from path 1 and all AGV is from path A
  expect_equal(
    as.vector(unlist(ret$trt1[1, -(1:4)])),
    c(0.56, 0, 0.56, 0.56, 0, 0, 0),
    ignore_attr = FALSE
  )

  #    id  fid  mid   path  trt1 trt1_pa trt1_ms trt1_A trt1_B trt1_C trt1_UNKNOWN
  # 2   B <NA> <NA>      A  0.04    0.00   0.04   0.04   0.00    0.0    0.000
  # --> ditto

  #    id  fid  mid   path  trt1 trt1_pa trt1_ms trt1_A trt1_B trt1_C trt1_UNKNOWN
  # 3   C <NA> <NA>      B -0.60    0.00  -0.60   0.00  -0.60    0.0    0.000
  # --> ditto for path B
  expect_equal(
    as.vector(unlist(ret$trt1[3, -(1:4)])),
    c(-0.6, 0, -0.6, 0, -0.6, 0, 0),
    ignore_attr = FALSE
  )

  #    id  fid  mid   path  trt1 trt1_pa trt1_ms trt1_A trt1_B trt1_C trt1_UNKNOWN
  # 4   F    A    B      A  0.47    0.30   0.17   0.47   0.00    0.0    0.000
  # --> ditto for path 1 and both parents are from path A
  expect_equal(
    as.vector(unlist(ret$trt1[4, -(1:4)])),
    c(0.47, 0.3, 0.17, 0.47, 0, 0, 0),
    ignore_attr = FALSE
  )

  #    id  fid  mid   path  trt1 trt1_pa trt1_ms trt1_A trt1_B trt1_C trt1_UNKNOWN
  # 5   G    C    B      A -0.31   -0.28  -0.03  -0.01  -0.30    0.0    0.000
  #
  # id fid mid path
  #  A   0   0    A 1
  #  B   0   0    A 2
  #  C   0   0    B 3
  #  F   A   B    A 4
  #  G   C   B    A 5
  #
  # a_._5 = 1/2(a_3  + a_2)  + w_5
  #       = 1/2(w_3  + w_2)  + w_5 --> this corresponds to T[5,] .    0.5    0.50 .   1.0 . . . . .
  #       = 1/2(-0.6 + 0.04) + -0.03
  #
  # a_1_5 = T_5 P_1_5 w
  #     T[5,] .    0.5    0.50 .   1.0 . . . . .
  #    TP[5,] .    0.5       . .   1.0 . . . . . --> selects genes from path 1 - parent 1 genes and Mendelian sampling (parent 3 is from path 2!!!)
  #  TInv[5,] .   -0.5   -0.50 .   1.0 . . . . . --> when computing Mendelian sampling we substract parent average -1/2(a_f(i) + a_m(i))
  #       = 1/2w_2 + w_5
  #       = 1/2*0.04 + -0.03
  #       = -0.01
  #
  # a_2_5 = T_5 P_2_5 w
  #     T[5,] .    0.5    0.50 .   1.0 . . . . .
  #    TP[5,] .    0.0    0.50 .     0 . . . . . --> selects genes from path 2 (parent 2 and animals is from path 1!!!)
  #
  #       = 1/2w_3
  #       = 1/2*-0.60
  #       = -0.30
  expect_equal(
    as.vector(unlist(ret$trt1[5, -(1:4)])),
    c(-0.31, -0.28, -0.03, -0.01, -0.3, 0, 0),
    ignore_attr = FALSE
  )

  #    id  fid  mid   path  trt1 trt1_pa trt1_ms trt1_A trt1_B trt1_C trt1_UNKNOWN
  # 6   J    F    G      A  0.03    0.08  -0.05   0.18  -0.15    0.0    0.000
  #
  # id fid mid path
  #  A   0   0    1 1
  #  B   0   0    1 2
  #  C   0   0    2 3
  #  F   A   B    1 4
  #  G   3   B    1 5
  #  J   F   G    1 6
  #
  # a_._6 = 1/2(a_4  + a_5)  + w_6
  #       = 1/2(1/2a_1 + 1/2a_2 + w_4) + 1/2(1/2a_3  + 1/2a_2 + w_5) + w_6
  #       = 1/4w_1 + 1/4w_2 + 1/2w_4 + 1/4w_3 + 1/4w_2 + 1/2w_5 + w_6
  #       = 1/4w_1 + 1/2w_2 + 1/4w_3 + 1/2w_4 + 1/2w_5 + w_6 --> this corresponds to T[6,] 0.25 0.5 0.25 0.5 0.5 1 . . . .
  #       = 0.03
  #
  # a_1_6 = T_6 P_1_6 w
  #              1   2    3   4   5 6
  #     T[6,] 0.25 0.5 0.25 0.5 0.5 1 . . . .
  #    TP[6,] 0.25 0.5 0    0.5 0.5 1 . . . . --> selects genes from path 1 - Mendelian sampling (6) and both parents (4,5) as well as three grandparents (1, 2x2)
  #       = 1/4w_1 + 1/2w_2 + 0w_3 + 1/2w_4 + 1/2w_5 + w_6
  #       = 0.18
  # ...
  expect_equal(
    as.vector(unlist(ret$trt1[6, -(1:4)])),
    c(0.03, 0.08, -0.05, 0.18, -0.15, 0, 0),
    ignore_attr = FALSE
  )

  #    id  fid  mid   path  trt1 trt1_pa trt1_ms trt1_A trt1_B trt1_C trt1_UNKNOWN
  # 7   K    F    G      A     0    0.08  -0.08   0.15  -0.15     0        0
  expect_equal(
    as.vector(unlist(ret$trt1[7, -(1:4)])),
    as.numeric(c(0, 0.08, -0.08, 0.15, -0.15, 0, 0)),
    ignore_attr = FALSE
  )

  #    id  fid  mid   path  trt1 trt1_pa trt1_ms trt1_A trt1_B trt1_C trt1_UNKNOWN
  # 8   E    A    B      A  1.00    0.30   0.70   1.00   0.00    0.0    0.000
  # --> reference for test animals, both parents path 1 and animal path 1 = all OK
  expect_equal(
    as.vector(unlist(ret$trt1[8, -(1:4)])),
    c(1, 0.3, 0.7, 1, 0, 0, 0),
    ignore_attr = FALSE
  )

  #    id  fid  mid   path  trt1 trt1_pa trt1_ms trt1_A trt1_B trt1_C trt1_UNKNOWN
  # 9   D    A    B      B  1.00    0.30   0.70   0.30   0.70    0.0    0.000
  # --> the same parents as for 8, but different partitioning, due to different claimed path - path 2 was doing "selection" of Mendelian sampling!!!
  expect_equal(
    as.vector(unlist(ret$trt1[9, -(1:4)])),
    c(1, 0.3, 0.7, 0.3, 0.7, 0, 0),
    ignore_attr = FALSE
  )

  #    id  fid  mid   path  trt1 trt1_pa trt1_ms trt1_A trt1_B trt1_C trt1_UNKNOWN
  # 10  I    A    B      C  1.00    0.30   0.70   0.30   0.00    0.7    0.000
  # --> ditto
  expect_equal(
    as.vector(unlist(ret$trt1[10, -(1:4)])),
    c(1, 0.3, 0.7, 0.3, 0, 0.7, 0),
    ignore_attr = FALSE
  )

  #    id  fid  mid    path  trt1 trt1_pa trt1_ms trt1_A trt1_B trt1_C trt1_UNKNOWN
  # 11  H    C    B UNKNOWN  0.10  -0.065  0.165  0.235  -0.30    0.0    0.165
  # --> argument pathNA=TRUE sets unknown path to dummy path called UNKNOWN
  expect_equal(
    as.vector(unlist(ret$trt1[11, -(1:4)])),
    c(0.1, -0.28, 0.38, 0.02, -0.3, 0, 0.38),
    ignore_attr = FALSE
  )

  #    id  fid  mid   path  trt1 trt1_pa trt1_ms trt1_A trt1_B trt1_C trt1_UNKNOWN
  # 12  L    K    H      B  0.50    0.05   0.45  0.085  0.225    0.0     0.19
  expect_equal(
    as.vector(unlist(ret$trt1[12, -(1:4)])),
    c(0.5, 0.05, 0.45, 0.085, 0.225, 0.0, 0.19),
    ignore_attr = FALSE
  )

  #    id  fid  mid   path  trt1 trt1_pa trt1_ms trt1_A trt1_B trt1_C trt1_UNKNOWN
  # 13  M   NA    H      B  0.00    0.05  -0.05   0.01  -0.20      0     0.19
  #
  # id fid mid    path
  #  A   0   0       A 1
  #  B   0   0       A 2
  #  C   0   0       B 3
  #  H   C   B UNKNOWN 11
  #  M   0   H       B 13
  #
  # a_._13 = 1/2(0 + a_11)                   + w_13
  #        = 1/2(0 + 1/2a_3 + 1/2a_2 + w_11) + w_13 --> this corresponds to T[13,]
  #        = 1/2*(0 + -0.3   + 0.02   + 0.38) + -0.05
  #        = 1/2*0.1 + -0.05
  #        = 0
  #
  # a_1_5 = T_5 P_1_5 w
  #     T[13,] .     0.25 0.250 .    .    . .   . . .  0.5 . 1
  #    TP[13,] .     0.25 0     .    .    . .   . . .  0   . 0 --> selects genes from path 1
  #  TInv[13,] .     .    .     .    .    . .   . . . -0.5 . 1 --> when computing Mendelian sampling we substract parent average -1/2(a_f(i) + a_m(i))
  #       = 1/4*w_2
  #       = 1/2*0.04
  #       = 0.01
  #
  # ...
  expect_equal(
    as.vector(unlist(ret$trt1[13, -(1:4)])),
    c(0, 0.05, -0.05, 0.01, -0.20, 0, 0.19),
    ignore_attr = FALSE
  )
})

test_that("Test computation - 2nd example", {
  # dput(AlphaPart.ped) so we have a fixed dataset
  dat <- structure(
    list(
      id = structure(
        c(1L, 2L, 3L, 6L, 4L, 5L, 7L, 8L),
        levels = c("A", "B", "C", "D", "E", "T", "U", "V"),
        class = "factor"
      ),
      father = structure(
        c(1L, 1L, 2L, 2L, 1L, 3L, 3L, 4L),
        levels = c("", "B", "D", "E"),
        class = "factor"
      ),
      mother = structure(
        c(1L, 1L, 2L, 1L, 1L, 3L, 1L, 1L),
        levels = c("", "A", "C"),
        class = "factor"
      ),
      generation = c(1, 1, 2, 2, 2, 3, 3, 4),
      country = structure(
        c(1L, 2L, 1L, 2L, 2L, 1L, 2L, 1L),
        levels = c("domestic", "import"),
        class = "factor"
      ),
      sex = structure(
        c(1L, 2L, 1L, 1L, 2L, 2L, 1L, 1L),
        levels = c("F", "M"),
        class = "factor"
      ),
      trait1 = c(100, 105, 104, 102, 108, 107, 107, 109),
      trait2 = c(88, 110, 100, 97, 101, 80, 102, 105)
    ),
    row.names = c(NA, -8L),
    class = "data.frame"
  )
  part <- AlphaPart(x = dat, colPath = "country", colBV = "trait1", verbose = 0)

  # We worked these out in the intro vignette - domestic is blue and import is red
  # a_A & = 0 + \color{blue}{r_A} \\
  #    & = \color{blue}{100} = 100 \\
  expect_true(part$trait1$trait1_domestic[part$trait1$id == "A"] == 100)
  expect_true(part$trait1$trait1_import[part$trait1$id == "A"] == 0)

  # a_B & = 0 + \color{red}{r_B} \\
  #     & = \color{red}{105} = 105 \\
  expect_true(part$trait1$trait1_domestic[part$trait1$id == "B"] == 0)
  expect_true(part$trait1$trait1_import[part$trait1$id == "B"] == 105)

  # a_C & = \color{red}{\frac{1}{2} r_B} +
  #         \color{blue}{\frac{1}{2} r_A} +
  #         \color{blue}{r_C} \\
  #      & = \color{red}{\frac{1}{2} 105} +
  #          \color{blue}{\frac{1}{2} 100} +
  #          \color{blue}{1.5} \\
  #      & = \color{red}{52.5} + \color{blue}{50 + 1.5} \\
  #      & = \color{red}{52.5} + \color{blue}{51.5} = 104 \\
  expect_true(part$trait1$trait1_domestic[part$trait1$id == "C"] == 51.5)
  expect_true(part$trait1$trait1_import[part$trait1$id == "C"] == 52.5)

  # a_T & = \color{red}{\frac{1}{2} r_B} +
  #         \color{red}{r_T} \\
  #     & = \color{red}{\frac{1}{2} 105} +
  #         \color{red}{49.5} \\
  #     & = \color{red}{52.5 + 49.5} \\
  #     & = \color{red}{102} = 102 \\
  expect_true(part$trait1$trait1_domestic[part$trait1$id == "T"] == 0)
  expect_true(part$trait1$trait1_import[part$trait1$id == "T"] == 102)

  # a_D & = 0 + \color{red}{r_D} \\
  #    & = \color{red}{108} = 108 \\
  expect_true(part$trait1$trait1_domestic[part$trait1$id == "D"] == 0)
  expect_true(part$trait1$trait1_import[part$trait1$id == "D"] == 108)

  # a_E & = \color{red}{\frac{1}{2} r_D} +
  #         \color{red}{\frac{1}{4} r_B} +
  #         \color{blue}{\frac{1}{4} r_A} +
  #         \color{blue}{\frac{1}{2} r_C} +
  #         \color{blue}{r_E} \\
  #     & = \color{red}{\frac{1}{2} 108} +
  #         \color{red}{\frac{1}{4} 105} +
  #         \color{blue}{\frac{1}{4} 100} +
  #         \color{blue}{\frac{1}{2} 1.5} +
  #         \color{blue}{1.0} \\
  #     & = \color{red}{54 + 26.25} + \color{blue}{25 + 0.75 + 1} \\
  #     & = \color{red}{80.25} + \color{blue}{26.75} = 107 \\
  expect_true(part$trait1$trait1_domestic[part$trait1$id == "E"] == 26.75)
  expect_true(part$trait1$trait1_import[part$trait1$id == "E"] == 80.25)

  # a_U & = \color{red}{\frac{1}{2} r_D} +
  #         \color{red}{r_U} \\
  #     & = \color{red}{\frac{1}{2} 108} +
  #         \color{red}{53.0} \\
  #     & = \color{red}{54 + 53} \\
  #     & = \color{red}{107} = 107 \\
  expect_true(part$trait1$trait1_domestic[part$trait1$id == "U"] == 0)
  expect_true(part$trait1$trait1_import[part$trait1$id == "U"] == 107)

  # a_V & = \color{red}{\frac{1}{4} r_D} +
  #         \color{red}{\frac{1}{8} r_B} +
  #         \color{blue}{\frac{1}{8} r_A} +
  #         \color{blue}{\frac{1}{4} r_C} +
  #         \color{blue}{\frac{1}{2} r_E} +
  #         \color{blue}{r_V} \\
  #     & = \color{red}{\frac{1}{4} 108} +
  #         \color{red}{\frac{1}{8} 105} +
  #         \color{blue}{\frac{1}{8} 100} +
  #         \color{blue}{\frac{1}{4} 1.5} +
  #         \color{blue}{\frac{1}{2} 1.0} +
  #         \color{blue}{55.5} \\
  #     & = \color{red}{27 + 13.125} + \color{blue}{12.5 + 0.375 + 0.5 + 55.5} \\
  #     & = \color{red}{40.125} + \color{blue}{68.875} = 109
  expect_true(part$trait1$trait1_domestic[part$trait1$id == "V"] == 68.875)
  expect_true(part$trait1$trait1_import[part$trait1$id == "V"] == 40.125)
})
