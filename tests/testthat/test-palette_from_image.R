test_that("testing palfromimg", {
  pal_test <- palfromimg(imgpath = "./ipcc10.jpg",
                         coln = 10,
                         alpha = 1,
                         seed = 1234,
                         resize = TRUE,
                         sort = 1)
  pal_test2 <- palfromimg(imgpath = "./ipcc10.jpg",
                         coln = 10,
                         alpha = 1,
                         seed = 1234,
                         sort = 1,
                         onlycolour = TRUE)
  pal <- pal_test$pal
  pal_code <- c("#052E5FFF","#2164AAFF",
                "#4591C2FF", "#66011CFF",
                "#90C5DFFF", "#9BC2D7FF",
                "#AE172AFF", "#D35F4DFF",
                "#E6DFDAFF", "#F3A484FF")
  expect_is(pal_test, "list")
  expect_equal(length(pal(10)), 10)
  expect_equal(pal(10), pal_code)
  expect_equal(pal_test2, pal_code)
})
