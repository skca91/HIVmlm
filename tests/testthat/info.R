context("read.csv")

archivo <- "../HIVmlm/VEN_adm2.csv"

test_that("read.csv()", {
  test_data <- read.csv(file = archivo ,header = TRUE, sep = ",")
  test_data2 <- read.csv("../HIVmlm/VEN_adm2.csv", header = TRUE,sep = ",")
  expect_equal(test_data, test_data2)
  
})
