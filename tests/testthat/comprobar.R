context("comprobar la extension del archivo a cargar")

test_that("Probando a comprobar()", {
  expect_that(comprobar(".csv", "VIH.csv"), equals(TRUE))
  expect_that(comprobar(".csv", "VIH.pdf"), equals(FALSE))
  
})

