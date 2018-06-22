context("Probando el guardado del archivo")

test_that("filename()",{
  
  expect_that(filename(),equals("reporte.docx"))
})