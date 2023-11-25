test_that("Algumas das entradas estão incorretas", {
  expect_s3_class(Graf_barras("Genre", "EU_Sales"), c('highchart','htmlwidget'))
})
