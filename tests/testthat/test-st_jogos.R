test_that("multiplication works", {
  expect_s3_class(st_jogos("Action", "PS2", 2000, 2010),
                  c('highchart','htmlwidget'))
})
