test_that("as.data.frame(Dados) não está classificado como somente data.frame", {
  expect_s3_class(as.data.frame(Dados), "data.frame")
  #warning("O banco de dados precisa estar em data.frame")
})
