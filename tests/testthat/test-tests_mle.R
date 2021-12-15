test_that("tests_mle", {
  set.seed(1337)

  ## sampling MLE instance example (small)
  expect_error(sample_mle(M=2), NA)

  ## short tests with sample_auc
  sample_auc(grd=1000) %>% study_mle(analysis = "acc", M=20, methods="grid")
  sample_auc(grd=1000) %>% study_mle(analysis = "cpe", M=20, methods="grid")

  ## study with MLE example instance (full)
  expect_error(example_mle <- SEPM.SIM:::example_mle, NA)
  expect_error(example_mle %>% study_mle(analysis = "acc"), NA)
  expect_error(example_mle %>% study_mle(analysis = "cpe"), NA)

  # TODO all design/algo parameter combinations
  # TODO: example_mle: other name??? :: VS :::

  example_mle %>% study_mle(methods = "glmnet", M = 50, analysis = "acc")

  example_mle %>% study_mle(M = 40, analysis = "acc", select.method = "close")
  example_mle %>% study_mle(M = 40, analysis = "acc", select.method = "best")
  example_mle %>% study_mle(M = 40, analysis = "acc", select.method = "optimal")
  example_mle %>% study_mle(M = 40, analysis = "acc", select.method = "oracle", select.args="type=train.acc")
  example_mle %>% study_mle(M = 40, analysis = "acc", select.method = "oracle", select.args="type=learn.acc")
  example_mle %>% study_mle(M = 40, analysis = "acc", select.method = "simplest.en")

  example_mle %>% study_mle(M = 40, analysis = "cpe", select.method = "close")
  example_mle %>% study_mle(M = 40, analysis = "cpe", select.method = "best")
  example_mle %>% study_mle(M = 40, analysis = "cpe", select.method = "optimal")
  example_mle %>% study_mle(M = 40, analysis = "cpe", select.method = "oracle", select.args="type=train.cpe")
  example_mle %>% study_mle(M = 40, analysis = "cpe", select.method = "oracle", select.args="type=learn.cpe")
  example_mle %>% study_mle(M = 40, analysis = "cpe", select.method = "simplest.en")

  example_mle %>% study_mle(M = 40, analysis = "acc", select.method = "close", select.limit = "none")
  example_mle %>% study_mle(M = 40, analysis = "acc", select.method = "close", select.limit = "sqrt")
  example_mle %>% study_mle(M = 40, analysis = "acc", select.method = "close", select.limit = "one")

  example_mle %>% study_mle(M = 40, analysis = "cpe", select.method = "close", transform = "logit")

})



