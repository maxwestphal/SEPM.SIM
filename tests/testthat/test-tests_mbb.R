test_that("tests_mbb", {
  expect_error(instance_mbb <- sample_mbb_acc(), NA)
  expect_error(instance_mbb %>% study_mbb_acc(method="sample"), NA)
  expect_error(instance_mbb %>% study_mbb_acc(method="copula"), NA)
  expect_error(instance_mbb %>% study_mbb_acc(method="approx"), NA)
  expect_error(instance_mbb %>% study_mbb_acc(method="approx", prior="vague"), NA)
  expect_error(instance_mbb %>% study_mbb_acc(method="approx", prior="correct"), NA)
  expect_error(instance_mbb %>% study_mbb_acc(method="approx", prior="liberal"), NA)
  expect_error(instance_mbb %>% study_mbb_acc(method="approx", contrast = "rdm"), NA)
  expect_error(sample_mbb_acc(nrep=2) %>% study_mbb_acc(), NA)
})

