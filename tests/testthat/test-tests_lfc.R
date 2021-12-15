test_that("tests_lfc", {
  expect_error(sample_lfc_acc() %>% study_lfc_acc(), NA)
  expect_error(sample_lfc_cpe() %>% study_lfc_cpe(), NA)
})
