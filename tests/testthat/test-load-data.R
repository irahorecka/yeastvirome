test_that("load_yeast_virome returns list of tibbles", {
  dl <- load_yeast_virome()
  expect_type(dl, "list")
  expect_true(all(purrr::map_lgl(dl, tibble::is_tibble)))
})
