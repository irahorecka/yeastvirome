test_that("virome summarisation helpers work on toy data", {
  virome <- tibble::tibble(
    std_name = c("S1", "S2"),
    contig1  = c(10, 0),
    contig2  = c(0,  5)
  )

  vmeta <- tibble::tibble(
    import_label = c("contig1", "contig2"),
    species      = c("virusA", "virusB"),
    family       = c("Fam1",   "Fam1")
  )

  species_wide <- summarise_virome_species(virome, vmeta)
  family_wide  <- summarise_virome_family(virome, vmeta)
  total_tbl    <- summarise_virome_total(virome)

  expect_equal(colnames(species_wide),
               c("std_name", "virusA", "virusB"))
  expect_equal(colnames(family_wide),
               c("std_name", "Fam1"))
  expect_true(all(total_tbl$total_virome == c(10, 5)))
})
