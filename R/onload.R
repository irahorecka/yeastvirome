# File: R/onload.R
# Consolidated .onLoad() hook

.onLoad <- function(libname, pkgname) {
  try({
    # Load phenome layout
    layout_tbl <- read_phenome_experiment_layout()
    assign("phenome.layout", layout_tbl, envir = asNamespace(pkgname))
    assign("phenome.layout", layout_tbl, envir = parent.env(environment()))
  }, silent = TRUE)

  try({
    # Load phenotype matrix
    pheno_tbl <- read_phenotype_matrix()
    assign("phenome.df", pheno_tbl, envir = asNamespace(pkgname))
    assign("phenome.df", pheno_tbl, envir = parent.env(environment()))
  }, silent = TRUE)

  try({
    # Load RNAseq accessions
    acc_tbl <- read_rnaseq_accessions()
    assign("rnaseq.accessions", acc_tbl, envir = asNamespace(pkgname))
    assign("rnaseq.accessions", acc_tbl, envir = parent.env(environment()))
  }, silent = TRUE)

  try({
    # Load Virome RPM data
    virome_tbl <- read_virome.df()
    assign("virome.df", virome_tbl, envir = asNamespace(pkgname))
    assign("virome.df", virome_tbl, envir = parent.env(environment()))
  }, silent = TRUE)

  invisible()
}

