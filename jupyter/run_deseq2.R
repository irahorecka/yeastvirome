#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(DESeq2)
  library(readr)
  library(dplyr)
  library(tibble)
  library(stringr)
  library(apeglm)
})

# ---- IO helpers ----
detect_delim <- function(path) {
  if (grepl("\\.(tsv|tab)(\\.gz)?$", path, ignore.case = TRUE)) return("\t")
  if (grepl("\\.csv(\\.gz)?$",  path, ignore.case = TRUE)) return(",")
  # fallback: sniff first line
  con <- file(path, "r"); on.exit(close(con))
  line <- readLines(con, n = 1L)
  if (length(gregexpr("\t", line)[[1]]) > 1) return("\t") else return(",")
}

 # ---- Args ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("Usage: run_deseq2.R <count_matrix.csv.gz> <sample_metadata.csv> <target_subpop> [out_dir]")
}
counts_csv    <- args[1]
meta_csv      <- args[2]
target_subpop <- args[3]  # e.g., a virus label or population name
out_dir       <- if (length(args) >= 4) args[4] else "data/DEG"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message("[i] Counts file: ", counts_csv)
message("[i] Metadata file: ", meta_csv)
message("[i] Target subpopulation: ", target_subpop)
message("[i] Output dir: ", out_dir)

# ---- Load counts ----
counts_df <- readr::read_delim(counts_csv, delim = detect_delim(counts_csv), show_col_types = FALSE)
gene_col  <- names(counts_df)[1]
counts    <- counts_df %>% column_to_rownames(gene_col) %>% as.data.frame()
counts[]  <- lapply(counts, function(x) as.integer(round(x)))
if (any(is.na(counts))) stop("NA in counts after coercion—check input.")

# ---- Load metadata ----
coldata <- readr::read_delim(meta_csv, delim = detect_delim(meta_csv), show_col_types = FALSE)
if (!all(c("Strain","Subpop") %in% colnames(coldata))) {
  stop("[error] sample metadata must have columns: Strain, Subpop (got: ", paste(colnames(coldata), collapse=", "), ")")
}
coldata <- coldata %>% mutate(Strain = as.character(Strain), Subpop = as.character(Subpop))

# ---- Harmonize and auto-fill metadata ----
# Trim keys to avoid invisible mismatches
colnames(counts) <- stringr::str_trim(colnames(counts))
coldata <- coldata %>%
  mutate(Strain = stringr::str_trim(as.character(Strain))) %>%
  distinct(Strain, .keep_all = TRUE)

# Add any missing strains from counts as "Rest"
missing <- setdiff(colnames(counts), coldata$Strain)
if (length(missing)) {
  message("[warn] Adding ", length(missing), " missing strains to metadata as Rest: ",
          paste(utils::head(missing, 10), collapse = ", "),
          if (length(missing) > 10) " ..." else "")
  coldata <- dplyr::bind_rows(
    coldata,
    tibble::tibble(Strain = missing, Subpop = "Rest")
  )
}

# Now align order exactly to counts columns
coldata <- dplyr::filter(coldata, Strain %in% colnames(counts))
rownames(coldata) <- coldata$Strain
coldata <- coldata[colnames(counts), , drop = FALSE]
missing_meta <- setdiff(colnames(counts), coldata$Strain)
if (length(missing_meta)) stop("Missing from metadata: ", paste(missing_meta, collapse=", "))

# Binary factor: Japan vs Rest
coldata$Japan_vs_Rest <- ifelse(coldata$Subpop == target_subpop, "Japan", "Rest")
coldata$Japan_vs_Rest <- relevel(factor(coldata$Japan_vs_Rest), ref = "Rest")

use_mating <- "MatingType" %in% colnames(coldata)
if (use_mating) coldata$MatingType <- factor(coldata$MatingType)

# ---- Filter low counts ----
keep <- rowSums(counts >= 10) >= max(10, floor(0.05 * ncol(counts)))
counts_f <- counts[keep, , drop=FALSE]
message("[i] Kept ", nrow(counts_f), " genes out of ", nrow(counts))

if (nrow(counts_f) == 0) {
  stop("[error] No genes left after filtering. Consider lowering the filter threshold or inspect input counts.")
}

 # ---- DESeq2 ----
design_formula <- if (use_mating) ~ MatingType + Japan_vs_Rest else ~ Japan_vs_Rest
if (length(unique(coldata$Japan_vs_Rest)) < 2) {
  stop("[error] Design has a single class for Japan_vs_Rest. Check that target_subpop matches Subpop values in metadata.")
}
dds <- DESeqDataSetFromMatrix(countData = counts_f, colData = coldata, design = design_formula)
dds <- DESeq(dds, parallel = FALSE)

res <- results(dds, contrast = c("Japan_vs_Rest", "Japan", "Rest"))
res_shrunk <- lfcShrink(dds, coef = resultsNames(dds)[grep("Japan_vs_Rest", resultsNames(dds))[1]], type = "apeglm")

 # ---- Save outputs ----
alpha <- 0.05
lfc_cut <- 0.3
out <- as.data.frame(res_shrunk) %>%
  rownames_to_column("gene") %>%
  arrange(padj, desc(abs(log2FoldChange))) %>%
  mutate(sig = !is.na(padj) & padj < alpha & abs(log2FoldChange) > lfc_cut)

out_file <- file.path(out_dir, sprintf("DEG_%s_vs_Rest.csv", target_subpop))
write_csv(out, file = out_file)

message("[✓] DEG analysis complete. Significant genes: ", sum(out$sig, na.rm=TRUE))
