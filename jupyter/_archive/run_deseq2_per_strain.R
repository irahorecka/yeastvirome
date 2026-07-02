#!/usr/bin/env Rscript

# run_deseq2_per_strain.R
# For each strain in the counts matrix, run DESeq2: that strain vs all others.

suppressPackageStartupMessages({
  library(DESeq2)
  library(readr)
  library(dplyr)
  library(tibble)
  library(apeglm)
})

# ---- Helper ----
detect_delim <- function(path) {
  if (grepl("\\.(tsv|tab)(\\.gz)?$", path, ignore.case = TRUE)) return("\t")
  if (grepl("\\.csv(\\.gz)?$",  path, ignore.case = TRUE)) return(",")
  con <- file(path, "r"); on.exit(close(con))
  line <- readLines(con, n = 1L)
  if (length(gregexpr("\t", line)[[1]]) > 1) return("\t") else return(",")
}

# ---- Args ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: run_deseq2_per_strain.R <count_matrix.csv.gz> [out_dir]")
}
counts_csv <- args[1]
out_dir    <- if (length(args) >= 2) args[2] else "data/DEG_per_strain"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

message("[i] Counts file: ", counts_csv)
message("[i] Output dir: ", out_dir)

# ---- Load counts ----
counts_df <- readr::read_delim(counts_csv, delim = detect_delim(counts_csv), show_col_types = FALSE)
gene_col  <- names(counts_df)[1]
counts    <- counts_df %>% column_to_rownames(gene_col) %>% as.data.frame()
counts[]  <- lapply(counts, function(x) as.integer(round(x)))
if (any(is.na(counts))) stop("NA in counts after coercion—check input.")

strains <- colnames(counts)
message("[i] Found ", length(strains), " strains.")

# ---- Per-strain DE ----
alpha    <- 0.05
lfc_cut  <- 0.3
min_ct   <- 10
min_frac <- 0.05

summary_list <- list()

for (target_strain in strains) {
  message("[*] Running DE for: ", target_strain)

  # Binary factor: Target vs Rest
  coldata <- data.frame(Strain = strains, Group = ifelse(strains == target_strain, "Target", "Rest"))
  rownames(coldata) <- coldata$Strain
  coldata$Group <- relevel(factor(coldata$Group), ref = "Rest")

  # Filter low counts
  keep <- rowSums(counts >= min_ct) >= max(10, floor(min_frac * ncol(counts)))
  counts_f <- counts[keep, , drop=FALSE]
  if (nrow(counts_f) == 0) {
    warning("No genes left after filtering for strain ", target_strain)
    next
  }

  # DESeq2
  dds <- DESeqDataSetFromMatrix(countData = counts_f, colData = coldata, design = ~ Group)
  dds <- DESeq(dds, parallel = FALSE)

  res <- results(dds, contrast = c("Group", "Target", "Rest"))
  res_shrunk <- lfcShrink(dds, coef = resultsNames(dds)[grep("Group", resultsNames(dds))[1]], type = "apeglm")

  out <- as.data.frame(res_shrunk) %>%
    rownames_to_column("gene") %>%
    arrange(padj, desc(abs(log2FoldChange))) %>%
    mutate(sig = !is.na(padj) & padj < alpha & abs(log2FoldChange) > lfc_cut)

  out_file <- file.path(out_dir, sprintf("DEG_%s_vs_all.csv", target_strain))
  write_csv(out, file = out_file)

  summary_list[[target_strain]] <- data.frame(
    strain = target_strain,
    n_sig  = sum(out$sig, na.rm=TRUE)
  )
}

# ---- Summary ----
summary_df <- do.call(rbind, summary_list)
write_tsv(summary_df, file.path(out_dir, "_summary.tsv"))
message("[✓] Done. Summary saved to _summary.tsv")
