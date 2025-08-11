#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(DESeq2)
  library(readr)
  library(dplyr)
  library(tibble)
  library(stringr)
  library(apeglm)
})

# ---- Args ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) {
  stop("Usage: run_deseq2.R <count_matrix.csv.gz> <sample_metadata.csv> <target_subpop>")
}

counts_csv <- args[1]
meta_csv   <- args[2]
target_subpop <- args[3]  # e.g., "Japan"

message("[i] Counts file: ", counts_csv)
message("[i] Metadata file: ", meta_csv)
message("[i] Target subpopulation: ", target_subpop)

# ---- Load counts ----
counts_df <- read_csv(counts_csv)
gene_col  <- names(counts_df)[1]
counts    <- counts_df %>% column_to_rownames(gene_col) %>% as.data.frame()
counts[]  <- lapply(counts, function(x) as.integer(round(x)))
if (any(is.na(counts))) stop("NA in counts after coercion—check input.")

# ---- Load metadata ----
coldata <- read_csv(meta_csv) %>% mutate(Strain = as.character(Strain))
missing_meta <- setdiff(colnames(counts), coldata$Strain)
if (length(missing_meta)) stop("Missing from metadata: ", paste(missing_meta, collapse=", "))

coldata <- coldata %>% filter(Strain %in% colnames(counts)) %>% distinct(Strain, .keep_all = TRUE)
rownames(coldata) <- coldata$Strain
coldata <- coldata[colnames(counts), , drop=FALSE]

# Binary factor: Japan vs Rest
coldata$Japan_vs_Rest <- ifelse(coldata$Subpop == target_subpop, "Japan", "Rest")
coldata$Japan_vs_Rest <- relevel(factor(coldata$Japan_vs_Rest), ref = "Rest")

use_mating <- "MatingType" %in% colnames(coldata)
if (use_mating) coldata$MatingType <- factor(coldata$MatingType)

# ---- Filter low counts ----
keep <- rowSums(counts >= 10) >= max(10, floor(0.05 * ncol(counts)))
counts_f <- counts[keep, , drop=FALSE]
message("[i] Kept ", nrow(counts_f), " genes out of ", nrow(counts))

# ---- DESeq2 ----
design_formula <- if (use_mating) ~ MatingType + Japan_vs_Rest else ~ Japan_vs_Rest
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

dir.create("data", showWarnings = FALSE)

write_csv(out, file = sprintf("data/DEG_%s_vs_Rest.csv", target_subpop))
norm_counts <- counts(dds, normalized = TRUE) %>% as.data.frame() %>% rownames_to_column("gene")
write_csv(norm_counts, "data/normalized_counts.csv")
vsd <- vst(dds, blind = TRUE)
vsd_mat <- assay(vsd) %>% as.data.frame() %>% rownames_to_column("gene")
write_csv(vsd_mat, "data/vst_matrix.csv")

message("[✓] DEG analysis complete. Significant genes: ", sum(out$sig, na.rm=TRUE))