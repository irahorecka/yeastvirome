# The RNA Virome of Yeast

Explore the RNA virome of the 1,011-strain *Saccharomyces cerevisiae* collection.

---

## `yeastvirome`

- Structured virome data for the 1000 yeast strains
- Access to phenotype, virome, ecological, and geographic metadata
- Preloaded tibbles and helper functions to simplify exploration
- Utilities for virome summarization, visualizationM, and statistics

---

## Installation

```r
# install.packages("devtools")
devtools::install_github("ababaian/yeastvirome")
```

---

## Included Data Objects

| Object              | Description                          |
|---------------------|--------------------------------------|
| `phenotypes.1011`   | ~223 phenotypes for 1,011 strains    |
| `phenome.layout`    | Describes each phenotype experiment  |
| `rnaseq.accessions` | RNA-seq accessions                   |
| `virome.df`         | Contig-level virome matrix (RPM)     |
| `virometa.df`       | Viral contig metadata                |
| `geodata.df`        | Geographic metadata                  |
| `ecodata.df`        | Ecological metadata                  |


## Development

This package is under active development :)