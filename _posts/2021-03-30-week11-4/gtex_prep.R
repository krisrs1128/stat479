# gtex preparation

library("data.table")
library("dplyr")
library("tidyr")
library("readr")
library("tibble")

x <- fread("~/Downloads/GTEx_Analysis_v6_RNA-seq_RNA-SeQCv1.1.8_gene_reads.gct", skip = 2, sep = "\t", nrow = 2000) %>%
  as_tibble() %>%
  pivot_longer(starts_with("GTEX"), names_to = "SAMPID")
metadata <- read_tsv("~/Downloads/GTEx_Data_V6_Annotations_SampleAttributesDS.txt") %>%
  select(SAMPID, SMTS, SMTSD) %>%
  rename(tissue = SMTS, tissue_detail = SMTSD)

head(x)
x_filter <- x %>%
  group_by(Name) %>%
  mutate(sd_gene = sd(value)) %>%
  group_by(SAMPID) %>%
  mutate(sd_sample = sd(value)) %>%
  slice_max(sd_gene, n = 2000) %>%
  group_by(Name) %>%
  slice_max(sd_sample, n = 1000) %>%
  left_join(metadata) %>%
  select(SAMPID, Name, starts_with("tissue"), Description, value) %>%
  rename(sample = SAMPID, gene = Name)

write_csv(x_filter, "~/Desktop/gtex.csv")