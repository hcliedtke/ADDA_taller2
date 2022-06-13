res$bD_bV %>%
  as_tibble(rownames = "gene_id") %>%
  drop_na(padj) %>% # drop all genes with NAs
  #filter(padj<0.99) %>% # reduce the number of points that need to be plotted
  mutate(sig= padj<0.05 & abs(log2FoldChange)>=2) %>% # make a variable to indicate if a gene is significant based on a specific thresholds
  mutate(sig2= padj<0.001 & abs(log2FoldChange)>=2) %>% # make a variable to indicate if a gene is significant based on a specific thresholds
  left_join(xtrop, by=c("gene_id")) %>% # add annotations
  filter(padj<0.1) %>%
  ggplot(aes(x=log2FoldChange, y=-log10(padj), color=sig)) +
  geom_point(alpha=0.75, shape=16) +
  geom_text_repel(data=. %>% filter(sig2),
                  aes(label=xtr_pep_name_x),
                  max.overlaps = 50,
                  size=2) +
  xlim(-10,10) +
  ggtitle("DEGs in Black Dorsal Skin in Comparison to Black Ventral Skin") +
  theme_bw() +
  theme(legend.position = "none")


test<-res$bD_bV %>%
  as_tibble(rownames = "gene_id") %>%
  drop_na(padj) %>% # drop all genes with NAs
  #filter(padj<0.99) %>% # reduce the number of points that need to be plotted
  mutate(sig= padj<0.05 & abs(log2FoldChange)>=2) %>% # make a variable to indicate if a gene is significant based on a specific thresholds
  mutate(sig2= padj<0.001 & abs(log2FoldChange)>=2) %>% # make a variable to indicate if a gene is significant based on a specific thresholds
  left_join(xtrop, by=c("gene_id"))
