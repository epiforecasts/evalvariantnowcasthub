
library(tidyr)
library(data.table)
library(dplyr)
library(ggplot2)
library(MetBrewer)

clade_comp_weekly <- data.table::fread("covidnet_vs_ncbi_clade_proportions.csv")

ncolors <- length(unique(clade_comp_weekly$clade))
cladepal <- MetBrewer::met.brewer(name="Hiroshige", n=ncolors, type="continuous")

clade_comp_weekly %>%   
  group_by(nowcast_date, week, source) %>% 
  mutate(prop=nseq/sum(nseq)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_bar(aes(x=week, y=prop, fill=clade), stat="identity") +
  geom_vline(aes(xintercept=nowcast_date), linetype="dashed") +
  facet_grid(source~nowcast_date, scales="free_x") +
  scale_x_date("Date", date_breaks="1 month", date_labels="%b") +
  scale_y_continuous("Proportion of all sequences") +
  scale_fill_manual("Clade", values = cladepal) +
  theme_classic()

volume_comp_weekly <- data.table::fread("covidnet_vs_ncbi_sequence_volume.csv")

volume_comp_weekly %>% 
  mutate(ratio=ncbi/covidnet) %>% 
  ggplot() +
  geom_line(aes(x=week, y=ratio)) +
  geom_hline(aes(yintercept=1), color="red") +
  geom_vline(aes(xintercept=nowcast_date), linetype="dashed") +
  facet_grid(~nowcast_date, scales="free_x") +
  scale_x_date("Date", date_breaks="1 month", date_labels="%b") +
  scale_y_continuous("Ratio of NCBI to COVIDNet sequence volume") +
  scale_fill_discrete("Data") +
  theme_classic()

