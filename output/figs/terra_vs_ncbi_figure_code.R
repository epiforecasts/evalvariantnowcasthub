
clade_comp_weekly <- data.table::fread("terra_vs_ncbi_clade_proportions.csv")

clade_comp_weekly %>% 
  ggplot() +
  geom_bar(aes(x=week, y=prop, fill=clade), stat="identity") +
  geom_vline(aes(xintercept=nowcast_date), linetype="dashed") +
  facet_grid(source~nowcast_date, scales="free_x") +
  scale_x_date("Date", date_breaks="1 month", date_labels="%b") +
  scale_y_continuous("Proportion of all sequences") +
  scale_fill_manual(values = cladepal) +
  theme_classic()

volume_comp_weekly <- data.table::fread("terra_vs_ncbi_sequence_volume.csv")

volume_comp_weekly %>% 
  mutate(ratio=hub/terra) %>% 
  ggplot() +
  geom_line(aes(x=week, y=ratio)) +
  geom_hline(aes(yintercept=1), color="red") +
  geom_vline(aes(xintercept=nowcast_date), linetype="dashed") +
  facet_grid(~nowcast_date, scales="free_x") +
  scale_x_date("Date", date_breaks="1 month", date_labels="%b") +
  scale_y_continuous("Ratio of NCBI to Terra sequence volume") +
  scale_fill_discrete("Data") +
  theme_classic()
