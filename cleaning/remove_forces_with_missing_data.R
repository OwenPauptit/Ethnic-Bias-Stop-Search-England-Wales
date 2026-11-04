arrests <- read.csv("data/cleaned/arrests.csv")

months <- list.files(path = "data/stop-and-search")

missing_forces <- data.frame(
  month = c(),
  forces = c()
)

prefix_len = str_length("xxxx-xx-")
suffix_len = str_length("-stop-and-search.csv")

getMissingForcesForMonth <- function(month) {
  files <- list.files(path = paste0("data/stop-and-search/", month),
                      pattern = "*-stop-and-search.csv")
  
  filename_len = str_length(files)
  forces <- substr(files, prefix_len+1, filename_len - suffix_len)
  forces <- gsub('-', '_', forces)
  return(setdiff(arrests$police_force, forces))
}

missing_forces <- lapply(months, getMissingForcesForMonth)

missing_forces_df <- data.frame(months, I(missing_forces))

missing_forces_df %>%
  mutate(
    missing_forces = str_to_title(gsub('_', ' ',  paste( missing_forces,  collapse=", ")))
  ) %>%
  knitr::kable("latex")

financial_year <- function(yearmonth) {
  year <- as.integer(substr(yearmonth,1,4))
  month <- as.integer(substr(yearmonth, 6,7))
  year <- year - (month < 4)
  return(paste0(year, "/", year+1-2000))
}

missing_forces_by_year <- missing_forces_df %>%
  mutate(f_year = financial_year(months)) %>%
  group_by(f_year) %>%
  summarise(missing_forces = list(unique(unlist(missing_forces))))

# Probably a better table to include
missing_forces_by_year %>%
  group_by(f_year) %>%
  mutate(
    missing_forces = str_to_title(gsub('_', ' ',  paste(unlist(missing_forces),  collapse=", ")))
  ) %>%
  knitr::kable("latex")

stopsearch_arrests <- read.csv("data/cleaned/stopsearch_arrests.csv")

missing_forces_by_prev_year <- missing_forces_by_year %>%
  unnest(missing_forces) %>%
  mutate(
    previous_financial_year = as.integer(substr(f_year,3,4)),
    previous_financial_year = paste0(2000+previous_financial_year-1, "/", previous_financial_year),
    police_force = missing_forces
  )

stopsearch_arrests <- stopsearch_arrests %>%
  anti_join(missing_forces_by_prev_year, by=c("previous_financial_year", "police_force"))

write.csv(stopsearch_arrests, "data/cleaned/stopsearch_arrests.csv", row.names=FALSE)


###########
# Plots

#read areas <- read_csv...

areas <- areas %>%
  st_simplify(dTolerance = 4000, preserveTopology = TRUE)


all_forces <- unique(arrests$police_force)

missing_forces_for_map <- matrix(NA, ncol = nrow(missing_forces_by_year), nrow = length(all_forces))

for (i in 1:nrow(missing_forces_by_year)) {
  missing_forces_for_map[,i] <- all_forces %in% unlist(missing_forces_by_year[i, "missing_forces"])
}

rownames(missing_forces_for_map) <- all_forces
colnames(missing_forces_for_map) <- missing_forces_by_year$f_year

missing_map_df <- as.data.frame(missing_forces_for_map) %>%
  rownames_to_column("police_force") %>%
  pivot_longer(-police_force, names_to = "year", values_to = "missing") %>%
  filter(year > "2015/16", year < "2024/25") 

areas_and_missing_forces <- left_join(areas, missing_map_df)


ggplot(areas_and_missing_forces) + 
  geom_sf(aes(fill = missing)) +
  facet_wrap(vars(year), nrow=2) +
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = rel(1.0), colour = "black")) +
  scale_fill_manual(values = c("white", "red"))

ggsave("visualisations/missing_data.pdf", width=6, height=4, units = "in")
