summarize_puma_feeding <- function(boris_data) {
  
  aggregated_puma_feeding <- boris_data %>%
    # consolidate puma and kitten behaviours
    mutate(Subject = ifelse(grepl("\\d+[MF]|PUMA.*", Subject), "Puma", Subject)) %>%
    mutate(Subject = ifelse(grepl("KITTEN[123]", Subject), "Kitten", Subject)) %>%
    # filter out non-puma behaviours
    filter(Subject == "Puma" | Subject == "Kitten") %>%
    # interested only in feeding
    filter(Behaviour == "feed") %>%
    # get statistics on feeding times
    group_by(Deployment.id, Subject) %>%
    summarise(
      `Total Feeding Time` = sum(Duration..s.),
      `Mean Feeding Bout Duration` = mean(Duration..s.),
      `Std Dev Feeding Bout Duration` = sd(Duration..s.),
      .groups = "keep"
    ) %>%
    ungroup() %>%
    arrange(Deployment.id, Subject) 
  
  # pivot to shift behavioural counts into their own columns
  feeding_summary <- aggregated_puma_feeding %>%
    pivot_wider(names_from = Subject,
                values_from = c(
                  "Total Feeding Time",
                  "Mean Feeding Bout Duration",
                  "Std Dev Feeding Bout Duration"),
                values_fill = 0,
                names_sep = " ")
  
  return(feeding_summary)
}