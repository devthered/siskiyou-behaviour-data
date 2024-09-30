summarize_puma_non_feeding_behaviours <- function(boris_data) {
  
  # Count up puma non-feeding behaviours
  aggregated_puma_behaviours <- boris_data %>%
    # NZ spelling
    rename(Behaviour = Behavior) %>%
    # consolidate puma and kitten behaviours
    mutate(Subject = ifelse(grepl("\\d+[MF]|PUMA.*", Subject), "PUMA", Subject)) %>%
    mutate(Subject = ifelse(grepl("KITTEN[123]", Subject), "KITTEN", Subject)) %>%
    # filter out non-puma behaviours
    filter(Subject == "PUMA" | Subject == "KITTEN") %>%
    # filter out unspecified or feeding behaviours
    filter(
      Behaviour != "feed"
      & Behaviour != "repeat feeding"
      & Behaviour != "other"
      & Behaviour != "unknown") %>%
    # sum behaviour counts
    group_by(Deployment.id, Subject, Behaviour) %>%
    summarise(
      Behaviour.Count = n(),
      .groups = "keep"
    ) %>%
    ungroup() %>%
    arrange(Deployment.id, Subject, Behaviour) 
  
  # pivot to shift behavioural counts into their own columns
  behaviour_summary <- aggregated_puma_behaviours %>%
    pivot_wider(names_from = c(Subject, Behaviour),
                values_from = Behaviour.Count,
                values_fill = 0,
                names_sep = " ") %>%
    group_by(Deployment.id) %>%
    summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))
    
  # sort columns alphabetically
  behaviour_summary <- behaviour_counts %>%
    select(order(colnames(.)))
  
  return(behaviour_summary)
}