library(tidyverse)

source_dir <- "publications"
all_units <- tibble(
  unit = list.files(source_dir)
) %>%
  filter(
    str_sub(unit, 1, 1) != "_",
    unit != "index.qmd"
  )

get_yaml_vars <- function(unit, source_dir) {
  post_text <- readLines(paste(source_dir, unit,"index.qmd", sep = "/"))
  yaml_lines <- which(post_text == "---")
  yaml_text <- post_text[(yaml_lines[1] + 1):(yaml_lines[2] - 1)]
  yaml_vars <- yaml::read_yaml(text = yaml_text)
  yaml_vars
}

get_yaml_vars(all_units$unit[4], source_dir = source_dir)

yaml_vals <- 
  all_units %>%
  mutate(
    yamls = lapply(unit, get_yaml_vars, source_dir = source_dir),
    year = sapply(yamls, \(x) lubridate::year(x$date)),
    title = sapply(yamls, \(x) x$title),
    type = sapply(yamls, \(x) x$citation$type),
    links = lapply(yamls, \(x) data.frame(link = sapply(x$links, \(y) y$text), present = TRUE))
  ) %>%
  unnest(links) %>%
  pivot_wider(names_from = link, values_from = present, values_fill = FALSE) %>%
  mutate(
    OA = unit %in% c("More-is-not-necessarily-better","BC-SMD-primer-and-tutorial","SMD-for-SCD","Understanding-teacher-leadership"),
    Open = OA | PDF | `Pre-Print`
  )

yaml_vals %>% count(type)

yaml_vals %>%
  filter(type %in% c("article","article-journal","chapter")) %>%
  group_by(type) %>%
  summarize(
    n = n(),
    across(c(Journal, `Pre-Print`, Publisher, PDF, OA, Open), ~ sum(.x))
  )

yaml_vals %>%
  filter(
    type %in% c("article","article-journal","chapter"),
    year >= 2020
  ) %>%
  group_by(type) %>%
  summarize(
    n = n(),
    across(c(Journal, `Pre-Print`, Publisher, PDF, OA, Open), ~ sum(.x))
  )


yaml_vals %>%
  filter(
    type %in% c("article","article-journal","chapter"),
    !`Pre-Print`
  ) %>%
  arrange(desc(year)) %>%
  select(unit, year, title, type, Open) %>% 
  View()
