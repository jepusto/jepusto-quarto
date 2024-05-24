library(tidyverse)

get_yaml_vars <- function(unit, source_dir, just_names = TRUE) {
  post_text <- readLines(paste(source_dir, unit, sep = "/"))
  yaml_lines <- which(post_text == "---")
  yaml_text <- post_text[(yaml_lines[1] + 1):(yaml_lines[2] - 1)]
  yaml_vars <- yaml::read_yaml(text = yaml_text)
  if (just_names) {
    return(names(yaml_vars))
  } else {
    yaml_vars
  }
}

source_dir <- "posts"

yaml_vals <- 
  tibble(
    unit = list.files(source_dir, pattern = "index.qmd$", recursive = TRUE)
  ) %>%
  filter(str_detect(unit, "^\\_", negate = TRUE)) %>%
  mutate(
    yamls = lapply(unit, get_yaml_vars, source_dir = source_dir, just_names = FALSE),
    yaml_names = lapply(yamls, \(x) tibble(token = names(x)))
  )

yaml_vals %>%
  select(-yamls) %>%
  unnest(yaml_names) %>%
  mutate(X = "X") %>%
  pivot_wider(names_from = token, values_from = X) %>%
  filter(author == "X")

edit_publication <- function(file) {
  
  x <- readLines(file) |>
    str_replace(pattern = "authors:", replacement = "author:") |>
    str_replace(pattern = "icon: fa ", replacement = "fa-icon: ")
  
  x <- x[x != "{{< include '../_publication_header.qmd' >}}"]
  
  write_lines(x, file = file)
}


tibble(
  unit = list.files(source_dir, pattern = "index.qmd$", recursive = TRUE, full.names = TRUE)
) %>%
  pull(unit) %>%
  walk(edit_publication)
