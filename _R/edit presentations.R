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

source_dir <- "presentations"

yaml_vals <- 
  tibble(
    unit = list.files(source_dir, pattern = "qmd$")
  ) %>%
  filter(str_detect(unit, "^\\_", negate = TRUE)) %>%
  mutate(
    yamls = lapply(unit, get_yaml_vars, source_dir = source_dir, just_names = FALSE),
  )

icons <- 
  yaml_vals %>%
  mutate(
    icons = lapply(yamls, \(x) tibble(icon = sapply(x$links, \(y) y$icon))),
    n = sapply(icons, nrow)
  ) %>%
  filter(n > 0L) %>%
  select(-yamls) %>%
  unnest(icons)

icons %>%
  count(icon)


edit_presentation <- function(file) {
  
  x <- readLines(file) |>
    str_replace(pattern = "^- icon: fa ", replacement = "- fa-icon: ") |>
    str_replace(pattern = "^authors:", replacement = "author:")
  
  x <- x[x != "{{< include _presentation_header.qmd >}}"]
  
  write_lines(x, file = file)
}


tibble(
  unit = list.files(source_dir, pattern = "qmd$", full.names = TRUE)
) %>%
  pull(unit) %>%
  walk(edit_presentation)
