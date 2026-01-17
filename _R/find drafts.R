library(tidyverse)

get_yaml_vars <- function(unit, just_names = TRUE) {
  post_text <- readLines(unit)
  yaml_lines <- which(post_text == "---")
  yaml_text <- post_text[(yaml_lines[1] + 1):(yaml_lines[2] - 1)]
  yaml_vars <- yaml::read_yaml(text = yaml_text)
  if (just_names) {
    return(names(yaml_vars))
  } else {
    yaml_vars
  }
}


post_yaml <- 
  tibble(
    post = list.files(
      "posts", 
      pattern = "index.qmd", 
      full.names = TRUE, 
      recursive = TRUE
    )
  ) |> 
  mutate(
    text = map(post, get_yaml_vars)
  )
