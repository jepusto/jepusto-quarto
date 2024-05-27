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

source_dir <- "publication"

yaml_vals <- 
  tibble(
    unit = list.files(source_dir, pattern = "index.qmd$", recursive = TRUE)
  ) %>%
  filter(str_detect(unit, "^\\_", negate = TRUE)) %>%
  mutate(
    yamls = lapply(unit, get_yaml_vars, source_dir = source_dir, just_names = FALSE),
    icons = lapply(yamls, \(x) tibble(icon = lapply(x$links, \(y) y$faicon))),
    n = sapply(icons, nrow)
  )

icons <- 
  yaml_vals %>%
  filter(n > 0L) %>%
  select(-yamls) %>%
  unnest(icons) %>%
  unnest(icon)

icons %>%
  count(icon)


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


# # FA                    # BS
# code                    code-slash
# file                    file-pdf-fill
# file-pen                pencil-square
# gift                    box2
# lock-open               unlock-fill
# person-chalkboard       file-easel
# tablet-screen-button    tablet
# newspaper               newspaper
# table                   table
# youtube                 youtube


edit_icon_yaml <- function(x) {
  read_lines(x) |>
    str_replace(pattern = "faicon: code", replacement = "icon: code-slash") |>
    str_replace(pattern = "faicon: file", replacement = "icon: file-pdf-fill") |>
    str_replace(pattern = "faicon: file-pen", replacement = "icon: pencil-square") |>
    str_replace(pattern = "faicon: gift", replacement = "icon: box2") |>
    str_replace(pattern = "faicon: lock-open", replacement = "icon: unlock-fill") |>
    str_replace(pattern = "faicon: person-chalkboard", replacement = "icon: file-easel") |>
    str_replace(pattern = "faicon: tablet-screen-button", replacement = "icon: tablet") |>
    str_replace(pattern = "faicon: newspaper", replacement = "icon: newspaper") |>
    str_replace(pattern = "faicon: table", replacement = "icon: table") |>
    str_replace(pattern = "faicon: youtube", replacement = "icon: youtube") |>
    write_lines(file = x)
}

list.files("publication", pattern = "index.qmd$", recursive = TRUE, full.names = TRUE) |>
  walk(.f = edit_icon_yaml)
