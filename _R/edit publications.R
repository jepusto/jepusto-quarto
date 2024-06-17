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

source_dir <- "publications"

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



# create citation yamls

yaml_vals %>%
  select(unit, yamls) %>%
  mutate(
    yamls = map(yamls, ~ tibble(par = names(.x), val = .x)),
  ) %>% 
  unnest(yamls) %>%
  pivot_wider(names_from = par, values_from = val)


author_list <- 
  yaml_vals %>%
  mutate(author = map(yamls, ~ .x$author)) %>%
  pull(author)

yaml_vals %>%
  mutate(author = map(yamls, ~ tibble(author = unlist(.x$author)))) %>%
  select(unit, author) %>%
  unnest(author) %>%
  count(author) %>%
  View()


parse_names <- function(name) {
  if (is.list(name)) return(name)
  
  x <- str_split(name, " ")[[1]]
  
  if ("van" %in% tolower(x)) {
    i <- which(tolower(x) == "van")
    dropping_particle <- x[i]
    x <- x[-i]
  } else {
    dropping_particle <- NULL
  }
  
  last_i <- length(x)
  
  name_list <- list(
    name = list(
      given = paste(x[-last_i], collapse = " "),
      family = x[last_i]
    )
  )
  
  if (!is.null(dropping_particle)) {
    name_list$name$`dropping-particle` <- dropping_particle
  }
  
  return(name_list)
}
  

edit_publication <- function(file) {
  
  post_text <- read_lines(file)
  yaml_lines <- which(post_text == "---")
  yaml_text <- post_text[(yaml_lines[1] + 1):(yaml_lines[2] - 1)]
  yaml_vars <- yaml::read_yaml(text = yaml_text)
  
  if (!is.null(yaml_vars$citation)) return()
  if (yaml_vars$publication_type != "Journal article") return()
  
  yaml_vars$author <- lapply(yaml_vars$author, parse_names)

  yaml_vars$citation <- list(
    type = "article-journal",
    `container-title` =  yaml_vars$publication
  )
  
  yaml_link_names <- sapply(yaml_vars$links, \(x) x$text)
  if ("Journal" %in% yaml_link_names) {
    yaml_vars$citation$url <- yaml_vars$links[[which(yaml_link_names == "Journal")]]$url
    yaml_vars$citation$doi <- str_sub(str_extract(url, "doi\\.org/.+"), 9, -1)
  }
  
  if (!is.null(yaml_vars$volume)) yaml_vars$citation$volume <- yaml_vars$volume
  if (!is.null(yaml_vars$issue)) yaml_vars$citation$issue <- yaml_vars$issue
  if (!is.null(yaml_vars$page)) yaml_vars$citation$page <- yaml_vars$page
  
  yaml_vars$publication_type <- NULL
  yaml_vars$publication <- NULL
  yaml_vars$publish_date <- NULL
  yaml_vars$volume <- NULL
  yaml_vars$issue <- NULL
  yaml_vars$pages <- NULL
  
  yaml_text <- yaml::as.yaml(yaml_vars)
  
  if (length(post_text) > yaml_lines[2]) {
    post_text_body <- post_text[(yaml_lines[2] + 1):length(post_text)]
  } else {
    post_text_body <- ""
  }
  
  x <- c(
    "---", 
    yaml_text, 
    "---", 
    post_text_body,
    ""
  )
  
  write_lines(x, file = file)
}


all_pubs <- 
  tibble(
    unit = list.files(source_dir, pattern = "index.qmd$", recursive = TRUE, full.names = TRUE)
  ) %>%
  filter(unit != "publications/index.qmd") %>%
  pull(unit)

edit_publication(all_pubs[3])

walk(all_pubs, edit_publication)


pub_types <- 
  yaml_vals %>%
  filter(unit != "index.qmd") %>%
  mutate(
    type = map_chr(yamls, ~ if ("citation" %in% names(.x)) { .x$citation$type } else {.x$publication_type})
  )

pub_types %>%
  count(type)

pub_types %>%
  filter(type != "article-journal") %>%
  arrange(type) 

