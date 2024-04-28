library(tidyverse)

source_dir <- "_talk"
all_units <- tibble(
  unit = list.files(source_dir)
)

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

yaml_vars <- 
  all_units %>%
  mutate(
    yamls = lapply(unit, get_yaml_vars, source_dir = source_dir),
    X = "X"
  ) %>%
  unnest(yamls) %>%
  pivot_wider(names_from = yamls, values_from = X)

yaml_vals <- 
  all_units %>%
  mutate(
    yamls = lapply(unit, get_yaml_vars, source_dir = source_dir, just_names = FALSE),
    authors = sapply(yamls, \(x) x$authors),
    title = sapply(yamls, \(x) x$title),
    date = sapply(yamls, \(x) x$date),
    event = sapply(yamls, \(x) x$event),
    event_short = sapply(yamls, \(x) x$event_short),
    location = sapply(yamls, \(x) x$location),
    featured = sapply(yamls, \(x) x$featured),
    pdf = sapply(yamls, \(x) x$url_pdf),
    slides = sapply(yamls, \(x) x$url_slides),
    math = sapply(yamls, \(x) x$math),
    highlight = sapply(yamls, \(x) x$highlight),
    abstract = sapply(yamls, \(x) x$abstract),
    event_url = sapply(yamls, \(x) x$event_url),
    video = sapply(yamls, \(x) x$url_video),
    code = sapply(yamls, \(x) x$url_code),
    projects = sapply(yamls, \(x) x$projects),
    summary = sapply(yamls, \(x) x$summary),
    tags = sapply(yamls, \(x) x$tags),
  )

View(yaml_vals)

convert_post <- function(unit, source_dir, destination_dir) {
  
  post_text <- readLines(paste(source_dir,unit,  sep = "/"))
  yaml_lines <- which(post_text == "---")
  yaml_text <- post_text[(yaml_lines[1] + 1):(yaml_lines[2] - 1)]
  yaml_vars <- yaml::read_yaml(text = yaml_text)
  
  if (is.null(yaml_vars$authors) || length(yaml_vars$authors) == 0) {
    yaml_vars$authors <- list("admin")
  }
  
  yaml_vars$title <- str_remove_all(yaml_vars$title, "\n")
  
  new_links <- list()
  files_to_move <- list()
  
  if (!is.null(yaml_vars$url_pdf) && str_length(yaml_vars$url_pdf) > 0) {
    new_links[[length(new_links) + 1L]] <- list(
      icon = "fa file",
      text = "PDF",
      url = yaml_vars$url_pdf
    )
  }

  if (!is.null(yaml_vars$url_slides) && str_length(yaml_vars$url_slides) > 0) {
    new_links[[length(new_links) + 1L]] <- list(
      icon = "fa person-chalkboard",
      text = "Slides",
      url = yaml_vars$url_slides
    )
  }
  
  if (!is.null(yaml_vars$url_video) && str_length(yaml_vars$url_video) > 0) {
    new_links[[length(new_links) + 1L]] <- list(
      icon = "fa youtube",
      text = "Video",
      url = yaml_vars$url_video
    )
  }
  
  if (!is.null(yaml_vars$url_code) && str_length(yaml_vars$url_code) > 0) {
    new_links[[length(new_links) + 1L]] <- list(
      icon = "fa code",
      text = "Code",
      url = yaml_vars$url_code
    )
  }
  
  new_yaml <- list(
    title = yaml_vars$title,
    authors = yaml_vars$authors,
    date = yaml_vars$date,
    publish_date = yaml_vars$publishDate,
    event = yaml_vars$event,
    event_short = yaml_vars$event_short,
    event_url = yaml_vars$event_url,
    location = yaml_vars$location,
    links = new_links
  )
  
  new_yaml <- new_yaml[sapply(new_yaml, \(x) !is.null(x))]
  
  yaml_text <- yaml::as.yaml(new_yaml)

  if (length(post_text) > yaml_lines[2]) {
    post_text_body <- post_text[(yaml_lines[2] + 1):length(post_text)]
  } else {
    post_text_body <- ""
  }
  
  if (!is.null(yaml_vars$abstract) && str_length(yaml_vars$abstract) > 0L) {
    post_text_body <- c(post_text_body, yaml_vars$abstract)
  }
  
  post_text_new <- c(
    "---", 
    yaml_text, 
    "---", 
    "{{< include _presentation_header.qmd >}}",
    "",
    post_text_body,
    ""
  )
  
  unit_name <- str_replace(unit, ".md$", ".qmd")
  new_post_name <- paste(destination_dir, unit_name, sep = "/")
  write_lines(post_text_new, file = new_post_name)
  
}


convert_post(
  "ABAI-2019-log-response-ratios.md", 
  source_dir = "_talk", 
  destination_dir = "presentations"
)

walk(
  all_units$unit, 
  convert_post,
  source_dir = "_talk", 
  destination_dir = "presentations"
)
