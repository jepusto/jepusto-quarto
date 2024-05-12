library(tidyverse)

source_dir <- "_publication"
all_units <- tibble(
  unit = list.files(source_dir)
)

get_yaml_vars <- function(unit, source_dir, just_names = TRUE) {
  post_text <- readLines(paste(source_dir, unit,"index.md", sep = "/"))
  yaml_lines <- which(post_text == "---")
  yaml_text <- post_text[(yaml_lines[1] + 1):(yaml_lines[2] - 1)]
  yaml_vars <- yaml::read_yaml(text = yaml_text)
  if (just_names) {
    return(names(yaml_vars))
  } else {
    yaml_vars
  }
}

get_yaml_vars(all_units$unit[1], source_dir = source_dir)

yaml_vars <- 
  all_units %>%
  mutate(
    yamls = lapply(unit, get_yaml_vars, source_dir = source_dir),
    X = "X"
  ) %>%
  unnest(yamls) %>%
  pivot_wider(names_from = yamls, values_from = X)

yaml_vars %>%
  summarize(across(-unit, ~ sum(.x == "X", na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "field", values_to = "n") %>%
  View()
            
yaml_vals <- 
  all_units %>%
  mutate(
    yamls = lapply(unit, get_yaml_vars, source_dir = source_dir, just_names = FALSE),
    date = sapply(yamls, \(x) x$date),
    publishDate = sapply(yamls, \(x) x$publishDate),
    pub_type = sapply(yamls, \(x) x$publication_types),
    authors = sapply(yamls, \(x) x$authors),
    title = sapply(yamls, \(x) x$title),
    publication = sapply(yamls, \(x) x$publication),
    abstract = sapply(yamls, \(x) x$abstract),
    featured = sapply(yamls, \(x) x$featured),
    tags = sapply(yamls, \(x) x$tags),
    image = sapply(yamls, \(x) x$image),
    projects = sapply(yamls, \(x) x$projects),
    link_names = sapply(yamls, \(x) lapply(x$links, \(y) y$name)),
    links = sapply(yamls, \(x) x$links),
    slides = sapply(yamls, \(x) x$url_slides),
    preprint = sapply(yamls, \(x) x$url_preprint),
    code = sapply(yamls, \(x) x$url_code),
    dataset = sapply(yamls, \(x) x$url_dataset),
    pdf = sapply(yamls, \(x) x$url_pdf),
    video = sapply(yamls, \(x) x$url_video),
  )

View(yaml_vals)

unlist(yaml_vals$links) %>% table()

process_links <- function(links) {
  
}

convert_post <- function(unit, source_dir, destination_dir) {
  
  post_text <- readLines(paste(source_dir,unit,  sep = "/"))
  yaml_lines <- which(post_text == "---")
  yaml_text <- post_text[(yaml_lines[1] + 1):(yaml_lines[2] - 1)]
  yaml_vars <- yaml::read_yaml(text = yaml_text)
  
  yaml_vars$title <- str_remove_all(yaml_vars$title, "\n")

  if (is.null(yaml_vars$authors) || length(yaml_vars$authors) == 0) {
    yaml_vars$authors <- list("admin")
  }
  
  yaml_vars$publication_types <- case_match(yaml_vars$publication_types, 
                                            0 = "Report",
                                            2 = "Journal article",
                                            3 = "Pre-print",
                                            6 = "Book chapter")
  
  new_links <- process_links(yaml_vars$links)
  
  if (!is.null(yaml_vars$url_preprint) && str_length(yaml_vars$url_preprint) > 0) {
    new_links[[length(new_links) + 1L]] <- list(
      icon = "fa lock-open",
      text = "Pre-Print",
      url = yaml_vars$url_preprint
    )
  }
  

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
  
  if (!is.null(yaml_vars$url_code) && str_length(yaml_vars$url_code) > 0) {
    new_links[[length(new_links) + 1L]] <- list(
      icon = "fa code",
      text = "Code",
      url = yaml_vars$url_code
    )
  }
  
  if (!is.null(yaml_vars$url_dataset) && str_length(yaml_vars$url_dataset) > 0) {
    new_links[[length(new_links) + 1L]] <- list(
      icon = "fa table",
      text = "Data",
      url = yaml_vars$url_dataset
    )
  }
  
  if (!is.null(yaml_vars$url_video) && str_length(yaml_vars$url_video) > 0) {
    new_links[[length(new_links) + 1L]] <- list(
      icon = "fa youtube",
      text = "Video",
      url = yaml_vars$url_video
    )
  }
  
  
  new_yaml <- list(
    title = yaml_vars$title,
    authors = yaml_vars$authors,
    date = yaml_vars$date,
    publish_date = yaml_vars$publishDate,
    publication_type = yaml_vars$publication_types,
    publication = yaml_vars$publication,
    categories = yaml_vars$tags,
    image = yaml_vars$image$caption,
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
    "{{< include '../_publication_header.qmd' >}}",
    "",
    post_text_body,
    ""
  )
  
  unit_name <- str_replace(unit, ".md$", ".qmd")
  new_post_name <- paste(destination_dir, unit_name, sep = "/")
  write_lines(post_text_new, file = new_post_name)
 
  image_file <- list.files(paste(source_dir,unit, sep = "/"), pattern = ".(jpg|png)")
  file.copy(
    from = paste(source_dir, unit, image_file, sep = "/"), 
    to = paste(destination_dir,unit, image_file, sep = "/"),
    overwrite = TRUE
  ) 
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
