library(tidyverse)

all_units <- tibble(
  unit = list.files("software")
)

get_yaml_vars <- function(unit, just_names = TRUE) {
  post_text <- readLines(paste("_software",unit, "index.md", sep = "/"))
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
    yamls = lapply(unit, get_yaml_vars),
    X = "X"
  ) %>%
  unnest(yamls) %>%
  pivot_wider(names_from = yamls, values_from = X)


convert_post <- function(unit) {
  
  post_text <- readLines(paste("_software",unit, "index.md", sep = "/"))
  image_file <- list.files(paste("_software",unit, sep = "/"), pattern = ".(jpg|png)")
  yaml_lines <- which(post_text == "---")
  yaml_text <- post_text[(yaml_lines[1] + 1):(yaml_lines[2] - 1)]
  yaml_vars <- yaml::read_yaml(text = yaml_text)
  
  new_links <- lapply(yaml_vars$links, \(x) {
    names(x) <- c("text","url")
    x
  })
  urls <- yaml_vars[str_detect(names(yaml_vars), "^url")]
  urls <- urls[str_length(urls) > 0]
  url_text <- str_sub(names(urls), 5, -1)
  urls <- as.character(urls)
  urls_list <- mapply(\(x,y) list(text = x, url = y), x = url_text, y = urls, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  new_links <- c(new_links, urls_list)
  new_links <- lapply(new_links, \(x) {
    if (x$text == "code") {
      list(icon = "github", url = x$url)
    } else {
      x
    }
  })
  
  new_yaml <- list(
    title = yaml_vars$title,
    subtitle = yaml_vars$summary,
    image = image_file,
    header_image = yaml_vars$header$image,
    header_caption = yaml_vars$header$caption,
    authors = yaml_vars$authors,
    date = yaml_vars$date,
    categories = yaml_vars$tags,
    `page-layout` = "full",
    about = list(
      template = "trestles",
      links = new_links
    )
  )
  
  yaml_text <- yaml::as.yaml(new_yaml)

  post_text_body <- post_text[(yaml_lines[2] + 1):length(post_text)]
  
  post_text_new <- c(
    "---", 
    yaml_text, 
    "---", 
    post_text_body,
    ""
  )
  
  new_post_name <- paste("software", unit, "index.qmd", sep = "/")
  
  dir_path <- paste("software", unit, sep = "/")
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  write_lines(post_text_new, file = new_post_name)
  
  file.copy(
    from = paste("_software",unit, image_file, sep = "/"), 
    to = paste("software",unit, image_file, sep = "/"),
    overwrite = TRUE
  )
}


convert_post("wildmeta")

walk(all_units$unit, convert_post)
