library(tidyverse)

all_people <- tibble(
  person = list.files("people")
)

get_yaml_vars <- function(person, just_names = TRUE) {
  post_text <- readLines(paste("people",person, "_index.md", sep = "/"))
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
  all_people %>%
  mutate(
    yamls = lapply(person, get_yaml_vars),
    X = "X"
  ) %>%
  unnest(yamls) %>%
  pivot_wider(names_from = yamls, values_from = X)


convert_post <- function(person) {
  
  post_text <- readLines(paste("people",person, "_index.md", sep = "/"))
  yaml_lines <- which(post_text == "---")
  yaml_text <- post_text[(yaml_lines[1] + 1):(yaml_lines[2] - 1)]
  yaml_vars <- yaml::read_yaml(text = yaml_text)
  
  links <- map(yaml_vars$social, ~ {
    if (.x$icon == "google-scholar") {
      list(text = "{{< ai google-scholar >}}", href = .x$link)
    } else {
      list(icon = .x$icon, href = .x$link)
    }
  })
  
  new_yaml <- list(
    title = yaml_vars$name,
    image = "avatar.jpg",
    `page-layout` = "full",
    about = list(
      id = "about",
      template = "trestles",
      `image-shape` = "round",
      links = links
    ),
    org_name = yaml_vars$organizations[[1]]$name,
    org_url = yaml_vars$organizations[[1]]$url,
    role = yaml_vars$role,
    `people-group` = yaml_vars$user_groups
  )
  
  yaml_text <- yaml::as.yaml(new_yaml)
  education_text <- map(
    yaml_vars$education$courses,
    ~ paste0("- ", .x$course, " | ", .x$year, " <br> [", .x$institution, "]{style='color:gray;'}")
  ) %>%
    unlist()
  
  interests_text <- paste("-", yaml_vars$interests)
  
  post_text_body <- post_text[(yaml_lines[2]+4):length(post_text)]
  
  post_text_new <- c(
    "---", 
    yaml_text, 
    "---", 
    "",
    ":::::{#about}",
    "",
    post_text_body,
    "",
    "::::{.grid}",
    "",
    "::: {.g-col-6}",
    "",
    "## Education",
    "",
    education_text,
    "",
    ":::",
    "",
    "::: {.g-col-6}",
    "",
    "## Interests",
    "",
    interests_text,
    "",
    ":::",
    "",
    "::::",
    "",
    ":::::"
  )
  
  new_post_name <- paste("people", person, "index.qmd", sep = "/")
  
  write_lines(post_text_new, file = new_post_name)
}


convert_post("Bethany-H-Bhat")

walk(all_people$person, convert_post)
