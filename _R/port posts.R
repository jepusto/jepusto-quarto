library(tidyverse)

all_posts <- tibble(
  post = list.files("_RMD-posts", "Rmd$")
)

get_yaml_vars <- function(post, just_names = TRUE) {
  post_text <- readLines(paste("_Rmd-posts",post, sep = "/"))
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
  all_posts %>%
  mutate(
    yamls = lapply(post, get_yaml_vars),
    X = "X"
  ) %>%
  unnest(yamls) %>%
  pivot_wider(names_from = yamls, values_from = X)

yaml_vars %>%
  filter(!is.na(bibliography)) %>%
  mutate(
    bibliography = map_chr(post, \(x) get_yaml_vars(x, just_names = FALSE)$bibliography)
  )

yaml_vars %>%
  summarize(across(-post, ~ sum(!is.na(.x)))) %>%
  pivot_longer(everything())

convert_post <- function(post) {
  post_text <- readLines(paste("_Rmd-posts",post, sep = "/"))
  
  yaml_lines <- which(post_text == "---")
  yaml_text <- post_text[(yaml_lines[1] + 1):(yaml_lines[2] - 1)]
  yaml_vars <- yaml::read_yaml(text = yaml_text)
  slug <- yaml_vars$slug
  yaml_vars$categories <- yaml_vars$tags
  yaml_vars$tags <- NULL
  yaml_vars$header <- NULL
  yaml_vars$slug <- NULL
  yaml_vars$featured <- NULL
  if ("lastmod" %in% names(yaml_vars)) {
    yaml_vars$`date-modified` <- yaml_vars$lastmod
    yaml_vars$lastmod <- NULL
  }
  if ("number_sections" %in% names(yaml_vars)) {
    yaml_vars$`number-sections` <- yaml_vars$number_sections
    yaml_vars$number_sections <- NULL
  }
  if ("summary" %in% names(yaml_vars)) {
    yaml_vars$description <- yaml_vars$summary
    yaml_vars$summary <- NULL
  }

  if ("codefolding_show" %in% names(yaml_vars)) {
    yaml_vars$`code-fold` <- if (yaml_vars$codefolding_show == "show")  {
      "show"
    } else if (yaml_vars$codefolding_show == "hide") {
      TRUE
    } else {
      FALSE
    }
    yaml_vars$codefolding_show <- NULL
  }
  
  yaml_vars$`code-tools` <- TRUE
  
  if ("disable_codefolding" %in% names(yaml_vars)) {
    yaml_vars$`code-tools` <- !yaml_vars$disable_codefolding 
    yaml_vars$disable_codefolding <- NULL
  }

  if ("codefolding_nobutton" %in% names(yaml_vars)) {
    yaml_vars$`code-tools` <- !yaml_vars$codefolding_nobutton 
    yaml_vars$codefolding_nobutton <- NULL
  }
  
  
  yaml_text <- yaml::as.yaml(yaml_vars, handlers = list(logical = yaml::verbatim_logical))
  
  post_text_body <- post_text[(yaml_lines[2]+1):length(post_text)]
  
  post_text_body <- str_replace_all(post_text_body, "`r ", "`{r} ")
  new_post_name <- paste("posts", slug, "index.qmd", sep = "/")
  
  dir_path <- paste("posts", slug, sep = "/")
  if (!dir.exists(dir_path)) dir.create(dir_path)
  post_text_new <- c("---", yaml_text, "---", post_text_body)
  write_lines(post_text_new, file = new_post_name)
}

convert_post("correlated-z-transformed-correlations.Rmd")
convert_post("imputing-covariance-matrices-for-multi-variate-meta-analysis.Rmd")
convert_post("Alternating-renewal-process-models-for-behavioral-observation.Rmd")
convert_post("vcalc-example.Rmd")

walk(all_posts$post, convert_post)

