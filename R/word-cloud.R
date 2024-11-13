library(tidyverse)
library(openalexR)
library(wordcloud)

author <- oa_authors(orcid = "0000-0003-4719-7472")

works_from_orcids <- oa_fetch(
  entity = "works",
  author.orcid = "0000-0003-4719-7472",
  verbose = TRUE
)

concept_cloud <- works_from_orcids |>
  select(inst_id = id, concepts) |>
  tidyr::unnest(concepts) |>
  filter(level > 1) |>
  add_count(display_name) |>
  select(display_name, n) |>
  group_by(display_name) |>
  summarise(score = sqrt(sum(n)))

pal <- c("black", scales::brewer_pal(palette = "Set1")(5))

set.seed(1)


wordcloud::wordcloud(
  concept_cloud$display_name,
  concept_cloud$score,
  scale = c(2, .4),
  colors = pal
)

