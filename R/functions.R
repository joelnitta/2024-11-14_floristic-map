#' Generate a wordcloud of research topics for a researcher from their ORCID
#' @param orcid Character vector: ORCID (id) for one or more researchers
#' @param seed Seed for random number generator, needed for consistent
#'   wordclouds
#' @return Plot object
orcid_to_wordcloud <- function(orcid = NULL, works = NULL, seed = 1) {
  # Fetch all works for the author by their ORCID
  if (is.null(works) && !is.null(orcid)) {
    works_from_orcids <- openalexR::oa_fetch(
      entity = "works",
      author.orcid = orcid,
      verbose = TRUE
    )
  } else if (is.null(orcid) && !is.null(works)) {
    works_from_orcids <- works
  } else {
    stop("Must provide orcid or works, but not both")
  }

  # List concepts of their works and assign an importance score
  concept_cloud <- works_from_orcids |>
    dplyr::select(inst_id = id, concepts) |>
    tidyr::unnest(concepts) |>
    # Level 1 is so general its not helpful ("biology")
    dplyr::filter(level > 1) |>
    dplyr::add_count(display_name) |>
    dplyr::select(display_name, n) |>
    dplyr::group_by(display_name) |>
    dplyr::summarize(score = sqrt(sum(n)))

  # Generate wordcloud
  pal <- c("black", scales::brewer_pal(palette = "Set1")(5))

  set.seed(seed)

  wordcloud::wordcloud(
    concept_cloud$display_name,
    concept_cloud$score,
    scale = c(2, .4),
    colors = pal
  )
}

works_from_orcid <- function(orcid) {
  openalexR::oa_fetch(
    entity = "works",
    author.orcid = orcid,
    verbose = TRUE
  )
}