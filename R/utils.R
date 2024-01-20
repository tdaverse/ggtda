# @importFrom ripserr vietoris_rips
# @importFrom RTriangle triangulate pslg
NULL

`%||%` <- getFromNamespace("%||%", "ggplot2")

# adapted from {ggplot2}
rd_sec_computed_vars <- function(
    stat, ..., .details = "", .skip_intro = FALSE, extra_note = NULL
) {
  args  <- list(...)
  items <- names(args)
  descr <- unname(args)
  
  # Format preamble
  header <- "@section Computed variables: "
  intro  <- paste0(
    "`stat_", stat, "` calculates the following variables ",
    "that can be accessed with [delayed evaluation][ggplot2::aes_eval]. "
  )
  if (.skip_intro) intro <- ""
  preamble <- c(header, paste0(intro, gsub("\n", "", .details)))
  
  # Format items
  fmt_items <- gsub(",", ")`, `after_stat(", items, fixed = TRUE)
  fmt_items <- gsub("|", ")` *or* `after_stat(",
                    fmt_items, fixed = TRUE)
  fmt_items <- paste0("*  `after_stat(", fmt_items, ")`")
  
  # Compose item-list
  fmt_descr <- gsub("\n", "", descr)
  fmt_list  <- paste(fmt_items, fmt_descr, sep = "\\cr ")
  
  c(
    preamble,
    fmt_list,
    if (!is.null(extra_note)) paste0("\n", extra_note)
  )
}

# adapted from {ggplot2}
rd_sec_aesthetics <- function(..., extra_note = NULL) {
  args  <- list(...)
  names <- names(args)
  items <- lapply(
    args,
    function (x) {
      req <- x$required_aes
      req <- sub("|", "} \\emph{or} \\code{", req, fixed = TRUE)
      req_aes <- 
        unlist(strsplit(x$required_aes %||% character(0L), "|", fixed = TRUE))
      optional_aes <- setdiff(x$aesthetics(), req_aes)
      all <- union(req, sort(optional_aes))
      ifelse(all %in% req, paste0("\\strong{\\code{", all, "}}"), 
             paste0("\\code{", all, "}"))
    }
  )
  
  paras <- mapply(\(x, y) c(
    paste0(
      "\\code{", x, "()} ",
      "understands the following aesthetics (required aesthetics are in bold):"
    ),
    "\\itemize{",
    paste0("  \\item ", y),
    "}"
  ), x = names, y = items)
  
  c(
    "@section Aesthetics:",
    unlist(unname(paras)),
    if (!is.null(extra_note)) paste0(extra_note, "\n"),
    paste0(
      "Learn more about setting these aesthetics in ",
      "\\code{vignette(\"ggplot2-specs\", package = \"ggplot2\")}."
    )
  )
}
