# =========================
# Rscript1: Download dataset from OpenAlex to reproduce Figure 6
# =========================

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(stringr)

output_folder <- "openalex_outputs_final"
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

# -------------------------
# Basic parameters
# -------------------------
results_per_page <- 200
pages_per_year <- 2
max_author_sample <- 1000
year_start <- 2000
year_end <- 2020   # keep enough years for citations
physics_id <- "C121332964"

# -------------------------
# Function: get one page from OpenAlex
# -------------------------
get_openalex_data <- function(url) {
  req <- request(url) |>
    req_user_agent("OpenAlex Figure6 reproduction / student project") |>
    req_retry(max_tries = 3)
  
  resp <- req_perform(req)
  raw_txt <- resp_body_string(resp)
  
  # keep nested lists (important)
  jsonlite::fromJSON(raw_txt, simplifyVector = FALSE)
}

# -------------------------
# Function: turn works into table
# -------------------------
extract_works <- function(results) {
  if (is.null(results) || length(results) == 0) {
    return(tibble())
  }
  
  map_dfr(results, function(work) {
    authors <- work$authorships
    
    if (is.null(authors) || length(authors) == 0) {
      return(NULL)
    }
    
    map_dfr(authors, function(a) {
      a_id <- if (!is.null(a$author$id)) as.character(a$author$id) else NA_character_
      a_name <- if (!is.null(a$author$display_name)) as.character(a$author$display_name) else NA_character_
      
      inst_id <- NA_character_
      inst_name <- NA_character_
      
      if (!is.null(a$institutions) && length(a$institutions) > 0) {
        inst <- a$institutions[[1]]
        if (!is.null(inst$id)) inst_id <- as.character(inst$id)
        if (!is.null(inst$display_name)) inst_name <- as.character(inst$display_name)
      }
      
      tibble(
        work_id = if (!is.null(work$id)) as.character(work$id) else NA_character_,
        title = if (!is.null(work$title)) as.character(work$title) else NA_character_,
        year = if (!is.null(work$publication_year)) as.integer(work$publication_year) else NA_integer_,
        citations = if (!is.null(work$cited_by_count)) as.numeric(work$cited_by_count) else NA_real_,
        author_id = a_id,
        author_name = a_name,
        inst_id = inst_id,
        inst_name = inst_name
      )
    })
  })
}

# -------------------------
# STEP 1: sample authors from physics papers
# -------------------------
message("Step 1: collecting sample of authors...")

author_pool <- character()

for (yr in year_start:year_end) {
  message("Year: ", yr)
  
  cursor <- "*"
  
  for (p in seq_len(pages_per_year)) {
    url <- paste0(
      "https://api.openalex.org/works?",
      "filter=concepts.id:", physics_id,
      ",publication_year:", yr,
      "&per-page=", results_per_page,
      "&cursor=", URLencode(cursor, reserved = TRUE),
      "&select=id,title,publication_year,cited_by_count,authorships"
    )
    
    data <- tryCatch(get_openalex_data(url), error = function(e) NULL)
    
    if (is.null(data) || is.null(data$results) || length(data$results) == 0) break
    
    df <- extract_works(data$results)
    
    if (nrow(df) > 0) {
      author_pool <- c(author_pool, df$author_id)
      author_pool <- unique(author_pool[!is.na(author_pool)])
    }
    
    cursor <- data$meta$next_cursor
    if (is.null(cursor) || length(cursor) == 0) break
    if (length(author_pool) >= max_author_sample) break
  }
  
  if (length(author_pool) >= max_author_sample) break
}

author_pool <- author_pool[seq_len(min(length(author_pool), max_author_sample))]

write_csv(
  tibble(author_id = author_pool),
  file.path(output_folder, "sampled_authors.csv")
)

message("Total sampled authors: ", length(author_pool))

if (length(author_pool) == 0) {
  stop("No authors found.")
}

# -------------------------
# STEP 2: get all works for those authors
# -------------------------
message("Step 2: downloading works for each author...")

all_data <- list()

for (i in seq_along(author_pool)) {
  a_id <- author_pool[i]
  
  if (i %% 25 == 0) {
    message("Author ", i, "/", length(author_pool))
  }
  
  cursor <- "*"
  
  repeat {
    url <- paste0(
      "https://api.openalex.org/works?",
      "filter=author.id:", a_id,
      ",concepts.id:", physics_id,
      ",publication_year:>", year_start - 1,
      ",publication_year:<", year_end + 1,
      "&per-page=", results_per_page,
      "&cursor=", URLencode(cursor, reserved = TRUE),
      "&select=id,title,publication_year,cited_by_count,authorships"
    )
    
    data <- tryCatch(get_openalex_data(url), error = function(e) NULL)
    
    if (is.null(data) || is.null(data$results) || length(data$results) == 0) break
    
    df <- extract_works(data$results)
    
    if (nrow(df) > 0) {
      df <- df |> filter(author_id == a_id)
      
      if (nrow(df) > 0) {
        all_data[[length(all_data) + 1]] <- df
      }
    }
    
    cursor <- data$meta$next_cursor
    if (is.null(cursor) || length(cursor) == 0) break
  }
}

works_all <- bind_rows(all_data)

if (nrow(works_all) == 0) {
  stop("No works downloaded.")
}

# basic cleaning
works_all <- works_all |>
  distinct(work_id, author_id, inst_id, .keep_all = TRUE) |>
  filter(
    !is.na(author_id),
    !is.na(inst_id),
    !is.na(year),
    !is.na(citations)
  )

message("Rows after cleaning: ", nrow(works_all))

# -------------------------
# STEP 3: use current citation count as approximate c5
# -------------------------
works_all <- works_all |>
  mutate(c5 = citations)

# -------------------------
# STEP 4: filter authors (basic career filters)
# -------------------------
author_stats <- works_all |>
  group_by(author_id) |>
  summarise(
    first_year = min(year),
    last_year = max(year),
    span = last_year - first_year,
    total_papers = n(),
    total_insts = n_distinct(inst_id),
    .groups = "drop"
  )

valid_authors <- author_stats |>
  filter(
    span >= 10,
    total_papers >= 8,
    total_insts >= 2
  ) |>
  pull(author_id)

works <- works_all |> filter(author_id %in% valid_authors)

message("Rows after author filter: ", nrow(works))

# -------------------------
# STEP 5: rank institutions
# -------------------------
inst_rank <- works |>
  group_by(inst_id, inst_name) |>
  summarise(
    n_papers = n(),
    total_citations = sum(c5, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(total_citations)) |>
  mutate(rank = row_number())

top_n <- min(1000, nrow(inst_rank))

top_inst_ids <- inst_rank |>
  slice_head(n = top_n) |>
  pull(inst_id)

works <- works |> filter(inst_id %in% top_inst_ids)
inst_rank <- inst_rank |> filter(inst_id %in% top_inst_ids)

# -------------------------
# STEP 6: order papers and define stays
# -------------------------
works_ordered <- works |>
  left_join(inst_rank |> select(inst_id, rank), by = "inst_id") |>
  arrange(author_id, year, work_id) |>
  group_by(author_id) |>
  mutate(order = row_number()) |>
  ungroup()

stays_tmp <- works_ordered |>
  group_by(author_id) |>
  mutate(
    prev_inst = lag(inst_id),
    new_block = if_else(is.na(prev_inst) | inst_id != prev_inst, 1L, 0L),
    stay_id = cumsum(new_block)
  ) |>
  ungroup()

stays <- stays_tmp |>
  group_by(author_id, stay_id) |>
  summarise(
    inst_id = first(inst_id),
    inst_name = first(inst_name),
    rank = first(rank),
    start = min(year),
    end = max(year),
    n_papers = n(),
    c5_vals = list(c5),
    .groups = "drop"
  ) |>
  filter(n_papers >= 2)

# -------------------------
# STEP 7: build moves
# -------------------------
moves <- stays |>
  group_by(author_id) |>
  arrange(stay_id, .by_group = TRUE) |>
  mutate(
    next_inst = lead(inst_id),
    next_rank = lead(rank),
    next_vals = lead(c5_vals)
  ) |>
  ungroup() |>
  filter(!is.na(next_inst))

# -------------------------
# STEP 8: compute deltas
# -------------------------
author_all <- works_ordered |>
  group_by(author_id) |>
  summarise(all_c5 = list(c5), .groups = "drop")

moves <- moves |>
  left_join(author_all, by = "author_id") |>
  rowwise() |>
  mutate(
    mean_before = mean(unlist(c5_vals)),
    mean_after = mean(unlist(next_vals)),
    sd_all = sd(unlist(all_c5)),
    delta_r = next_rank - rank,
    delta_c_star = ifelse(sd_all == 0, NA, (mean_after - mean_before) / sd_all)
  ) |>
  ungroup()

# -------------------------
# Save everything
# -------------------------
write_csv(works, file.path(output_folder, "physics_works_clean.csv"))
write_csv(inst_rank, file.path(output_folder, "institution_ranks_5yr.csv"))
write_csv(stays, file.path(output_folder, "stays_5yr.csv"))
write_csv(moves, file.path(output_folder, "moves_raw_5yr.csv"))

message("Done.")