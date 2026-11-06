# =============================================================================
# Reproducible Workflow for LDA Topic Modeling and Topic-Network Analysis
# =============================================================================
#
# This script provides a reusable workflow for:
#   1. Importing and preparing a text corpus
#   2. Tokenizing and filtering text
#   3. Creating a document-feature matrix
#   4. Selecting the number of topics
#   5. Estimating LDA models
#   6. Extracting topic keywords and representative documents
#   7. Constructing a topic-similarity network
#   8. Examining topic prevalence over time
#
# Before running:
#   - Set the input file and column names in Section 2.
#   - Check the date format and text language.
#   - Review stopwords and document-frequency thresholds.
#   - Choose model parameters appropriate to your corpus and hardware.
#
# Note: This is a workflow template, not a one-click analysis. The researcher
# remains responsible for preprocessing decisions, model validation, topic
# interpretation, and reporting.
# =============================================================================


# 1. Packages and global options -----------------------------------------------

options(scipen = 999)

required_packages <- c(
  "readr", "readxl", "dplyr", "tidyr", "stringr", "stringi",
  "ggplot2", "lubridate", "tidytext", "stopwords", "quanteda",
  "topicmodels", "ldatuning", "doParallel", "foreach",
  "parallel", "igraph", "scales", "lsa", "openxlsx"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Please install the following packages first: ",
    paste(missing_packages, collapse = ", ")
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))


# 2. User configuration ---------------------------------------------------------

# Edit these settings for your own project.
input_file <- "path/to/your_corpus.csv"
output_dir <- "output"

text_column <- "text"       # Name of the column containing document text
date_column <- "date"       # Name of the date column; set to NULL if unavailable
document_id_column <- NULL  # Set to an existing ID column, or leave NULL
date_format <- "%m/%d/%Y"   # Example: "%m/%d/%Y", "%d/%m/%Y", "%Y-%m-%d"

language <- "en"
min_characters <- 3
min_document_frequency <- 0.001
max_document_frequency <- 0.50

# Add corpus-specific terms here, if needed.
custom_stopwords <- character(0)

# LDA model settings
candidate_topics <- c(5, seq(10, 50, by = 5))
candidate_alpha <- c(25, 10, 5, 2, 1)
cv_folds <- 5
random_seed <- 1234
n_cores <- max(1, parallel::detectCores() - 2)

# Final model settings (revise after model selection)
final_k <- 20
final_alpha <- 1 / final_k
final_iterations <- 2000
final_burnin <- 1000
final_thin <- 100

# Output settings
n_top_words <- 30
n_representative_documents <- 30

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)


# 3. Import and inspect data ----------------------------------------------------

if (!file.exists(input_file)) {
  stop("Input file not found. Please update `input_file` in Section 2.")
}

file_extension <- tolower(tools::file_ext(input_file))

data <- switch(
  file_extension,
  csv = readr::read_csv(input_file, show_col_types = FALSE),
  xlsx = readxl::read_excel(input_file),
  xls = readxl::read_excel(input_file),
  stop("Unsupported file type. Please provide a CSV or Excel file.")
)

if (!text_column %in% names(data)) {
  stop("The specified text_column was not found in the input data.")
}

if (!is.null(date_column) && !date_column %in% names(data)) {
  stop("The specified date_column was not found in the input data.")
}

data <- data %>%
  filter(!is.na(.data[[text_column]])) %>%
  mutate(document_id = row_number())

if (!is.null(document_id_column)) {
  if (!document_id_column %in% names(data)) {
    stop("The specified document_id_column was not found.")
  }
  data$document_id <- as.character(data[[document_id_column]])
}

data[[text_column]] <- as.character(data[[text_column]])

# Remove URLs. Review this step if URLs are substantively meaningful.
data[[text_column]] <- stringr::str_remove_all(
  data[[text_column]],
  "https?://\\S+|www\\.\\S+"
)

if (!is.null(date_column)) {
  data$date_parsed <- as.Date(data[[date_column]], format = date_format)
  data$date_month <- lubridate::floor_date(data$date_parsed, unit = "month")
  data$date_year <- lubridate::floor_date(data$date_parsed, unit = "year")
}

# Remove duplicate documents.
data <- data %>%
  distinct(.data[[text_column]], .keep_all = TRUE)

# Keep a copy of the prepared document-level data.
data_backup <- data


# 4. Descriptive corpus checks -------------------------------------------------

if (!is.null(date_column)) {
  document_frequency <- data %>%
    filter(!is.na(date_parsed)) %>%
    count(date_parsed, name = "n_documents")

  p_daily <- ggplot(document_frequency, aes(x = date_parsed, y = n_documents)) +
    geom_col() +
    theme_bw() +
    labs(x = "Date", y = "Number of documents", title = "Document frequency")

  ggsave(
    file.path(output_dir, "document_frequency_daily.png"),
    p_daily, width = 10, height = 5, dpi = 300
  )

  monthly_frequency <- data %>%
    filter(!is.na(date_month)) %>%
    count(date_month, name = "n_documents")

  p_monthly <- ggplot(monthly_frequency, aes(x = date_month, y = n_documents)) +
    geom_col() +
    theme_bw() +
    labs(x = "Month", y = "Number of documents",
         title = "Monthly document frequency")

  ggsave(
    file.path(output_dir, "document_frequency_monthly.png"),
    p_monthly, width = 10, height = 5, dpi = 300
  )
}

data$n_words <- stringr::str_count(data[[text_column]], "\\\\w+")
summary(data$n_words)


# 5. Tokenization and text preprocessing ---------------------------------------

stopword_list <- unique(c(
  stopwords::stopwords(language),
  stopwords::stopwords("en", source = "smart"),
  custom_stopwords
))

# Tokenize documents and apply general-purpose filters.
token_data <- data %>%
  select(document_id, all_of(text_column)) %>%
  rename(text = all_of(text_column)) %>%
  tidytext::unnest_tokens(word, text) %>%
  anti_join(tibble(word = stopword_list), by = "word") %>%
  mutate(word_length = nchar(word)) %>%
  filter(
    word_length >= min_characters,
    !str_detect(word, "[0-9]"),
    str_detect(word, "^[[:alpha:]]+$")
  )

# Calculate document frequency for each token.
n_documents <- n_distinct(token_data$document_id)

word_document_frequency <- token_data %>%
  distinct(document_id, word) %>%
  count(word, name = "document_count") %>%
  mutate(document_frequency = document_count / n_documents)

# Retain terms occurring in a specified proportion of documents.
retained_terms <- word_document_frequency %>%
  filter(
    document_frequency >= min_document_frequency,
    document_frequency <= max_document_frequency
  ) %>%
  select(word)

token_data_filtered <- token_data %>%
  semi_join(retained_terms, by = "word")

# Inspect frequent terms before proceeding.
top_terms <- token_data_filtered %>%
  count(word, sort = TRUE)

p_top_terms <- top_terms %>%
  slice_max(order_by = n, n = 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = n, y = word)) +
  geom_col() +
  theme_bw() +
  labs(x = "Token frequency", y = NULL, title = "Most frequent terms")

ggsave(
  file.path(output_dir, "top_terms.png"),
  p_top_terms, width = 8, height = 7, dpi = 300
)

# IMPORTANT: inspect the retained terms and revise stopwords or filters if needed.


# 6. Document-feature matrix ---------------------------------------------------

document_term_matrix <- token_data_filtered %>%
  count(document_id, word, name = "term_count") %>%
  quanteda::cast_dfm(
    document = document_id,
    term = word,
    value = term_count
  )

# Remove empty documents and retain the document IDs in matrix order.
document_term_matrix <- quanteda::dfm_trim(
  document_term_matrix,
  min_termfreq = 1
)

lda_input <- quanteda::convert(
  document_term_matrix,
  to = "topicmodels"
)

saveRDS(lda_input, file.path(output_dir, "lda_input.rds"))


# 7. Select the number of topics: cross-validation ------------------------------

# Assign documents to folds.
set.seed(random_seed)
n_lda_documents <- nrow(lda_input)
fold_assignment <- sample(
  rep(seq_len(cv_folds), length.out = n_lda_documents)
)

validation_grid <- expand.grid(
  k = candidate_topics,
  alpha = candidate_alpha,
  fold = seq_len(cv_folds)
) %>%
  arrange(desc(k))

run_lda_validation <- function(
    dtm,
    validation_grid,
    fold_assignment,
    n_cores = 2,
    seed = 1234
) {
  cluster <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cluster)

  on.exit({
    parallel::stopCluster(cluster)
    foreach::registerDoSEQ()
  }, add = TRUE)

  foreach::registerDoSEQ()
  parallel::clusterEvalQ(cluster, {
    library(topicmodels)
  })

  parallel::clusterExport(
    cluster,
    varlist = c("dtm", "validation_grid", "fold_assignment", "seed"),
    envir = environment()
  )

  results <- foreach::foreach(
    i = seq_len(nrow(validation_grid)),
    .combine = rbind,
    .packages = "topicmodels"
  ) %dopar% {
    settings <- validation_grid[i, ]

    train_dtm <- dtm[fold_assignment != settings$fold, ]
    test_dtm <- dtm[fold_assignment == settings$fold, ]

    fitted_model <- topicmodels::LDA(
      train_dtm,
      k = settings$k,
      method = "Gibbs",
      control = list(alpha = settings$alpha, seed = seed)
    )

    data.frame(
      k = settings$k,
      alpha = settings$alpha,
      fold = settings$fold,
      perplexity = topicmodels::perplexity(
        fitted_model,
        newdata = test_dtm
      )
    )
  }

  as.data.frame(results)
}

# Run only when cross-validation is needed.
# validation_results <- run_lda_validation(
#   dtm = lda_input,
#   validation_grid = validation_grid,
#   fold_assignment = fold_assignment,
#   n_cores = n_cores,
#   seed = random_seed
# )
# saveRDS(validation_results, file.path(output_dir, "validation_results.rds"))


# 8. Optional topic-number diagnostics -----------------------------------------

# The ldatuning package provides several topic-number diagnostics.
# These metrics should be treated as complementary evidence, not as an
# automatic decision rule.
#
# topic_number_results <- ldatuning::FindTopicsNumber(
#   lda_input,
#   topics = candidate_topics,
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(alpha = final_alpha, seed = random_seed),
#   mc.cores = n_cores,
#   verbose = TRUE
# )
#
# p_topic_number <- ldatuning::FindTopicsNumber_plot(topic_number_results)
# ggsave(
#   file.path(output_dir, "topic_number_diagnostics.png"),
#   p_topic_number, width = 10, height = 7, dpi = 300
# )


# 9. Estimate the final LDA model -----------------------------------------------

set.seed(random_seed)

lda_model <- topicmodels::LDA(
  lda_input,
  k = final_k,
  method = "Gibbs",
  control = list(
    alpha = final_alpha,
    iter = final_iterations,
    burnin = final_burnin,
    thin = final_thin,
    seed = random_seed
  )
)

saveRDS(lda_model, file.path(output_dir, "lda_model.rds"))
print(summary(lda_model))


# 10. Extract topic keywords ---------------------------------------------------

get_top_words <- function(model, n_words = 30) {
  beta_matrix <- exp(topicmodels::terms(model, n = n_words))
  as.data.frame(beta_matrix, check.names = FALSE)
}

# Extract the highest-probability terms for each topic.
topic_word_probabilities <- tidytext::tidy(lda_model, matrix = "beta")

top_words <- topic_word_probabilities %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = n_top_words, with_ties = FALSE) %>%
  arrange(topic, desc(beta)) %>%
  ungroup()

readr::write_csv(
  top_words,
  file.path(output_dir, "topic_top_words.csv")
)

# FREX keyword extraction.
get_frex_words <- function(model, n_words = 30, weight = 0.3) {
  beta <- exp(model@beta)
  beta <- t(beta)

  word_frequency <- rowSums(beta)
  frex <- 1 / (
    weight / (beta / word_frequency) +
      (1 - weight) / beta
  )

  output <- lapply(seq_len(ncol(frex)), function(topic_id) {
    selected <- order(frex[, topic_id], decreasing = TRUE)[
      seq_len(min(n_words, nrow(frex)))
    ]

    data.frame(
      topic = topic_id,
      rank = seq_along(selected),
      term = rownames(frex)[selected],
      frex = frex[selected, topic_id]
    )
  })

  bind_rows(output)
}

frex_words <- get_frex_words(
  lda_model,
  n_words = n_top_words
)

readr::write_csv(
  frex_words,
  file.path(output_dir, "topic_frex_words.csv")
)


# 11. Extract representative documents -----------------------------------------

get_representative_documents <- function(
    model,
    document_data,
    text_column = "text",
    id_column = "document_id",
    n_documents = 30
) {
  theta <- as.data.frame(model@gamma)
  theta$document_id <- as.character(model@documents)

  document_data <- document_data %>%
    mutate(document_id = as.character(.data[[id_column]]))

  matched_data <- document_data %>%
    inner_join(theta, by = "document_id")

  topic_columns <- as.character(seq_len(model@k))

  output <- lapply(topic_columns, function(topic_id) {
    matched_data %>%
      arrange(desc(.data[[topic_id]])) %>%
      slice_head(n = n_documents) %>%
      transmute(
        topic = as.integer(topic_id),
        document_id,
        topic_probability = .data[[topic_id]],
        text = .data[[text_column]]
      )
  })

  bind_rows(output)
}

# Match document IDs in the original data to the order used by the DTM.
# Check this alignment before interpreting representative documents.
representative_documents <- get_representative_documents(
  model = lda_model,
  document_data = data,
  text_column = text_column,
  id_column = "document_id",
  n_documents = n_representative_documents
)

readr::write_csv(
  representative_documents,
  file.path(output_dir, "representative_documents.csv")
)


# 12. Construct a topic-similarity network -------------------------------------

# The network is based on cosine similarity between topic distributions
# across documents (the columns of the document-topic matrix).
build_topic_network <- function(
    model,
    topic_labels = NULL,
    topic_sizes = NULL,
    remove_topics = integer(0),
    output_file = NULL
) {
  theta <- model@gamma
  colnames(theta) <- as.character(seq_len(model@k))

  similarity_matrix <- lsa::cosine(t(as.matrix(theta)))

  if (is.null(dim(similarity_matrix))) {
    stop("Could not calculate a topic similarity matrix.")
  }

  network <- igraph::graph_from_adjacency_matrix(
    similarity_matrix,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE
  )

  if (!is.null(topic_labels)) {
    if (length(topic_labels) != model@k) {
      stop("topic_labels must contain one label per topic.")
    }
    igraph::V(network)$name <- topic_labels
  } else {
    igraph::V(network)$name <- paste("Topic", seq_len(model@k))
  }

  if (!is.null(topic_sizes)) {
    if (length(topic_sizes) != model@k) {
      stop("topic_sizes must contain one value per topic.")
    }
    igraph::V(network)$topic_size <- topic_sizes
  }

  if (length(remove_topics) > 0) {
    network <- igraph::delete_vertices(network, remove_topics)
  }

  # Community detection algorithms.
  igraph::V(network)$louvain <- igraph::membership(
    igraph::cluster_louvain(network)
  )
  igraph::V(network)$walktrap <- igraph::membership(
    igraph::cluster_walktrap(network)
  )
  igraph::V(network)$fast_greedy <- igraph::membership(
    igraph::cluster_fast_greedy(network)
  )

  if (!is.null(output_file)) {
    igraph::write_graph(
      network,
      paste0(output_file, ".graphml"),
      format = "graphml"
    )
  }

  network
}

# Topic size can be calculated as the sum of document-topic probabilities.
topic_sizes <- colSums(lda_model@gamma)

# Optional: supply labels after manually interpreting the topics.
# topic_labels <- paste("Topic", seq_len(final_k))
# topic_network <- build_topic_network(
#   model = lda_model,
#   topic_labels = topic_labels,
#   topic_sizes = topic_sizes,
#   remove_topics = integer(0),
#   output_file = file.path(output_dir, "topic_network")
# )


# 13. Aggregate topic prevalence into higher-level groups -----------------------

# After inspecting the topic network, define groups of topics if theoretically
# and empirically justified. Replace the example vectors below.
#
# group_1_topics <- c(1, 2, 3)
# group_2_topics <- c(4, 5, 6)
#
# theta <- as.data.frame(lda_model@gamma)
# theta$document_id <- as.character(lda_model@documents)
#
# theta$group_1 <- rowSums(theta[, as.character(group_1_topics), drop = FALSE])
# theta$group_2 <- rowSums(theta[, as.character(group_2_topics), drop = FALSE])


# 14. Examine topic prevalence over time ----------------------------------------

# This section requires a date variable and topic/group prevalence variables.
# Example below uses one topic's document-level prevalence.
#
# if (!is.null(date_column)) {
#   theta <- as.data.frame(lda_model@gamma)
#   theta$document_id <- as.character(lda_model@documents)
#
#   date_data <- data %>%
#     mutate(document_id = as.character(document_id)) %>%
#     inner_join(theta, by = "document_id")
#
#   topic_to_plot <- "1"
#
#   temporal_data <- date_data %>%
#     filter(!is.na(date_parsed)) %>%
#     group_by(date_month) %>%
#     summarise(
#       mean_prevalence = mean(.data[[topic_to_plot]], na.rm = TRUE),
#       .groups = "drop"
#     )
#
#   p_temporal <- ggplot(
#     temporal_data,
#     aes(x = date_month, y = mean_prevalence)
#   ) +
#     geom_line(alpha = 0.5) +
#     geom_smooth(se = FALSE) +
#     theme_bw() +
#     labs(
#       x = "Date",
#       y = "Mean topic prevalence",
#       title = "Topic prevalence over time"
#     )
#
#   ggsave(
#     file.path(output_dir, "topic_prevalence_over_time.png"),
#     p_temporal, width = 10, height = 5, dpi = 300
#   )
# }


# 15. Save session information -------------------------------------------------

writeLines(
  capture.output(sessionInfo()),
  con = file.path(output_dir, "session_info.txt")
)

message("Workflow completed. Review all outputs in: ", output_dir)
