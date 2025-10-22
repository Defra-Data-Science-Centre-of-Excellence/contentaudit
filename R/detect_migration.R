
#' detect_migration
#'
#' @param data 
#' @param threshold 
#'
#' @returns
#' @export
#'
#' @examples
detect_migration <- function(data, threshold = 0.5) {
  
  # Step 1: Tokenize names and lowercase for consistency
  data <- data %>%
    mutate(
      name_words = str_split(name, "_|-|\\s+"),
      name_lower = tolower(name)
    )
  
  # Step 2: Identify exact matches
  exact_matches <- inner_join(
    data %>% filter(new_connect == "new_server") %>% select(name, owner_username, app_mode),
    data %>% filter(old_connect == "old_server") %>% select(name, owner_username, app_mode),
    by = c("name", "owner_username", "app_mode")
  ) %>%
    distinct()
  
  # Step 3: Mark exact matches
  data <- data %>%
    mutate(
      is_exact_match = if_else(
        new_connect == "new_server" &
          paste(name, owner_username, app_mode) %in%
          paste(exact_matches$name, exact_matches$owner_username, exact_matches$app_mode),
        TRUE, FALSE
      )
    )
  
  # Step 4: Track matched old names to avoid reuse
  matched_old_names <- character(0)
  
  # Step 5: Rowwise fuzzy matching and exact match scoring
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      similarity_score = {
        if (isTRUE(new_connect == "new_server") && is_exact_match) {
          1.0  # perfect match
        } else if (isTRUE(new_connect == "new_server")) {
          candidates <- data %>%
            dplyr::filter(old_connect == "old_server",
                   owner_username == owner_username,
                   app_mode == app_mode,
                   !(name %in% matched_old_names),
                   !paste(name, owner_username, app_mode) %in%
                     paste(exact_matches$name, exact_matches$owner_username, exact_matches$app_mode))
          
          new_words <- unlist(name_words)
          
          scores <- purrr::map_dbl(candidates$name_words, function(old_words) {
            shared <- intersect(tolower(old_words), tolower(new_words))
            avg_len <- mean(c(length(old_words), length(new_words)))
            length(shared) / avg_len
          })
          
          if (length(scores) > 0) max(scores) else NA_real_
        } else {
          NA_real_
        }
      },
      matched_old_name = {
        if (isTRUE(new_connect == "new_server") && is_exact_match) {
          name  # exact match
        } else if (isTRUE(new_connect == "new_server")) {
          candidates <- data %>%
            dplyr::filter(old_connect == "old_server",
                   owner_username == owner_username,
                   app_mode == app_mode,
                   !(name %in% matched_old_names),
                   !paste(name, owner_username, app_mode) %in%
                     paste(exact_matches$name, exact_matches$owner_username, exact_matches$app_mode))
          
          new_words <- unlist(name_words)
          
          scores <- purrr::map_dbl(candidates$name_words, function(old_words) {
            shared <- intersect(tolower(old_words), tolower(new_words))
            avg_len <- mean(c(length(old_words), length(new_words)))
            length(shared) / avg_len
          })
          
          if (length(scores) > 0 && max(scores) >= threshold) {
            best_match <- candidates$name[which.max(scores)]
            matched_old_names <<- c(matched_old_names, best_match)
            best_match
          } else {
            NA_character_
          }
        } else {
          NA_character_
        }
      },
      matched_new_name = if (isTRUE(new_connect == "new_server")) name else NA_character_,
      migration = dplyr::case_when(
        isTRUE(new_connect == "new_server") & is_exact_match ~ "migrated",
        isTRUE(new_connect == "new_server") & !is.na(matched_old_name) ~ "migrated",
        isTRUE(new_connect == "new_server") & is.na(matched_old_name) ~ "new app",
        TRUE ~ NA_character_
      ),
      contains_test_app_inold = stringr::str_detect(tolower(matched_old_name), "test"),
      contains_test_app_innew = stringr::str_detect(tolower(matched_new_name), "test")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-name_lower, -is_exact_match)
  
  return(data)
}

#resultforone <- detect_migration(one_user, threshold = 0.6)

#' detect_migration_and_testapps 
#'
#' @param data 
#' @param threshold 
#'
#' @returns
#' @export
#'
#' @examples
detect_migration_and_testapps <- function(data, threshold = 0.5) {
  
  # Step 1: Tokenize app names and detect "test" in raw name
  data <- data %>%
    dplyr::mutate(
      name_words = stringr::str_split(name, "_|-|\\s+"),
      contains_test = stringr::str_detect(tolower(name), "test")
    )
  
  # Step 2: Create a working copy to track matched old names
  matched_old_names <- character(0)
  
  # Step 3: Compute similarity, migration, matched names, and test flags
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      similarity_score = {
        if (isTRUE(new_connect == "new_server")) {
          candidates <- data %>%
            dplyr::filter(old_connect == "old_server",
                   owner_username == owner_username,
                   app_mode == app_mode,
                   !(name %in% matched_old_names))
          
          new_words <- unlist(dplyr::cur_data()$name_words)
          
          scores <- purrr::map_dbl(candidates$name_words, function(old_words) {
            shared <- intersect(tolower(old_words), tolower(new_words))
            avg_len <- mean(c(length(old_words), length(new_words)))
            length(shared) / avg_len
          })
          
          if (length(scores) > 0) max(scores) else NA_real_
        } else {
          NA_real_
        }
      },
      matched_old_name = {
        if (isTRUE(new_connect == "new_server")) {
          candidates <- data %>%
            dplyr::filter(old_connect == "old_server",
                   owner_username == owner_username,
                   app_mode == app_mode,
                   !(name %in% matched_old_names))
          
          new_words <- unlist(dplyr::cur_data()$name_words)
          
          scores <- purrr::map_dbl(candidates$name_words, function(old_words) {
            shared <- intersect(tolower(old_words), tolower(new_words))
            avg_len <- mean(c(length(old_words), length(new_words)))
            length(shared) / avg_len
          })
          
          if (length(scores) > 0 && max(scores) >= threshold) {
            best_match <- candidates$name[which.max(scores)]
            matched_old_names <<- c(matched_old_names, best_match)
            best_match
          } else {
            NA_character_
          }
        } else {
          NA_character_
        }
      },
      matched_new_name = if (isTRUE(new_connect == "new_server")) name else NA_character_,
      migration = dplyr::case_when(
        isTRUE(new_connect == "new_server") & !is.na(matched_old_name) ~ "migrated",
        isTRUE(new_connect == "new_server") & similarity_score == 0 ~ "new app",
        isTRUE(new_connect == "new_server") ~ "not migrated",
        TRUE ~ NA_character_
      ),
      contains_test_app_inold = stringr::str_detect(tolower(matched_old_name), "test"),
      contains_test_app_innew = stringr::str_detect(tolower(matched_new_name), "test")
    ) %>%
    dplyr::ungroup()
  
  return(data)
}