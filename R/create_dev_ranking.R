#' create_dev_ranking
#'
#' @param new_connect_rsc_users 
#' @param new_connect_rsc_content 
#'
#' @returns
#' @export
#'
#' @examples
#' @importFrom magrittr %>%
create_dev_ranking <- function(new_connect_rsc_users, new_connect_rsc_content) {
  # Filter out locked users
  active_users <- new_connect_rsc_users %>% filter(locked == FALSE)
  
  # Focus on publishers only
  publishers <- active_users %>% filter(user_role == "publisher")
  
  # Count number of apps per publisher
  app_counts <- new_connect_rsc_content %>%
    group_by(owner_guid) %>%
    summarise(app_count = n(), .groups = "drop")
  
  # Join with publisher info
  ranking <- publishers %>%
    left_join(app_counts, by = c("guid" = "owner_guid")) %>%
    mutate(app_count =  replace_na(app_count, 0)) %>%
    arrange(desc(app_count))
  
  # Optional: select relevant columns
  ranking %>% select(username, email, app_count)
}