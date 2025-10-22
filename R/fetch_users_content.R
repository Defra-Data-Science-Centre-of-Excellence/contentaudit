#' fetch_users_content
#'
#' @param connect_server 
#' @param connect_api_key 
#'
#' @returns
#' @export
#'
#' @examples
#' @importFrom magrittr %>%
fetch_users_content <- function(connect_server = server, connect_api_key = api_key ){
  
  old_connect_rsc_client <-connectapi::connect(
    server = connect_server,
    api_key = connect_api_key
  )
  
  apps_usage <- connectapi::get_usage_shiny(old_connect_rsc_client, limit = Inf)
  
  # TO DO: refresh this in the background
  old_rsc_content <- old_connect_rsc_client %>% 
    connectapi::get_content() %>%
    #mutate(lock = filter(locked == FALSE)) %>%
    dplyr::mutate(owner_username = purrr::map_chr(owner, "username", .default = NA_character_))
  
  apps_published <- old_rsc_content %>%
    dplyr::select(guid, name, title, created_time, last_deployed_time, app_mode, app_role, owner_username)
  
}
