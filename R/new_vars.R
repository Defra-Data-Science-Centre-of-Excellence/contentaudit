#' new_vars
#'
#' @param data 
#'
#' @returns
#' @export
#'
#' @examples
#' @importFrom magrittr %>%
new_vars <- function(data){
  
  # Named vector of known exceptions
  department_map <- c(
    "Naturalengland" = "Natural England")
  
  new_var_data1 <- data %>%
    dplyr::mutate(a_nona_account = stringr::str_detect(owner_username, "^a-a"), 
                  owner_username = stringr::str_split(owner_username, "@", simplify = TRUE),
                  owner_username = stringr::str_c(stringr::str_to_lower(owner_username[,1]), "@", owner_username[,2]))
  
  new_var_data2 <- new_var_data1 %>%
    dplyr::mutate(department = owner_username %>%
                    stringr::str_split("@", simplify = TRUE) %>%
                    .[,2] %>%
                    stringr::str_remove("\\.gov\\.uk|\\.org\\.uk|\\.co\\.uk") %>%
                    stringr::str_split("\\.", simplify = TRUE) %>%
                    .[,1] %>%
                    stringr::str_replace_all("-", " ") %>%
                    stringr::str_to_title() %>%
                    stringr::str_replace_all(stringr::fixed(department_map))
    )
  
} 