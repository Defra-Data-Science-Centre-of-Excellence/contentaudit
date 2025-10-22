# library(shiny)
# library(connectapi) # Tested with 0.1.0.9031
# library(connectViz)
# library(dplyr)
# library(bslib)
# library(echarts4r)
# library(toastui)
# library(httr)
# library(tidyr)
# 
# 
# apps_usage <- get_usage_shiny(new_connect_rsc_client, limit = Inf)
# 
# # TO DO: refresh this in the background
# new_connect_rsc_content <- new_connect_rsc_client %>% get_content()
# # Remove locked users?
# new_connect_rsc_users <- new_connect_rsc_client %>% get_users(limit = Inf) %>%
#   filter(locked == FALSE)
# 
# publishers <- new_connect_rsc_users %>% filter(user_role == "publisher")
# shiny_apps <- new_connect_rsc_content %>% filter(app_mode == "shiny")
# 
# general_metrics <- list(
#   "Onboarded Users" = nrow(rsc_users),
#   "Publishers" = nrow(publishers),
#   "Deployments" = nrow(rsc_content),
#   "Shiny Apps" = nrow(shiny_apps)
# )
# 
# general_metrics_cards <- purrr::map(
#   seq_along(general_metrics),
#   function(i) {
#     value_box(
#       theme_color = "secondary",
#       value = general_metrics[[i]],
#       title = names(general_metrics)[[i]]
#     )
#   }
# )
# 
# new_connect_developers_apps_ranking <- create_dev_ranking(new_connect_rsc_users, new_connect_rsc_content)
