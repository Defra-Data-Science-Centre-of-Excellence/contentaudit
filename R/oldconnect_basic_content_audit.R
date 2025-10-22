# old_rsc_client <- get_usage_shiny(old_connect_rsc_client, limit = Inf)
# 
# # TO DO: refresh this in the background
# old_apps_usage <- old_rsc_client %>% get_usage_shiny(limit = Inf)
# old_rsc_content <- old_rsc_client %>% get_content()
# # Remove locked users?
# old_rsc_users <- old_rsc_client %>% get_users(limit = Inf) %>%
#   filter(locked == FALSE)
# old_publishers <- old_rsc_users %>% filter(user_role == "publisher")
# old_shiny_apps <- old_rsc_content %>% filter(app_mode == "shiny")
# 
# general_metrics <- list(
#   "Onboarded Users" = nrow(old_rsc_users),
#   "Publishers" = nrow(old_publishers),
#   "Deployments" = nrow(old_rsc_content),
#   "Shiny Apps" = nrow(old_shiny_apps)
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
# #developers_apps_ranking <- create_dev_ranking(rsc_users, rsc_content)