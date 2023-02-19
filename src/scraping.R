top_url <- config$urls$top
club_top_url <- config$urls$`club-top`
club_top_html <- rvest::read_html(club_top_url)

# create meta table
teams_html <- list(
  j1_html = list(
    html = html_elements(club_top_html, xpath = "/html/body/div[7]/div[1]/section/section[1]/ul/li"),
    league = "J1"
  ),
  j2_html = list(
    html = html_elements(club_top_html, xpath = "/html/body/div[7]/div[1]/section/section[2]/ul/li"),
    league = "J2"
  ),
  j3_html = list(
    html = html_elements(club_top_html, xpath = "/html/body/div[7]/div[1]/section/section[3]/ul/li"),
    league = "J3"
  )
)

teams_meta <- teams_html %>% 
  purrr::map(function(e){
    team_text <- e$html %>% html_text()
    team <- str_sub(team_text, end = str_length(team_text) %/% 2)
    description_url <- e$html %>% 
      html_elements("a") %>% 
      html_attr("href") %>% 
      str_sub(start=2)
    league <- e$league
    res <- tibble(team = team, 
                  description_url = description_url,
                  league = league)
    return(res)
  }) %>% 
  bind_rows() %>% 
  mutate(team_id = 1:n())

# team details
pjs <- webdriver::run_phantomjs()
ses <- webdriver::Session$new(port = pjs$port)

detail_list <- list()
detail_colnames <- c(
  "number", "omit", "is_HG", "name", "position", "birth_from", "birth_day",
  "height_weight", "matchs_played", "goals"
)

for(i in 1:nrow(teams_meta)) {
  
  team_id <- teams_meta$id[i]
  team_name <- teams_meta$team[i]
  playerData_url <- file.path(top_url, teams_meta$description_url[i], "#player", fsep = "")
  
  ses$go(playerData_url)
  Sys.sleep(3) # wait 3 sec for reading all content
  detail_html <- ses$getSource()
  
  update_date <- read_html(detail_html) %>% 
    html_nodes(".clubResultDate") %>% 
    html_text()
  
  player_table <- read_html(detail_html) %>% 
    html_node(".playerDataTable") %>% 
    html_table(header = 1) %>% 
    magrittr::set_colnames(detail_colnames) %>% 
    mutate(
      team_id = team_id,
      team = team_name,
      update_date = update_date,
      height = as.numeric(str_split(player_table$height_weight, "/", simplify = T)[,1]),
      weight = as.numeric(str_split(player_table$height_weight, "/", simplify = T)[,2]),
      is_HG = is_HG == "HG"
    ) %>% 
    select(-omit)
  
  detail_list[[i]] <- player_table
  msg <- glue("team: {team_name} complete.")
  message(msg)
}

ses$delete()
remove(pjs)

player_info <- detail_list %>% bind_rows()
player_info <- player_info %>% 
  bind_cols(
    player_info$birth_day %>% 
      str_split("/", simplify = T) %>%
      as_tibble() %>% 
      mutate(across(everything(), as.numeric)) %>% 
      magrittr::set_colnames(c("birth_year", "birth_month", "birth_date"))
  ) %>% 
  mutate(
    update_date = update_date %>% 
      strptime("%Y年%m月%d") %>% 
      as.character()
  ) %>% 
  select(-height_weight) %>% 
  mutate(
    birth_day = paste(birth_year, 
                      str_pad(birth_month, side="left", pad="0", width=2) , 
                      str_pad(birth_date, side="left", pad="0", width=2), 
                      sep="-"),
    age = floor(lubridate::as_date(birth_day) %--% lubridate::today() / years(1))
  )

# write data
teams_meta %>% write.csv("data/teams_meta.csv", row.names = FALSE)
player_info %>% write.csv("data/player_info.csv", row.names = FALSE)
