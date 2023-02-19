team <- read_csv("data/teams_meta.csv")
player <- read_csv("data/player_info.csv")

theme_set(theme_bw(base_family = "HiraKakuPro-W3"))

# player height
player %>% 
  ggplot() + 
  geom_histogram(aes(x=height), binwidth = 1)
  ggsave(file="img/fig1.1.png", height = 3, width = 3)

player %>% 
  left_join(team %>% select(team_id, league), by = "team_id") %>% 
  dplyr::filter(league == "J1") %>% 
  ggplot() + 
  geom_histogram(aes(x=height), binwidth = 1) +
  facet_wrap(~team)
  ggsave(file="img/fig1.2.png", height = 5, width = 6)

player %>% 
  left_join(team %>% select(team_id, league), by = "team_id") %>% 
  ggplot() + 
  geom_histogram(aes(x=height, group=league), binwidth = 1) +
  facet_wrap(~league, ncol=1)
  ggsave(file="img/fig1.3.png", height = 5, width = 3)

player %>% 
  left_join(team %>% select(team_id, league), by = "team_id") %>% 
  ggplot() + 
  geom_histogram(aes(x=weight, group=league), binwidth = 1) +
  facet_wrap(~league, ncol=1)
  ggsave(file="img/fig1.4.png", height = 5, width = 3)

player %>% 
  left_join(team %>% select(team_id, league), by = "team_id") %>% 
  ggplot() + 
  geom_point(aes(x=weight, y=height, group=league)) +
  facet_wrap(~league, ncol=1)
  ggsave(file="img/fig1.5.png", height = 5, width = 3)

player %>% 
  left_join(team %>% select(team_id, league), by = "team_id") %>% 
  ggplot() + 
  geom_histogram(aes(x=age), binwidth = 1) +
  facet_wrap(~league)
  ggsave(file="img/fig1.6.png", height = 3, width = 5)
  
player %>% 
  left_join(team %>% select(team_id, league), by = "team_id") %>% 
  dplyr::filter(league == "J1") %>% 
  ggplot(aes(x=height, group=team)) + 
  geom_density() + 
  facet_wrap(~team)
  ggsave(file="img/fig1.7.png", height = 5, width = 6)

player %>% 
  select(height, weight, age) %>% 
  ggpairs(diag = list(continuous="barDiag"))
  ggsave(file="img/fig1.8.png", height = 4, width = 4)

player %>% 
  left_join(team %>% select(team_id, league), by = "team_id") %>% 
  dplyr::filter(league == "J1") %>% 
  ggplot(aes(weight, height, color=age, group="team")) + 
  geom_point() + 
  facet_wrap(~team)
  ggsave(file="img/fig1.9.png", , height = 5, width = 6)

height_plot <- list()
for (i in 1:3) {
  height_plot[[i]] <- player %>% 
    left_join(team %>% select(team_id, league), by = "team_id") %>% 
    dplyr::filter(league == glue("J{i}")) %>%
    ggplot() + 
    geom_density_ridges_gradient(aes(x=height, y=team, fill=stat(x), group=team)) + 
    scale_fill_viridis_c(option="C") + 
    ggtitle(label = glue("J{i}:チームごとの身長の分布"))
}
gridExtra::grid.arrange(
  height_plot[[1]],
  height_plot[[2]],
  height_plot[[3]],
  ncol=1
)

ggsave(
  plot = height_plot[[1]],
  file="img/fig2.1.png", height = 5, width = 6
  )

ggsave(
  plot = height_plot[[2]],
  file="img/fig2.2.png", height = 5, width = 6
)

ggsave(
  plot = height_plot[[3]],
  file="img/fig2.3.png", height = 5, width = 6
)
