source("Functions.R")

df_long <- 2005:2013 %>% 
  set_names %>% 
  map_df(read_data, "long", .id = "Year")

df_wide <- 2005:2013 %>% 
  set_names %>% 
  map_df(read_data, "wide", .id = "Year") %>% 
  mutate(
    Point_Diff = Points.w - Points.l
  )

# df_long %>%
#   select_if(is.numeric) %>%
#   select(c(
#     -Win, -contains("TD"), -contains("Misc"), -contains("Field Goal"), -contains("XP"), -contains("2XP"), -contains("Kickoff")
#   )) %>%
#   select(-Points, everything(), Points) %>%
#   as.data.frame %>%
#   bestglm::bestglm(method = "exhaustive")
# -(Pass Comp, Punt Ret, Fumble, Tackle For Loss Yard, Sack, QB Hurry, Penalty Yard)

df_points <- df_long %>%
  select_if(is.numeric) %>%
  select(c(
    -Win, -contains("TD"), -contains("Misc"), -contains("Field Goal"), -contains("XP"), -contains("2XP"), -contains("Kickoff"),
    -`Pass Comp`, -`Punt Ret`, -Fumble, -`Tackle For Loss Yard`, -Sack, -`QB Hurry`, -`Penalty Yard`
  ))

lm_points <- lm(Points ~ ., df_points)

lm_points %>% 
  car::avPlots()
# Seem normal

ggplot() + 
  geom_histogram(aes(x = MASS::stdres(lm_points))) +
  labs(
    title = "Histogram of standardized model residuals",
    x = "Standardized Residual",
    y = "Count"
  )
ks.test(MASS::stdres(lm_points), "pnorm")

ggplot() +
  geom_point(aes(x = lm_points$fitted.values, y = lm_points$residuals)) +
  labs(
    title = "Scatterplot of fitted values vs standardized residuals",
    x = "Fitted Values",
    y = "Standardized Residual"
  )
lmtest::bptest(lm_points)

df_long %>% 
  select_if(is.numeric) %>% 
  select(
    Win, 1:15
  ) %>% 
  select(-Win, everything(), Win) %>% 
  as.data.frame %>% 
  bestglm::bestglm(family = binomial, method = "backward")
