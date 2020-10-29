library(tidyverse)
read_data <- function(year, format){
  df <- str_c("Data/", year, "/team-game-statistics.csv") %>% 
    read_csv %>% 
    full_join(read_csv(str_c("Data/", year, "/team.csv")), "Team Code") %>% 
    full_join(read_csv(str_c("Data/", year, "/conference.csv")), "Conference Code") %>% 
    filter(Subdivision == "FBS")

  miss_gc <- df %>% 
    count(`Game Code`) %>% 
    filter(n == 1) %>% 
    pull(`Game Code`)
  
  df <- df %>% 
    filter(!(`Game Code` %in% miss_gc))
  
  win <- df %>% 
    arrange(Name.x) %>% 
    group_by(`Game Code`) %>% 
    slice(which.max(Points)) %>% 
    ungroup
  
  if(format == "long"){
    bind_rows(
      win %>% 
        mutate(Win = 1),
      df %>% 
        setdiff(win) %>% 
        mutate(Win = 0)
    ) %>% 
      select(
        Team = Name.x,
        Conference = Name.y,
        Win,
        everything(),
        -`Team Code`,
        -`Conference Code`,
        -Subdivision
      )
  } else if(format == "wide"){
    full_join(
      win,
      df %>% 
        setdiff(win),
      by = "Game Code",
      suffix = c(".w", ".l")
    ) %>% 
      select(
        `Game Code`,
        Team.w = Name.x.w,
        Team.l = Name.x.l,
        Conference.w = Name.y.w,
        Conference.l = Name.y.l,
        everything(),
        -`Team Code.w`,
        -`Team Code.l`,
        -`Conference Code.w`,
        -`Conference Code.l`,
        -Subdivision.w,
        -Subdivision.l
      ) 
  } else {
    print("Options for format are 'wide' or 'long'")
  }
}
