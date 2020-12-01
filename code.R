setwd("~/Developer/south-park-dialog")
library(ggplot2)

data = read.csv("data.csv", header = T)
data$line_count = 1

data_aggregated_num_lines_by_character = 
  aggregate(
  data$line_count, 
  by = list(
    Character = data$Character)
  , sum)

data_aggregated_num_lines_by_character$percentage = 
  (data_aggregated_num_lines_by_character$x / sum(data_aggregated_num_lines_by_character$x)) * 100

top_line_characters = data_aggregated_num_lines_by_character[which(data_aggregated_num_lines_by_character$percentage > 0.89), 1]

data_subset_top_line_characters = data[which(data$Character %in% top_line_characters), ]

data_aggregated_num_lines_by_all = 
  aggregate(
    data$line_count, 
    by = list(
      Season = data$Season,
      Episode = data$Episode,
      Character = data$Character)
    , sum)

most_lines_per_episode = data.frame(c(), c(), c(), c())

for (seasonNum in 1:max(data$Season)) {
  for (epNum in 1:max(data[which(data$Season == seasonNum), ]$Episode)) {
    
    season_subset = data_aggregated_num_lines_by_all[which(data_aggregated_num_lines_by_all$Season == seasonNum), ]
    season_episode_subset = season_subset[which(season_subset$Episode == epNum), ]
    
    max_lines = max(season_episode_subset$x)
    
    character = season_episode_subset[which(season_episode_subset$x == max_lines), ]$Character
    
    most_lines_per_episode = rbind(most_lines_per_episode, c(seasonNum, epNum, max_lines, character))
  }
}

colnames(most_lines_per_episode) = c("Season", "Episode", "LinesSpoken", "Character")
most_lines_per_episode$Season = as.numeric(most_lines_per_episode$Season)
most_lines_per_episode$Episode = as.numeric(most_lines_per_episode$Episode)
most_lines_per_episode$LinesSpoken = as.numeric(most_lines_per_episode$LinesSpoken)
most_lines_per_episode$CharacterCount = 1

aggregated_count_characters_with_most_lines = 
  aggregate(
    most_lines_per_episode$CharacterCount, 
    by = list(Character = most_lines_per_episode$Character), 
    sum
  )

most_talkative_characters = aggregated_count_characters_with_most_lines[which(aggregated_count_characters_with_most_lines$x >= 10), ]$Character
most_lines_per_episode$CharacterLabel = ifelse(most_lines_per_episode$Character %in% most_talkative_characters, most_lines_per_episode$Character, '_Other')

do_one_plot(
  most_lines_per_episode, 
  "Most Loquatious Character in South Park", 
  "Which character has the most lines in each episode?"
)
#--------------------------------------
# Plot and save one dataset (one TV show)
#--------------------------------------
do_one_plot = function(data, title, subtitle, height = 10, width = 10) {
  ggplot(
    data, 
    aes(
      x = factor(Episode, levels = sort(unique(Episode))), 
      y = factor(Season, levels = rev(sort(unique(Season)))), 
      fill = CharacterLabel,
    )
  ) + 
    geom_tile(color = "white", alpha = 0.5) + 
    geom_text(aes(label = CharacterLabel), size = 2) +
    scale_fill_discrete() + 
    coord_equal(ratio = 1) + 
    labs(
      x = "Episode in Season",
      y = "Season",
      title = title,
      subtitle = subtitle,
      caption = "Visualization by @yaylinda",
      fill = "Lines Spoken"
    ) + 
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.ticks = element_blank(),
      strip.text.y = element_text(size = 12, angle = 180),
      strip.text.x = element_text(size = 12),
      plot.title = element_text(size = 20)
    )
  
  # ggsave(
  #   paste(gsub("'", "", gsub(" ", "", title, fixed = TRUE)), ".png", sep = ""), 
  #   path = PLOT_SAVE_PATH,
  #   dpi = 320,
  #   width = width,
  #   height = height,
  #   device = "png",
  #   units = "in"
  # )
}