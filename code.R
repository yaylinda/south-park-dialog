setwd("~/Developer/south-park-dialog");
library(ggplot2);

data = read.csv("data.csv", header = T);
data$line_count = 1;

data$Character[data$Character == "Mr. Garrison"] = "Garrison"

num_lines_per_season_episode_character = aggregate(
  data$line_count, 
  by = list(
    Season = data$Season,
    Episode = data$Episode,
    Character = data$Character
  ), 
  sum
)

most_lines_per_episode = data.frame(c(), c(), c(), c())

for (seasonNum in 1:max(data$Season)) {
  for (episodeNum in 1:max(data[which(data$Season == seasonNum), ]$Episode)) {
    
    season_subset = num_lines_per_season_episode_character[which(num_lines_per_season_episode_character$Season == seasonNum), ]
    season_episode_subset = season_subset[which(season_subset$Episode == episodeNum), ]
    
    max_lines = max(season_episode_subset$x)
    
    character = season_episode_subset[which(season_episode_subset$x == max_lines), ]$Character
    
    most_lines_per_episode = rbind(most_lines_per_episode, c(seasonNum, episodeNum, max_lines, character))
  }
}

colnames(most_lines_per_episode) = c("Season", "Episode", "LinesSpoken", "Character")

most_lines_per_episode$Season = as.numeric(most_lines_per_episode$Season)
most_lines_per_episode$Episode = as.numeric(most_lines_per_episode$Episode)
most_lines_per_episode$LinesSpoken = as.numeric(most_lines_per_episode$LinesSpoken)
most_lines_per_episode$CharacterCount = 1

cumulative_lines_per_character = aggregate(
  most_lines_per_episode$LinesSpoken,
  by = list(Character = most_lines_per_episode$Character),
  sum
);
colnames(cumulative_lines_per_character) = c("Character", "LinesSpoken");

NUM_CHARACTERS_TO_PLOT = 10;

cumulative_lines_per_character = cumulative_lines_per_character[
  order(-cumulative_lines_per_character$LinesSpoken), 
];
cumulative_lines_per_character_most_talkative = cumulative_lines_per_character[1:NUM_CHARACTERS_TO_PLOT, ]

cumulative_lines_per_character_most_talkative = rbind(
  cumulative_lines_per_character_most_talkative, 
  c("Other", sum(cumulative_lines_per_character[-(1:NUM_CHARACTERS_TO_PLOT), 2]))
)

most_talkative_characters = cumulative_lines_per_character_most_talkative$Character;

most_talkative_characters_colors = 
  c(
    "#00B8C4", # Cartman
    "#EE324B", # Stan
    "#EE6325", # Kyle
    "#4e414d", # Randy
    "#f9f845", # Butters
    "#5862a2", # Jimmy
    "brown", #76564B", # Chef
    "pink", #e8183a", # Terrance
    "#ae70af", # Wendy
    "#57ca26", # Mr. Garrison
    "gray"     # Other
  );

most_lines_per_episode$CharacterLabel = ifelse(
  most_lines_per_episode$Character %in% most_talkative_characters, 
  most_lines_per_episode$Character, 
  'Other'
)

most_lines_per_episode$CharacterLabel = factor(
  most_lines_per_episode$CharacterLabel, 
  levels = most_talkative_characters
)

do_one_plot(
  most_lines_per_episode, 
  "Most Loquatious Character in South Park", 
  "Which character speaks the most lines in each episode?"
)

ggsave(
  paste("plot.png", sep = ""),
  path = "~/Developer/south-park-dialog",
  dpi = 320,
  width = 10,
  height = 10,
  device = "png",
  units = "in"
)

#--------------------------------------
# Plot
#--------------------------------------
do_one_plot = function(data, title, subtitle) {
  ggplot(
    data, 
    aes(
      x = factor(Episode, levels = sort(unique(Episode))), 
      y = factor(Season, levels = rev(sort(unique(Season)))), 
      fill = CharacterLabel,
    )
  ) + 
    geom_tile(color = "white", alpha = 0.8) + 
    geom_text(
      aes(label = paste(CharacterLabel, LinesSpoken, sep = "\n")), 
      size = 2
    ) +
    scale_fill_manual(
      values = most_talkative_characters_colors,
      breaks = most_talkative_characters,
      labels = paste(
        cumulative_lines_per_character_most_talkative$Character,
        " (",
        cumulative_lines_per_character_most_talkative$LinesSpoken, 
        ")",
        sep = ""
      )
    ) + 
    coord_equal(ratio = 1) + 
    labs(
      x = "Episode in Season",
      y = "Season",
      title = title,
      subtitle = subtitle,
      caption = "Visualization by @yaylinda",
      fill = "Character (Total Lines Spoken)"
    ) + 
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      axis.ticks = element_blank(),
      strip.text.y = element_text(size = 12, angle = 180),
      strip.text.x = element_text(size = 12),
      plot.title = element_text(size = 20),
      axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10)),
      axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 10, l = 0)),
      plot.subtitle = element_text(margin = margin(t = 5, r = 0, b = 10, l = 0))
    )
}