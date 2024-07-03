avgSentScoreMileIQ<- Application %>%
  group_by(Time)%>%
  summarise( mean = mean(Sentiment))

avgSentScoreDriversnote<- Application %>%
  group_by(Time)%>%
  summarise( mean = mean(Sentiment))

avgSentScoreStrde<- Application %>%
  group_by(Time)%>%
  summarise( mean = mean(Sentiment))

avgSentScoreMileIQ$Source <- 'App2'
avgSentScoreDriversnote$Source <- 'App1'
avgSentScoreStrde$Source <- 'App3'
combined_data <- bind_rows(avgSentScoreMileIQ, avgSentScoreDriversnote, avgSentScoreStrde)
color_paletteE <- brewer.pal(n = 3, name = "Set2")
# Plot the combined data
sentiment_line_plot <- ggplot(combined_data, aes(x = Time, y = mean, color = Source)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = round(mean, 2)), vjust = -0.5) +
  labs(x = NULL, y = NULL, title = "Average Sentiment Score Over Time", color = NULL) +
  scale_color_manual(values =color_paletteE ) +  # Custom colors for each source
  theme_minimal() +
  scale_x_continuous(breaks = unique(combined_data$Time)) +
  theme_classic(base_family = "Century") +
  ylim(-0.1, 0.6)
sentiment_line_plot
