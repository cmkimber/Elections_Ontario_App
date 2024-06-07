setwd("~/Documents/Elections_Ontario_App/elections_ontario_app")

library(tidyverse)
library(sf)
library(shiny)
library(leaflet)
library(DT)
library(glue)
library(ggiraph)
library(ggrepel)


election_and_party <- readRDS("./data/election_and_party.rds")

test_df <- election_and_party %>%
  filter(Year == 2022) %>%
  arrange(desc(Pct.Votes)) %>%
  mutate(Party.Temp = factor(Party.Temp, levels = Party.Temp),
         Party.Temp = fct_relevel(Party.Temp, "Other", after = Inf)) %>%
  arrange(Party.Temp) %>%
  mutate(csum = rev(cumsum(rev(Pct.Votes))),
         pos = Pct.Votes/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Pct.Votes/2, pos))

temp_palette <- test_df$Party.Col

#tooltip_text <- glue("<b>{Party.Temp}</b><br/>",
 #                    "{Votes.Cast} ({round(Pct.Votes, digits = 2)}%)")

# label_pos <- test_df %>%
#   mutate(csum = rev(cumsum(rev(Pct.Votes))),
#          pos = Pct.Votes/2 + lead(csum, 1),
#          pos = if_else(is.na(pos), Pct.Votes/2, pos))


#### THIS WORKS (Factors sort in order on the plot)
# But why? Moving all the aes calls to the geom seems necessary when the geom is interactive but I don't understand the reasoning. ggiraph's own examples place the aes for the data in the ggplot call

test_plot <- ggplot() +
  geom_col_interactive(data = test_df,
                       color = "white",
                       aes(x = 1,
                           y = Pct.Votes,
                           fill = Party.Temp,
                           tooltip = glue("<b>{Party.Temp}</b><br/>",
                                          "{Votes.Cast} ({round(Pct.Votes, digits = 2)}%)")),
                       show.legend = FALSE) + 
  coord_polar(theta = "y", start = 0) +
  geom_label_repel_interactive(data = test_df,
                   aes(x = 1.2, y = pos, label = glue("{Votes.Cast} ({round(Pct.Votes, digits = 1)}%)")),
                   size = 3.5,
                   #box_padding = 0.5,
                   nudge_x = 0.8,
                   #nudge_y = 0.5,
                   show.legend = FALSE) +
  xlim(c(0.1, 2)) + 
  scale_fill_manual(values = temp_palette) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
  
girafe(ggobj = test_plot)

#### THIS DOES NOT WORK (Factor sorting is wrong)

test_plot_2 <- ggplot(data = test_df,
                      stat = "identity",
                      color = "white",
                      aes(x = 1,
                          y = Pct.Votes,
                          fill = Party.Temp)) +
  geom_col_interactive(aes(tooltip = glue("<b>{Party.Temp}</b><br/>",
                                          "{Votes.Cast} ({round(Pct.Votes, digits = 2)}%)")),
                       show.legend = FALSE) + 
  coord_polar(theta = "y") +
  geom_label_repel_interactive(#data = label_pos,
                   aes(y = pos, label = Pct.Votes),
                   size = 4.5,
                   show.legend = FALSE) +
  xlim(c(0.2, 1.5)) + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

girafe(ggobj = test_plot_2)

#### THIS WORKS

ggplot(test_df, aes(x = 1, y = Pct.Votes, fill = Party.Temp)) +
  geom_col(show.legend = FALSE) + 
  coord_polar(theta = "y") +
  xlim(c(0.2, 1.5)) + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
