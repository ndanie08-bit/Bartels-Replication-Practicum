##Natalie Daniels
##Data wrangling for PP
##Oct. 9 2026

##Midterm
##Replicate Figures 1 and 2 from Bartels (2000), “Partisanship and Voting 
##Behavior”, for the years 2006–2024 using the Cumulative CCES Dataset.
##replicating figure 1 in this script

###load packages
install.packages("tidyverse")
library(tidyverse)
library(haven)

###Getting files and setting working directory
setwd("~/DATA WRANG/MIDTERM")

ces <- read_dta("cumulative_2006-2024.dta")
ces

#figure 1!

###find the variables
unique(ces$year) #year
unique(ces$pid7) #pid7

###keeping those names above year and pid7

#1. Filter and select
pid_data <- ces |> select(year, pid7)

table(as_factor(pid_data$pid7))

pid_data <- ces %>%
  select(year, pid7) %>%
  filter(year >= 2006 & year <= 2024)

#2. Mutate

###drp the DK
ces <- ces %>%
  mutate(pid = if_else(pid7 == 3, NA_real_, pid7))

table(ces$pid, useNA = "ifany")

###mutate and assign 1 and 0
ces <- ces %>%
  mutate(
    strong = case_when(
      pid7 %in% c(1, 7) ~ 1,
      pid7 %in% 2:6 ~ 0
    ),
    weak = case_when(
      pid7 %in% c(2, 6) ~ 1,
      pid7 %in% c(1, 3, 4, 5, 7) ~ 0
    ),
    leaners = case_when(
      pid7 %in% c(3, 5) ~ 1,
      pid7 %in% c(1, 2, 4, 6, 7) ~ 0
    ),
    indeps = case_when(
      pid7 == 4 ~ 1,
      pid7 %in% c(1, 2, 3, 5, 6, 7) ~ 0
    )
  )

#3. plot 1A
figure1_data <- ces %>%
  group_by(year) %>%
  summarize(
    strong = mean(strong, na.rm = TRUE),
    weak = mean(weak, na.rm = TRUE),
    leaners = mean(leaners, na.rm = TRUE),
    indeps = mean(indeps, na.rm = TRUE)
  )

###Plot
library(ggplot2)


endyear<-2024
caption_text<-"Presidential Election Years Only, Bartels Years"


bartels1A <- ggplot(figure1_data, aes(x = year)) +
  geom_line(aes(y = strong), color = "blue") +
  scale_y_continuous(
    limits = c(0, 0.5),
    breaks = seq(0, 0.5, 0.1),
    minor_breaks = seq(0, 0.5, 0.05),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = seq(min(figure1_data$year), endyear, 8),
    minor_breaks = seq(min(figure1_data$year), endyear, 4),
    name = "Year"
  ) +
  geom_hline(
    yintercept = seq(0.1, 0.5, 0.1),
    linetype = "dotted",
    linewidth = 0.2,
    color = "black"
  ) +
  geom_vline(
    xintercept = seq(min(figure1_data$year), endyear, by = 8),
    linetype = "dotted",
    linewidth = 0.2,
    color = "black"
  ) +

#4.top half of figure 1 

#Strong: connected (solid line + filled points)
geom_line(aes(y = strong, linetype = "Strong Identifiers"), linewidth = 0.7, color = "black") +
  geom_point(aes(y = strong, shape = "Strong Identifiers"), size = 2.2, stroke = 0.8,
             color = "black", fill = "black") +
  
#Weak
geom_line(aes(y = weak, linetype = "Weak Identifiers"), linewidth = 0.7, color = "black") +
geom_point(aes(y = weak, shape = "Weak Identifiers"), size = 2.2, stroke = 0.9,
             color = "black", fill = NA) +

#Titles and labeling
labs(
  title = paste0("Distribution of Party Identification 2006-", endyear),
  subtitle = "Proportions of National Election Study Sample",
  caption = caption_text 
) +

# Map linetypes and shapes to series (order matches legend order)
scale_linetype_manual(
  values = c("Strong Identifiers" = "solid",
             "Weak Identifiers"   = "dashed"),
  guide = guide_legend(order = 1)
) +
  scale_shape_manual(
    values = c("Strong Identifiers" = 16,   # filled circle
               "Weak Identifiers"   = 1),   # hollow circle
    guide = guide_legend(order = 1)
  ) +
  
  # Legend: one column, inside the plot
  guides(
    linetype = guide_legend(ncol = 1, title = NULL),
    shape    = guide_legend(ncol = 1, title = NULL)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = c(0.6, 0.2),  # tweak as desired
    legend.background = element_rect(fill = "white", color = "grey70", linewidth = 0.3), # light box
    panel.grid.minor = element_line(linewidth = 0.2),
    panel.grid.major = element_line(linewidth = 0.3),
    plot.title = element_text(face = "bold") 
  ) 

bartels1A

#5. plot 1B

bartels1B<-ggplot(figure1_data, aes(x = year)) +
  #Next, our scales and axes
  scale_y_continuous(
    limits = c(0, 0.5),
    breaks = seq(0, 0.5, 0.1),
    minor_breaks = seq(0, 0.5, 0.05),
    name = NULL
  ) +
  scale_x_continuous(
    breaks = seq(2006, endyear, 8),
    minor_breaks = seq(2006, endyear, 4),
    name = "Year"
  ) +
  # Reference lines
  # Horizontal reference lines at 0.1 ... 0.5 (vvthin + dotted + black)
  geom_hline(yintercept = seq(0.1, 0.5, 0.1),
             linetype = "dotted", linewidth = 0.2, color = "black") +
  # Vertical reference lines at election years (vvthin + dotted + black)
  geom_vline(
    xintercept = seq(2006, endyear, by = 8),
    linetype = "dotted",
    linewidth = 0.2,
    color = "black"
  ) +
  
  # Leaners: dashed + hollow
  geom_line(aes(y = leaners, linetype = "Independent Leaners"), linewidth = 0.7, color = "black") +
  geom_point(aes(y = leaners, shape = "Independent Leaners"),
             size = 2.2, stroke = 0.9, color = "black", fill = NA) +
  
  # Pure Independents: solid + filled
  geom_line(aes(y = indeps, linetype = "Pure Independents"), linewidth = 0.7, color = "black") +
  geom_point(aes(y = indeps, shape = "Pure Independents"),
             size = 2.2, stroke = 0.8, color = "black", fill = "black") +
  
  
  labs(
    title = paste0("Distribution of Party Identification 1952-", endyear), #Paste puts a string in as specified, which lets me use my passed endyear variable
    subtitle = "Proportions of National Election Study Sample",
    caption = caption_text
  ) +
  
  # Map linetypes and shapes to series (order matches your legend order)
  scale_linetype_manual(
    values = c("Independent Leaners" = "dashed",
               "Pure Independents"   = "solid"),
    guide = guide_legend(order = 1)
  ) +
  scale_shape_manual(
    values = c("Independent Leaners" = 1,   # hollow circle
               "Pure Independents"   = 16), # filled circle
    guide = guide_legend(order = 1)
  ) +
  
  # Legend: one column, inside the plot (similar to position(5) ring(0))
  guides(
    linetype = guide_legend(ncol = 1, title = NULL),
    shape    = guide_legend(ncol = 1, title = NULL)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = c(0.4, 0.8),  # tweak as desired
    legend.background = element_rect(fill = "white", color = "grey70", linewidth = 0.3), # light box
    panel.grid.minor = element_line(linewidth = 0.2),
    panel.grid.major = element_line(linewidth = 0.3),
    plot.title = element_text(face = "bold") 
  ) 

bartels1B

#6. combine 1a and 1b
library(patchwork)
bartels1A / bartels1B

#clean using ggplot
bartels1 <- (bartels1A +
               theme(
                 axis.title.x = element_blank(),
                 axis.text.x  = element_blank(),
                 axis.ticks.x = element_blank(),
                 plot.caption = element_blank()
               )
) /
  (bartels1B +
     labs(title = NULL, subtitle = NULL) +
     theme(axis.title.x = element_blank())
  )

bartels1

