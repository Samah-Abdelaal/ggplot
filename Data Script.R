library(tidyverse)
library(ggplot2)
library(ggthemes)

azz <- read_csv("Kidney.csv")

azz %>%
  group_by(Groups) %>%
  summarise(meancreat = mean(creatinine)) %>%
  ggplot(
    aes(x = Groups,
        y = meancreat)
    )+
  geom_bar(
    width = 0.66,
    stat = "identity",
    color = "black",
    fill = "cadetblue4"
    )+
  labs(x = "Groups",
       y = "Serum Creatinine (mg/dl)") +
  ylim(c(0,2.5)) +
  theme(axis.title.x = element_text(face = "bold",
                                    size = 16,
                                    vjust = -0.5),
        axis.text.x = element_text(face = "bold",
                                   size = 11),
        axis.title.y = element_text(face = "bold",
                                    size = 16,
                                    vjust = 2.5),
        axis.text.y = element_text(face = "bold",
                                   size = 11))



azz %>%
  group_by(Groups) %>%
  summarise(meanurea = mean(urea)) %>%
  ggplot(
    aes(x= Groups,
        y= meanurea)
    )+
  geom_bar(
    width = 0.66,
    stat = "identity",
    color = "black",
    fill = "darkseagreen4"
    )+ 
  labs(x = "Groups",
       y = "Blood Urea Nitrogen (mg/dl)") +
  ylim(c(0,80)) +
  theme(axis.title.x = element_text(face = "bold",
                                    size = 16,
                                    vjust = -0.5),
        axis.text.x = element_text(face = "bold",
                                   size = 11),
        axis.title.y = element_text(face = "bold",
                                    size = 16,
                                    vjust = 2.5),
        axis.text.y = element_text(face = "bold",
                                   size = 11))

azz %>%
  group_by(Groups) %>%
  summarise(meanoptical = mean(optical)) %>%
  ggplot(
    aes(x= Groups,
        y= meanoptical)
  )+
  geom_bar(
    width = 0.66,
    stat = "identity",
    color = "black",
    fill = "violetred4"
  )+ 
  labs(x = "Groups",
       y = "Optical Density") +
  ylim(c(0,0.8)) +
  theme(axis.title.x = element_text(face = "bold",
                                    size = 16,
                                    vjust = -0.5),
        axis.text.x = element_text(face = "bold",
                                   size = 11),
        axis.title.y = element_text(face = "bold",
                                    size = 16,
                                    vjust = 2.5),
        axis.text.y = element_text(face = "bold",
                                   size = 11))

azz %>%
  group_by(Groups) %>%
  summarise(meancol = mean(collagen)) %>%
  ggplot(
    aes(x= Groups,
        y= meancol)
  )+
  geom_bar(
    width = 0.66,
    stat = "identity",
    color = "black",
    fill = "orchid4"
  )+ 
  labs(x = "Groups",
       y = "Area Percentage of Collagen Fibers") +
  ylim(c(0,10)) +
  theme(axis.title.x = element_text(face = "bold",
                                    size = 16,
                                    vjust = -0.5),
        axis.text.x = element_text(face = "bold",
                                   size = 11),
        axis.title.y = element_text(face = "bold",
                                    size = 16,
                                    vjust = 2.5),
        axis.text.y = element_text(face = "bold",
                                   size = 11))
#########################

azz %>%
  group_by(Groups) %>%
  summarise(meancreat = mean(creatinine)) %>%
  ggplot(
    aes(x = Groups,
        y = meancreat)
  )+
  geom_bar(
    width = 0.7,
    stat = "identity",
    color = "black",
  )+
  labs(x = "Groups",
       y = "Serum Creatinine (mg/dl)") +
  ylim(c(0,2.5)) +
  theme(axis.title.x = element_text(face = "bold",
                                    size = 24,
                                    vjust = -0.4),
        axis.text.x = element_text(face = "bold",
                                   size = 20,
                                   vjust = 3),
        axis.title.y = element_text(face = "bold",
                                    size = 24,
                                    vjust = 1.8),
        axis.text.y = element_text(face = "bold",
                                   size = 20),
        axis.ticks.x = element_blank())


azz %>%
  group_by(Groups) %>%
  summarise(meanurea = mean(urea)) %>%
  ggplot(
    aes(x= Groups,
        y= meanurea)
  )+
  geom_bar(
    width = 0.7,
    stat = "identity",
    color = "black",
  )+ 
  labs(x = "Groups",
       y = "Blood Urea Nitrogen (mg/dl)") +
  ylim(c(0,80)) +
  theme(axis.title.x = element_text(face = "bold",
                                    size = 24,
                                    vjust = -0.4),
        axis.text.x = element_text(face = "bold",
                                   size = 20,
                                   vjust = 3),
        axis.title.y = element_text(face = "bold",
                                    size = 24,
                                    vjust = 1.8),
        axis.text.y = element_text(face = "bold",
                                   size = 20),
        axis.ticks.x = element_blank())

geom_text(
  aes(label = round(meanurea,2),
      vjust = -1),
  size = 5
)

azz %>%
  group_by(Groups) %>%
  summarise(meanoptical = mean(optical)) %>%
  ggplot(
    aes(x= Groups,
        y= meanoptical)
  )+
  geom_bar(
    width = 0.7,
    stat = "identity",
    color = "black",
  )+ 
  labs(x = "Groups",
       y = "Optical Density") +
  ylim(c(0,0.8)) +
  theme(axis.title.x = element_text(face = "bold",
                                    size = 24,
                                    vjust = -0.4),
        axis.text.x = element_text(face = "bold",
                                   size = 20,
                                   vjust = 3),
        axis.title.y = element_text(face = "bold",
                                    size = 24,
                                    vjust = 1.8),
        axis.text.y = element_text(face = "bold",
                                   size = 20),
        axis.ticks.x = element_blank())


azz %>%
  group_by(Groups) %>%
  summarise(meancol = mean(collagen)) %>%
  ggplot(
    aes(x= Groups,
        y= meancol)
  )+
  geom_bar(
    width = 0.7,
    stat = "identity",
    color = "black",
  )+ 
  labs(x = "Groups",
       y = "Area Percentage of Collagen Fibers") +
  ylim(c(0,10)) +
  theme(axis.title.x = element_text(face = "bold",
                                    size = 24,
                                    vjust = -0.4),
        axis.text.x = element_text(face = "bold",
                                   size = 20,
                                   vjust = 3),
        axis.title.y = element_text(face = "bold",
                                    size = 24,
                                    vjust = 1.8),
        axis.text.y = element_text(face = "bold",
                                   size = 20),
        axis.ticks.x = element_blank())
