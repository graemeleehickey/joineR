library(ggplot2)
library(joineRML)
library(reshape2)
library(hexSticker)

data(pbc2)
pbc2 <- subset(pbc2, id != "228")

p <- ggplot(aes(x = year, y = log(serBilir)), data = pbc2) +
  geom_line(aes(group = id), alpha = 0.5) +
  geom_line(data = subset(pbc2, id == "11"), colour = "red") +
  geom_line(data = subset(pbc2, id == "96"), colour = "blue") +
  labs(x = "", y = "") +
  theme_void() +
  theme(strip.background = element_blank(),
        strip.text = element_blank())

sticker(p, package = "joineR",
        p_size = 8,
        s_x = 1, s_y = .8, s_width = 1.55, s_height = 0.9,
        #p_color = "#FFFFFFDD",
        h_color = "#C74646",
        h_fill = "#46C7C7")

