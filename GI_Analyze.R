# Retrieving Data
library(rvest)

data1 <- read_html("https://genshin-impact.fandom.com/wiki/Characters/Comparison")
df1 <- data1 %>% html_elements("table") %>% html_table()
df1 <- as.data.frame(df1[2])
myvars <- names(df1) %in% "Icon"
df1 <- df1[!myvars]

data2 <- read_html("https://genshin-impact.fandom.com/wiki/Characters/List")
df2 <- data2 %>% html_elements("table") %>% html_table()
df2 <- as.data.frame(df2[2])
myvars <- names(df2) %in% c("Icon", "Rarity")
df2 <- df2[!myvars]

genshin <- merge(df1, df2, by = "Name")
genshin[genshin==""] = "Unknown"
View(genshin)

# Visualizing in Bar Chart
library(dplyr)
library(ggplot2)
library(ggpubr)

vis <- count(genshin, Element)
vis <- vis[order(vis$n),]
vis$Element <- factor(vis$Element, levels = vis$Element)

visgraph <- ggplot(data = vis, aes(x = Element, y = n)) +
  geom_bar(stat = "identity", color = "blue", fill = "skyblue") +
  theme_bw() +
  labs(title = "Genshin Impact Character Based On Vision", y = "Frequency")

reg <- count(genshin, Nation)
reg <- reg[order(reg$n),]
reg$Nation <- factor(reg$Nation, levels = reg$Nation)

reggraph <- ggplot(data = reg, aes(x = Nation, y = n)) +
  geom_bar(stat = "identity", color = "blue", fill = "skyblue") +
  theme_bw() +
  labs(title = "Genshin Impact Character Based On Region", y = "Frequency")

weap <- count(genshin, Weapon)
weap <- weap[order(weap$n),]
weap$Weapon <- factor(weap$Weapon, levels = weap$Weapon)

weapgraph <- ggplot(data = weap, aes(x = Weapon, y = n)) +
  geom_bar(stat = "identity", color = "blue", fill = "skyblue") +
  theme_bw() +
  labs(title = "Genshin Impact Character Based On Weapon", y = "Frequency")

sex <- count(genshin, Sex)
sex <- sex[order(sex$n),]
sex$Sex <- factor(sex$Sex, levels = sex$Sex)

sexgraph <- ggplot(data = sex, aes(x = Sex, y = n)) +
  geom_bar(stat = "identity", color = "blue", fill = "skyblue") +
  theme_bw()+
  labs(title = "Genshin Impact Character Based On Sex", y = "Frequency")

graph <- ggarrange(visgraph, reggraph, weapgraph, sexgraph,
                  ncol = 2, nrow = 2)
graph

# Characters Stats Visualization
mondstadt <- subset(genshin, Nation == "Mondstadt", select = c(Name, Element, Weapon, ATK, DEF))
liyue <- subset(genshin, Nation == "Liyue", select = c(Name, Element, Weapon, ATK, DEF))
inazuma <- subset(genshin, Nation == "Inazuma", select = c(Name, Element, Weapon, ATK, DEF))

ggplot(mondstadt, aes(x = ATK, y = DEF)) +
  geom_point(aes(color = factor(Element), shape = factor(Weapon)), size = 3) +
  labs(title = "Character From Mondstadt", color = "Element", shape = "Weapon") +
  geom_text(aes(label = Name), size = 2, nudge_y = -7) +
  theme_bw()

ggplot(liyue, aes(x = ATK, y = DEF)) +
  geom_point(aes(color = factor(Element), shape = factor(Weapon)), size = 3) +
  labs(title = "Characters From Liyue", color = "Element", shape = "Weapon") +
  geom_text(aes(label = Name), size = 2, nudge_y = -7) +
  theme_bw()

ggplot(inazuma, aes(x = ATK, y = DEF)) +
  geom_point(aes(color = factor(Element), shape = factor(Weapon)), size = 3) +
  labs(title = "Characters From Inazuma", color = "Element", shape = "Weapon") +
  geom_text(aes(label = Name), size = 2, nudge_y = -7) +
  theme_bw()
  
ggplot(genshin, aes(x = ATK, y = DEF)) +
  geom_point(aes(color = factor(Element), shape = factor(Weapon)), size = 3) +
  labs(title = "All Characters", color = "Element", shape = "Weapon") +
  geom_text(aes(label = Name), size = 2, nudge_y = -7) +
  theme_bw()
