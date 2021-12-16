library(readxl)
library(tidyverse)
library(readr)

df.진학률 <- read.csv('clipboard', header = FALSE, sep = '\t')

df.진학률 <- apply(df.진학률, 2, function(x) gsub(",", "", x))

df.진학률 <- as.data.frame(df.진학률)

df.진학률[, 3:11] <- sapply(df.진학률[, 3:11], as.numeric)

df.진학률 <- df.진학률 |>
  filter(V2 == '고등학교') |>
  select(1, 2, 3, 5) |>
  mutate(진학률 = round(V5/V3 * 100, 1))

colnames(df.진학률) <- c('연도', '학교급', '졸업자', '진학자', '진학율')

df.진학률$연도 <- factor(df.진학률$연도, ordered = TRUE)

ratio <- max(df.진학률$졸업자) / max(df.진학률$진학율)

install.packages('viridis')
library(viridis)

df.진학률 |>
  ggplot() +
  geom_col(aes(x = 연도, y = 졸업자, fill = '졸업자')) +
  geom_col(aes(x = 연도, y = 진학자, fill = '진학자')) +
  #scale_fill_manual(name = '구분', values = c('졸업자' = 'darkgreen', '진학자' = 'green')) +
  geom_line(aes(x = 연도, y = 진학율 * ratio, group = 1, color = '진학율')) +
  geom_point(aes(x = 연도, y = 진학율 * ratio, color = '진학율')) +
  geom_text(aes(x = 연도, y = 진학율 * ratio, label = 진학율), vjust = -1, size = 3) +
  scale_y_continuous(name = "학생수",  labels = scales::comma) + 
  scale_color_manual(name = '', values = c('진학율' = 'blue')) + 
  ##  scale_fill_viridis(name = '구분', discrete = TRUE) + 
  scale_fill_brewer(name = "구분", palette='YlOrRd') +
  theme_classic()


write.csv(df.진학률, 'clipboard')
