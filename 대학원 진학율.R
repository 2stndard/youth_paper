df_further <- read.csv('clipboard', header= TRUE, sep = '\t')

as_tibble(df_further)

df_further |>
  ggplot(aes(x = as.factor(연도))) +
  geom_line(aes(y = 총계, group = 1, color = '전체')) +
  geom_point(aes(y = 총계, color = '전체')) +
  geom_text(aes(y = 총계, label = paste0(총계, '%')), vjust = -1) +
  geom_line(aes(y = 전문대학, group = 1, color = '전문대학')) + 
  geom_point(aes(y = 전문대학, color = '전문대학')) +
  geom_text(aes(y = 전문대학, label = paste0(전문대학, '%')), vjust = -1) +
  geom_line(aes(y = 대학, group = 1, color = '대학')) + 
  geom_point(aes(y = 대학, color = '대학')) +
  geom_text(aes(y = 대학, label = paste0(대학, '%')), vjust = -1) +
  geom_line(aes(y = 일반대학원, group = 1, color = '일반대학원')) + 
  geom_point(aes(y = 일반대학원, color = '일반대학원')) +
  geom_text(aes(y = 일반대학원, label = paste0(일반대학원, '%')), vjust = -1) +
  labs(y = '진학율(%)', x = '연도') +
  scale_color_manual(name = '구분', values = c('전체' = 'green', '전문대학' = 'red', '대학' = 'blue', '일반대학원' = 'purple'))
