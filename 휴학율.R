df_abs <- read.csv('clipboard', header= FALSE, sep = '\t')

df_abs |>
  ggplot(aes(x = as.factor(V1))) +
  geom_line(aes(y = V2, group = 1, color = '전문대학')) +
  geom_point(aes(y = V2, color = '전문대학')) +
  geom_text(aes(y = V2, label = paste0(V2, '%')), vjust = -1) +
  geom_line(aes(y = V5, group = 1, color = '대학')) + 
  geom_point(aes(y = V5, color = '대학')) +
  geom_text(aes(y = V5, label = paste0(V5, '%')), vjust = -1) +
  labs(y = '휴학율(%)', x = '연도') +
  scale_color_manual(name = '구분', values = c('전문대학' = 'red', '대학' = 'blue'))
