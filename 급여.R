df_소득 <- read_excel('./이기준소장님_요청자료.xlsx', col_names= TRUE, sheet = '최근3년간 초임급여', skip = 8, 
                    col_type = c('numeric','text', rep('numeric', 3)))

df_소득$구분 <- fct_relevel(df_소득$구분, '총계', '대학', '석사', '박사')

df_소득 <- df_소득 |>
  gather(key, value, 3:5)

df_소득 |>
  filter(key != '분석대상자') |>
  ggplot(aes(x = as.factor(연도))) +
  geom_line(aes(y = value/1000, group = 구분, color = 구분)) +
  geom_point(aes(y = value/1000, color = 구분)) +
  geom_text(aes(y = value/1000, label = scales::comma(value/1000)), vjust = -1) +
  facet_wrap(~key) + 
  labs(x = '연도', y = '급여(천원)') + 
  scale_y_continuous(labels = scales::comma)



+
  geom_line(aes(y = 중위소득, group = 구분), color = 'blue')

