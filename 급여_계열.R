df_소득_계열 <- read_excel('./이기준소장님_요청자료211223.xlsx', col_names= FALSE, sheet = '최근3년간 초임급여', skip = 10, col_type = c('numeric','text','text', rep('numeric', 3)))

colnames(df_소득_계열) <- c('연도', '구분', '계열','분석대상', '평균소득', '중위소득')

df_소득_계열$구분 <- fct_relevel(df_소득_계열$구분, '전체', '대학', '석사', '박사')

df_소득_계열$계열 <- fct_relevel(df_소득_계열$계열, '총계', '인문', '사회', '교육', '공학', '자연', '의약', '예체능')

df_소득_계열 <- df_소득_계열 |>
  gather(key, value, 4:6)

df_소득_계열 |>
  filter(key != '분석대상', 구분 == '전체') |>
  ggplot(aes(x = as.factor(연도))) +
  geom_line(aes(y = value/1000, group = key, color = key)) +
  geom_point(aes(y = value/1000, color = key)) +
  geom_text(aes(y = value/1000, label = scales::comma(value/1000)), vjust = -1) +
  facet_wrap(~계열) + 
  labs(x = '연도', y = '급여(천원)') + 
  scale_y_continuous(labels = scales::comma)
