df.중단 <- read.csv('clipboard', header = TRUE, sep = '\t')

df_중단 <- read_excel('./복사본 2 사유별 학업중단자 현황.xlsx', col_names= TRUE, sheet = 'Sheet1', skip = 4, 
                     col_type = c('numeric', 'text', rep('numeric', 9)))


df_중단$조사회차 <- as.factor(round(df_중단$조사회차/100, 0))

df_중단_sum <- df_중단 |>
  bind_rows(
    df_중단 |>
    filter(학교급 %in% c('교육대학', '대학교', '산업대학', '기술대학', '각종대학(대학)', '원격대학(대학)', '사내대학(대학)', '사이버대학(대학)')) |>
      group_by(조사회차) |>
      summarise(across(where(is.character), ~"학부"), across(where(is.numeric), sum))
  ) |>
  bind_rows(
    df_중단 |>
      filter(학교급 %in% c('전문대학', '각종대학(전문)', '원격대학(전문)', '사내대학(전문)', '사이버대학(전문)', '전공대학', '기능대학')) |>
      group_by(조사회차) |>
      summarise(across(where(is.character), ~"전문대"), across(where(is.numeric), sum))
  ) |>
  filter(학교급 %in% c('합계', '학부', '전문대', '방송통신대학', '대학원'))


df_중단_rate <- df_중단_sum |>
  mutate(미등록 = 미등록 / 학업중단자,
         미복학 = 미복학 / 학업중단자,
         자퇴 = 자퇴 / 학업중단자,
         학사경고 = 학사경고 / 학업중단자,
         학생활동 = 학생활동 / 학업중단자,
         유급제적 = 유급제적 / 학업중단자,
         재학연한초과 = 재학연한초과 / 학업중단자,
         기타 = 기타 / 학업중단자
         )
  
df_중단_rate |>
  gather(key, value, 3:11) |>
  mutate(key = fct_relevel(key, '미등록', '미복학', '자퇴', '학사경고', '학생활동', 
                           '유급제적', '재학연한초과', '기타')) |>
  filter(학교급 %in% c('학부', '전문대'), key != '학업중단자') |>
  ggplot(aes(x = 조사회차)) +
  geom_col(aes(y = value, fill = key), position = position_fill(reverse = TRUE)) +
  geom_text(aes(y = value, label = ifelse(value > 0.02, paste0(round(value * 100, 1),"%"), '')), 
            position=position_stack(vjust= 0.5), size = 3) +
  labs(y = '비율(%)', x = '연도') +
  scale_fill_discrete(name = '구분', labels = c('미등록', '미복학', '자퇴', '학사경고', '학생활동', '유급제적', '재학연한초과', '기타')) +
  facet_wrap(~학교급, ncol = 1)

write.csv(df_중단_rate, 'clipboard')
