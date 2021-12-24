library(readxl)
library(tidyverse)
library(showtext)
showtext_auto()


df_univ <- read_excel('./복사본 2021년 _대학_12-다-1. 장학금 수혜 현황_학교별자료.xlsx', col_names= TRUE, 
                      col_type = c('numeric', rep('text', 5), rep('numeric', 15)))

df_univ$지역 <- fct_relevel(df_univ$지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')


df_univ |>
  filter(!(학교종류 %in% c('사이버대학(대학)', '방송통신대학'))) |>
  #filter(상태 == '기존') |>
  group_by(기준연도) |>
  summarise(rate = sum(총계)/sum(재학생)) |>
  write.csv('clipboard')

df_univ |>
  filter(!(학교종류 %in% c('사이버대학(대학)', '방송통신대학'))) |>
  filter(기준연도 == '2020') |>
  group_by(지역) |>
  summarise(rate = sum(총계)/sum(재학생)) |>
  #  arrange(desc(rate)) |>
  write.csv('clipboard')



df_col <- read_excel('./2021년 _전문대학_12-다-1. 장학금 수혜 현황_학교별자료.xlsx', col_names= TRUE, 
                     col_type = c('numeric', rep('text', 5), rep('numeric', 15)))

df_col$지역 <- fct_relevel(df_col$지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')

df_col |>
  filter(!(학교종류 %in% c('사이버대학(전문대학)', '방송통신대학'))) |>
  #filter(상태 == '기존') |>
  group_by(기준연도) |>
  summarise(rate = sum(총계)/sum(재학생)) |>
  write.csv('clipboard')

df_col |>
  filter(!(학교종류 %in% c('사이버대학(전문대학)', '방송통신대학'))) |>
  filter(기준연도 == '2020') |>
  group_by(지역) |>
  summarise(rate = sum(총계)/sum(재학생)) |>
  #  arrange(desc(rate)) |>
  write.csv('clipboard')


df_all <- read.csv('clipboard', sep = '\t')

df_all$기준연도 <- factor(df_all$기준연도)

df_all |>
  ggplot(aes(x = 기준연도)) +
  geom_line(aes(y = 전문대학, group = 1, color = '전문대학')) +
  geom_point(aes(y = 전문대학), color = 'red') +
  geom_text(aes(y = 전문대학, label = scales::comma(전문대학, accuracy = 1)), vjust = -1) +
  geom_line(aes(y = 대학, group = 1, color = '대학')) + 
  geom_point(aes(y = 대학), color = 'blue') + 
  geom_text(aes(y = 대학, label = scales::comma(대학, accuracy = 1)), vjust = -1) +
  labs(y = '장학금(천원)') +
  scale_color_manual(name = '구분', values = c('전문대학' = 'red', '대학' = 'blue'))


df_bar <- inner_join(
  df_univ |>
    filter(!(학교종류 %in% c('사이버대학(대학)', '방송통신대학'))) |>
    filter(기준연도 == '2020') |>
    group_by(지역) |>
    summarise(대학 = sum(총계)/sum(재학생)/1000),
  df_col |>
    filter(!(학교종류 %in% c('사이버대학(전문대학)', '방송통신대학'))) |>
    filter(기준연도 == '2020') |>
    group_by(지역) |>
    summarise(전문대학 = sum(총계)/sum(재학생)/1000)
)

df_bar$지역 <- fct_relevel(df_bar$지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')


df_bar |>
  gather(key = div, value = value, 2, 3) |>
  ggplot() +
  geom_col(aes(x = 지역, y = value, fill = div), position = 'dodge') + 
  labs(y = '장학금(천원)', fill = '구분')


+ 
  scale_fill_manual(name = '구분', values = c('전문대학' = 'darkred', '대학' = 'red'))



df_univ |>
  filter(!(학교종류 %in% c('사이버대학(대학)', '방송통신대학'))) |>
  filter(기준연도 == '2020') |>
  group_by(지역) |>
  summarise(대학 = sum(총계)/sum(재학생)
  )

df_univ |>
  filter(!(학교종류 %in% c('사이버대학(대학)', '방송통신대학'))) |>
  #filter(상태 == '기존') |>
  group_by(기준연도) |>
  summarise(rate = sum(`재난 장학금`)/sum(재학생))


df_col |>
  filter(!(학교종류 %in% c('사이버대학(대학)', '방송통신대학'))) |>
  #filter(상태 == '기존') |>
  group_by(기준연도) |>
  summarise(rate = sum(`재난 장학금`)/sum(재학생))

df_col |>
  filter(!(학교종류 %in% c('사이버대학(전문대학)', '방송통신대학'))) |>
  #filter(상태 == '기존') |>
  group_by(기준연도) |>
  summarise(rate = sum(`재난 장학금`)/sum(재학생))
