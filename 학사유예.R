df_유예 <- read_excel('./복사본 1 재적학생수 현황(유예생 구분).xlsx', col_names= FALSE, sheet = 'Sheet1', skip = 6, 
                    col_type = c('numeric', 'text', rep('numeric', 4)))

colnames(df_유예) <- c('조사회차', '구분', '재적학생수', '재학생수', '휴학생수', '유예자수')

df_유예$조사회차 <- as.factor(round(df_유예$조사회차/100, 0))

df_유예_sum <- 
  rbind(
    df_유예 |>
      filter(구분 %in% c('교육대학', '대학교', '산업대학', '기술대학', '각종대학(대학)', '원격대학(대학)', '사내대학(대학)', '사이버대학(대학)')) |>
      group_by(조사회차) |>
      summarise(across(where(is.character), ~"학부"), across(where(is.numeric), sum)), 
    df_유예 |>
      filter(구분 %in% c('전문대학', '각종대학(전문)', '원격대학(전문)', '사내대학(전문)', '사이버대학(전문)', '전공대학', '기능대학')) |>
      group_by(조사회차) |>
      summarise(across(where(is.character), ~"전문대"), across(where(is.numeric), sum))
  ) |> write.csv('clipboard')


