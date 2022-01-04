library(readxl)
library(tidyverse)
library(showtext)
showtext_auto()

df_등록금 <- read_excel('./등록금현황합본.xlsx', col_names= TRUE, sheet = 'Sheet1', skip = 3,
                     col_type = c(rep('text', 6), rep('numeric', 8)))

glimpse(df_등록금)

df_등록금$기준연도 <- fct_relevel(df_등록금$기준연도, '2019', '2020', '2021')

distinct(df_등록금, 지역)

df_등록금$지역 <- fct_relevel(df_등록금$지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')

df_등록금 <- bind_rows(
df_등록금 |>
  filter(기준연도 %in% c('2020', '2021')) |>
  mutate(입학금 = round(입학금/1000, 1), 수업료 = round(수업료/1000, 1), 
         등록금 = round(등록금/1000, 1), 인문사회 = round(인문사회/1000, 1),
         자연과학 = round(자연과학/1000, 1), 예체능 = round(예체능/1000, 1), 
         공학 = round(공학/1000, 1), 의학 = round(의학/1000, 1)), 
df_등록금 |>
  filter(기준연도 == '2019'))

df_등록금[, 7:14][df_등록금[, 7:14] == 0] <- NA

df_등록금 |>
  group_by(지역, 기준연도) |>
  summarise(round(mean(입학금, na.rm = TRUE), 1), 
            round(mean(수업료, na.rm = TRUE), 1), 
            round(mean(인문사회, na.rm = TRUE), 1), 
            round(mean(자연과학, na.rm = TRUE), 1), 
            round(mean(예체능, na.rm = TRUE), 1), 
            round(mean(공학, na.rm = TRUE), 1), 
            round(mean(의학, na.rm = TRUE), 1)) |>
  write.csv('clipboard')  


df_등록금 |>
  group_by(지역, 기준연도) |>
  summarise(round(mean(입학금), 1), 
            round(mean(수업료), 1), 
            round(mean(인문사회), 1), 
            round(mean(자연과학), 1), 
            round(mean(예체능), 1), 
            round(mean(공학), 1), 
            round(mean(의학), 1)) |>
  write.csv('clipboard')  


########################################################################

df_등록금_전문대 <- read_excel('./등록금현황합본_전문대.xlsx', col_names= TRUE, sheet = 'Sheet1',
                     col_type = c(rep('text', 6), rep('numeric', 8)))

glimpse(df_등록금)

df_등록금_전문대$기준년도 <- fct_relevel(df_등록금_전문대$기준년도, '2019', '2020', '2021')

distinct(df_등록금, 지역)

df_등록금_전문대$지역 <- fct_relevel(df_등록금_전문대$지역, '서울', '부산', '대구', '인천', '광주', '대전', '울산', '세종', '경기', '강원', '충북', '충남', '전북', '전남', '경북', '경남', '제주')

df_등록금_전문대 <- bind_rows(
  df_등록금_전문대 |>
    filter(기준년도 %in% c('2020', '2021')) |>
    mutate(입학금 = round(입학금/1000, 1), 수업료 = round(수업료/1000, 1), 
              등록금 = round(등록금/1000, 1), 인문사회 = round(인문사회/1000, 1),
              자연과학 = round(자연과학/1000, 1), 예체능 = round(예체능/1000, 1), 
              공학 = round(공학/1000, 1), 의학 = round(의학/1000, 1)), 
  df_등록금_전문대 |>
    filter(기준년도 == '2019'))

df_등록금_전문대[, 7:14][df_등록금_전문대[, 7:14] == 0] <- NA

df_등록금_전문대 |>
  group_by(지역, 기준년도) |>
  summarise(round(mean(입학금, na.rm = TRUE), 1), 
            round(mean(수업료, na.rm = TRUE), 1), 
            round(mean(인문사회, na.rm = TRUE), 1), 
            round(mean(자연과학, na.rm = TRUE), 1), 
            round(mean(예체능, na.rm = TRUE), 1), 
            round(mean(공학, na.rm = TRUE), 1), 
            round(mean(의학, na.rm = TRUE), 1)) |>
  write.csv('clipboard')  


df_등록금 |>
  group_by(지역, 기준년도) |>
  summarise(round(mean(입학금), 1), 
            round(mean(수업료), 1), 
            round(mean(인문사회), 1), 
            round(mean(자연과학), 1), 
            round(mean(예체능), 1), 
            round(mean(공학), 1), 
            round(mean(의학), 1)) |>
  write.csv('clipboard')  
