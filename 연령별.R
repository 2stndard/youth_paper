df_age <- read_excel('./연령별 학생수비율1.xlsx', col_names= TRUE, sheet = 'Sheet2', 
                      col_type = c('numeric', 'text', rep('numeric', 8)))
