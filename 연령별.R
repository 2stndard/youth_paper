library(readxl)
library(tidyverse)
library(showtext)
showtext_auto()


df_age <- read_excel('./연령별 학생수비율1.xlsx', col_names= TRUE, sheet = 'Sheet2', 
                      col_type = c('numeric', 'text', rep('numeric', 8)))

df_age <- df_age |>
  gather(key = div, value = value, 3:10)

df_age |>
  filter(구분 == '전문대학', div != '합계') |>
  ggplot(aes(x = as.factor(연도))) +
  geom_col(aes(y = value, fill = div), position = position_fill(reverse = TRUE)) +
  geom_text(aes(y = value, label = ifelse(value > 0.04, paste0(round(value * 100, 1),"%"), '')), 
            position=position_stack(vjust= 0.5), size=4) +
  #  geom_text(aes(label=per), position=position_stack(vjust=0.5), colour="black") +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent_format(suffix = "%", prefix = "")) +
#  scale_x_discrete(trans = 'reverse') +
  #  scale_y_discrete(labels = scales::percent_format()) +
  labs(x="연도", y="연령별 구성비", fill = '연령대') +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) + scale_fill_brewer(palette="Paired")

df_age |>
  filter(구분 == '대학', div != '합계') |>
  ggplot(aes(x = as.factor(연도))) +
  geom_col(aes(y = value, fill = div), position = position_fill(reverse = TRUE)) +
  geom_text(aes(y = value, label = ifelse(value > 0.04, paste0(round(value * 100, 1),"%"), '')), 
            position=position_stack(vjust= 0.5), size=4) +
  #  geom_text(aes(label=per), position=position_stack(vjust=0.5), colour="black") +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  scale_y_continuous(labels = scales::percent_format(suffix = "%", prefix = "")) +
  #  scale_x_discrete(trans = 'reverse') +
  #  scale_y_discrete(labels = scales::percent_format()) +
  labs(x="연도", y="연령별 구성비", fill = '연령대') +
  theme(axis.line = element_line(size=1, colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_blank(), panel.background = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) + scale_fill_brewer(palette="Paired")
