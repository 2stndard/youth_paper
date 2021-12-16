df.장학금 <- read.csv('clipboard', header = TRUE, sep = '\t')

df.장학금 <- data.frame(t(df.장학금))

df.장학금 <- apply(df.장학금, 2, function(x) gsub(",", "", x))


df.장학금[, 1] <- as.numeric(df.장학금[, 1])
df.장학금[, 2] <- as.numeric(df.장학금[, 2])


df.장학금 <- data.frame(df.장학금)

colnames(df.장학금) <- c('2017', '2018', '2019', '2020', '2021')
