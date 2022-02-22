library(tidyverse)
library(sparklyr)
library(dplyr)

# 4. Set up a connection to Spark by issuing the following commands:
# spark_install()
sc <- spark_connect(master = "local")

# 5
df1 <- iris %>% as_tibble()

# 6
df <- copy_to(sc, df1)

# 7
df1 %>% class()
df %>%  class()
# df1 is a tibble (tbl_df)
# df is apark type (tbl_spark)
# 8
df1 %>% colnames() # has a dot as a separation link (ex. Sepal.Length)

df %>%  colnames() # has a dash as a separation link (ex. Sepal_Length)

# the difference can be due to the class of each table

# 9-a
df %>% select(Sepal_Length,Species) %>% head() %>% print()

# 10-a
df %>% filter(Sepal_Length > 5.5) %>% head() %>% print()

# 11
df %>% select(Sepal_Length,Species) %>% filter(Sepal_Length > 5.5) %>% head() %>% print()

# 12
df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length), count = n()) %>% head() %>% print()

#13
df2 %>% arrange(Species) %>% head() %>% print()

