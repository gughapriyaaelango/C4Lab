library(tidyverse)
library(RMySQL)

# Reading in the data, fixing up some parsing problems
data<-read_csv("./data/data.csv",col_types = cols(withheld = col_character()))

# Drop useless columns
data<-data %>% select(-X1,-`Unnamed: 0`)

# Reorder the columns
data<-data %>% select(id,everything())

# You might need to use your own username / password here!
mydb = dbConnect(MySQL(),dbname="twitter_beliefs",host="localhost",user="root")

# Next two lines just verify the connection
summary(mydb)
dbListTables(mydb)

# Following this stack overflow post here: https://stackoverflow.com/questions/34932795/mysql-and-r-inserting-more-than-1000-rows-at-a-time
dbWriteTable(mydb, name = 'temp_table', value = data, row.names = F, append = F)
dbGetQuery(mydb, "insert into tweets select * from temp_table")

# Important to remember to disconnect
# Also, drop your temp table
dbDisconnect(mydb)
