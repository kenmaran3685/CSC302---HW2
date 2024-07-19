# Section # 1

library(dplyr)

df1=data.frame(Name=c('James','Paul','Richards','Marico','Samantha','Ravi','Raghu',
                      'Richards','George','Ema','Samantha','Catherine'),
               State=c('Alaska','California','Texas','North Carolina','California','Texas',
                       'Alaska','Texas','North Carolina','Alaska','California','Texas'),
               Sales=c(14,24,31,12,13,7,9,31,18,16,18,14))
aggregate(df1$Sales, by=list(df1$State), FUN=sum)

df1 %>% group_by(State) %>% summarise(sum_sales = sum(Sales))
# Section # 1
# These lines of code group the corresponding states with the corresponding sales number, 
# however I do not see a relationship with the sales people per state. The end code summarizes the data into a 
# group as well as a tibble that separates them into columns categorized by state and the sum of the sales.

# Section # 2 - World Cup Matches
df <- read.csv("G:/My Drive/CSC 302/DATA_Ken/WorldCupMatches.csv")

# Part a) - Size of the Data - Rows/Columns
size_df <- dim(df)
print(size_df)

# Part b) - Statistical Summary
summary_df <- summary(df)
print(summary_df)

# Part c) - Unique Locations
unique_locations <- df %>%
  distinct(City) %>%
  nrow()
print(unique_locations)

# Part d) - Average Attendance
avg_attendance <- mean(df$`Attendance`, na.rm = TRUE)
print(avg_attendance)

# Part e) - Total number of goals scored by team.
goals_by_home_team <- df %>%
  group_by(`Home.Team.Name`) %>%
  summarise(total_goals = sum(`Home.Team.Goals`, na.rm = TRUE)) %>%
  arrange(desc(total_goals))
print(goals_by_home_team, n = 78)

# Part f) - Average number of attendees for each year.
# I do not see any trend or pattern in the data.
avg_attendance_by_year <- df %>%
  group_by(`Year`) %>%
  summarise(avg_attendance = mean(`Attendance`, na.rm = TRUE)) %>%
  arrange(`Year`)
print(avg_attendance_by_year)


# Section 3 - Metabolites
df2 = read.csv("G:/My Drive/CSC 302/DATA_Ken/metabolite.csv", header=T)

# Part a) - 
alzheimers_patients <- df2 %>%
  group_by(`Label`) %>%
  summarise(alzheimers = sum(`Label`, na.rm = TRUE)) %>%
  arrange(desc(alzheimers))
print(alzheimers_patients, n = 78)

# Part b) - Missing values in each column.
sum(is.na(df2))

# Part c) - Remove the rows which has missing value for the Dopamine
data = (is.na(df2 = TRUE))
print(data)

# Part d) - Replace the missing values in the c4-OH-Pro column with the median value of the same column.

