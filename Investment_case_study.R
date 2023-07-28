#################################Investment Case Study ##############################
#     Project: Investment Case Study 
#     FileNAme: Investment_Case_Studay.R
#     Description: File consist the code in R language for the analysis required by 
#     CEO of Spark Funds.
######################################################################################

#installing required packages 
#install.packages(c("tidyr", "dplyr", "stringr"))

#including library 

library(tidyr)
library(dplyr)
library(stringr)

# Setting up the working directory, Using "Setwd" 
# please change the below path, if required 
setwd("G:/Upgrad/Investment Case Group Project")

##########################################################################################

#Checkpoint 1: Data Cleaning 1
#Loading Companies Data from companies.txt file using "read.delim" 
companies <- read.delim("companies.txt", stringsAsFactors =  FALSE )

str(companies)  #Verify Structure of the companies data frame 

#changing the name of the column permalink to company_permalink of companies data frame, so that the same can be use for comparison easily 
colnames(companies)[1] <- "company_permalink"

#changing the case of company_permalink column to upper case so that the same can be used for comparison 

companies <- mutate(companies,company_permalink = toupper(company_permalink) )

# Loading Funding Data from rounds2.csv file using "read.csv"

rounds2 <- read.csv("rounds2.csv", stringsAsFactors = FALSE)

str(rounds2)   #Verify Structure of the rounds2 data frame 

#changing the case of company_permalink column to upper case so that the same can be used for comparison 

rounds2 <- mutate(rounds2,company_permalink = toupper(company_permalink) )

#Table 1.1 Solution below : 

# Identifying the unique companies present in rounds2 data frame 
nrow(distinct(rounds2,company_permalink)) 

# Identifying the unique companies present in companies data frame 
nrow(distinct(companies,company_permalink))

#Identifying the companies present in rounds2 data frame but not in companies data frame 
setdiff(rounds2$company_permalink,companies$company_permalink)

#merging the "companies" and "rounds2" data frame to "master_frame" 

master_frame <- merge(companies,rounds2,by="company_permalink",all=TRUE)
str(master_frame)

#write.table(master_frame,file ="main_tableau_feed1.csv",sep=",", row.names = FALSE)
 
##########################################################################################

#Checkpoint 2: Funding Type Analysis
## TABLE 2.1 

#creating groups for type of funding round to identify the answers of the question 
funding_round_type_group <- group_by(master_frame,funding_round_type)

#summarising the average funding based on group for rounds of funds

#summarise(funding_round_type_group,avg_funding = mean(raised_amount_usd,na.rm = T) )

summarise(master_frame[master_frame$funding_round_type == "private_equity",],avg_funding = mean(raised_amount_usd,na.rm = T))
summarise(master_frame[master_frame$funding_round_type == "angel",],avg_funding = mean(raised_amount_usd,na.rm = T))
summarise(master_frame[master_frame$funding_round_type == "seed",],avg_funding = mean(raised_amount_usd,na.rm = T))
summarise(master_frame[master_frame$funding_round_type == "venture",],avg_funding = mean(raised_amount_usd,na.rm = T))


#Identify the type of funding suitable for Spark Funds, using filter function 

filter(summarise(funding_round_type_group,avg_funding = mean(raised_amount_usd,na.rm = T)),(avg_funding>=5000000 & avg_funding <= 15000000) )

#mutate(summarise(funding_round_type_group,avg_funding = mean(raised_amount_usd,na.rm = T)),result  = ifelse(avg_funding>=5000000 & avg_funding <= 15000000,"pass","fail") )

############################################################
#Checkpoint 3: Country Analysis
#Table 3.1: Analysing the Top 3 English-Speaking Countries

#Spark Funds wants to see the top nine countries which have received the highest total funding (across ALL sectors for the chosen investment type)
#For the chosen investment type, make a data frame named top9 with the top nine countries (based on the total investment amount each country has received)

# Most preferable investment type will be "Venture"

#Grouping the master_frame by country 
country_group <- group_by(master_frame,country_code)

#filtered the group by funding_round_type == "venture" and identifying the sum then arranging the output in desc order and limiting the output to 9 rows 
top9 <-  arrange(summarise(country_group[country_group$funding_round_type == "venture" & !country_group$country_code =="",],total_funding = sum(raised_amount_usd,na.rm = T)),desc(total_funding))[1:9,]


#####################################################################
#Checkpoint 4: Sector Analysis 1

# Loading mapping Data from mapping.csv file using "read.csv"
mapping <- read.csv("mapping.csv",stringsAsFactors = FALSE)

str(mapping) #checking the structure for further analysis 
names(mapping) #Verifying the names of the column 

mapping <- gather(mapping,main_sector,val,2:10) #changing the wide formate to long format 

mapping <- mapping[!mapping$val == 0,] #taking only relevant data from the long format frame

mapping <- mapping[,-3] #val column has same value for all the rows hence removing it to keep relevant data in frame

# Cleansing the data in mapping file , found 0 in category_list for 54 places  

# below is the code to identify the count 
sum(str_count(mapping$category_list,"0"))

#after doing small manual analysis found that all the 0 can be replaced with "na" which makes correct recognizable category list 
mapping$category_list <- str_replace_all(mapping$category_list,"0","na")
mapping$category_list <- toupper(mapping$category_list)

#After applying above replace the value for "ENTERPRISE 2.NA" is changed from ENTERPRISE 2.0 which is valid value 
#replacing "ENTERPRISE 2.NA" to 'ENTERPRISE 2.0"
mapping[mapping$category_list == "ENTERPRISE 2.NA","category_list"] <- "ENTERPRISE 2.0"

# to verify if the string has new replaced key word below is the command 

mapping[str_count(mapping$category_list,"[0-9]") == 1,]

#adding the primary_sector in the master_frame with the first character string in column category_list before "|" symbol

master_frame <- separate(master_frame,category_list, "primary_sector","\\|",remove = FALSE,extra = "drop")
#master_frame$temp_sector <- master_frame$primary_sector
master_frame$primary_sector <- toupper(master_frame$primary_sector)
names(mapping)
#Changing the name of column in mapping data_frame so that can be easily merged with master_frame
colnames(mapping)[1] <- "primary_sector"

#merging mapping and master_frame to generate main sector data in master_frame for sector analysis 
master_frame <- merge(master_frame,mapping,by ="primary_sector",all.x = TRUE )

main_sector_group  <- group_by (master_frame,main_sector)

# counting the number of investment per main sector 
count(main_sector_group)

#taking backup of the master frame , standard procedure before doing major change in main frame
master_frame_backup_cp4 <- master_frame 
#master_frame <- master_frame_backup_cp4

# As per point "" understanding it as "Blank" and "NA value can be removed from the master_frame    

master_frame <- master_frame[!is.na(master_frame$main_sector),]
master_frame <- master_frame[-which(master_frame$main_sector=="Blanks"),]

#Checkpoint 5: Sector Analysis 2

#Country 1 USA

D1 <- filter(master_frame, country_code=="USA",raised_amount_usd >= 5000000 ,  raised_amount_usd <= 15000000,funding_round_type =="venture")
#Taking data in D1 for country USA where raised amount in between 5 to 15 million and fund type = venture 

D1_invest_count <- count(D1,main_sector)
#taking count sector wise in D1_invest_count  
colnames(D1_invest_count)[2] <- "Total_num_investments"
# changing the column name to meaning full name and making it easy for merge 
D1 <- merge(D1,D1_invest_count,by="main_sector")
# merging it with D1 based on main sector 
Sector_group_D1 <- group_by(D1,main_sector) # grouping to find the sum
D1_invest_amount <- summarise(Sector_group_D1,Total_investment_amount = sum(raised_amount_usd) )
# taking results in D1_invest_amount, sum by main sector 
D1 <- merge(D1,D1_invest_amount,by="main_sector")
# merging it with D1 based on main_sector 

#verifying the distinct values updated above 
distinct(D1,main_sector,Total_num_investments,Total_investment_amount)



#Country 2 GBR

D2 <- filter(master_frame, country_code=="GBR",raised_amount_usd >= 5000000 ,  raised_amount_usd <= 15000000,funding_round_type =="venture")
#Taking data in D2 for country GBR , (United Kingdom) where raised amount in between 5 to 15 million and fund type = venture 

D2_invest_count <- count(D2,main_sector)
#taking count sector wise in D2_invest_count  
colnames(D2_invest_count)[2] <- "Total_num_investments"
# changing the column name to meaning full name 
D2 <- merge(D2,D2_invest_count,by="main_sector")
# merging it with D2 based on main sector 
Sector_group_D2 <- group_by(D2,main_sector) # grouping to find the sum
D2_invest_amount <- summarise(Sector_group_D2,Total_investment_amount = sum(raised_amount_usd) )
# taking results in D2_invest_amount, sum by main sector 
D2 <- merge(D2,D2_invest_amount,by="main_sector")
# merging it with D1 based on main_sector 

#verifying the distinct values updated above 
distinct(D2,main_sector,Total_num_investments,Total_investment_amount)



#Country 3 IND 

D3 <- filter(master_frame, country_code=="IND",raised_amount_usd >= 5000000 ,  raised_amount_usd <= 15000000,funding_round_type =="venture")
#Taking data in D3 for country IND  where raised amount in between 5 to 15 million and fund type = venture 

D3_invest_count <- count(D3,main_sector)
#taking count sector wise in D3_invest_count  
colnames(D3_invest_count)[2] <- "Total_num_investments"
# changing the column name to meaning full name 
D3 <- merge(D3,D3_invest_count,by="main_sector")
# merging it with D3 based on main sector 
Sector_group_D3 <- group_by(D3,main_sector) # grouping to find the sum
D3_invest_amount <- summarise(Sector_group_D3,Total_investment_amount = sum(raised_amount_usd) )
# taking results in D3_invest_amount, sum by main sector 
D3 <- merge(D3,D3_invest_amount,by="main_sector")
# merging it with D3 based on main_sector 

#verifying the distinct values updated above 
distinct(D3,main_sector,Total_num_investments,Total_investment_amount)



# Table 5.1
# 1. Total number of investment

nrow(D1) # total number of investment  in USA 
nrow(D2) # total number of investment  in GBR 
nrow(D3) # total number of investment  in IND 

#2.Total amount of investment (USD)

sum(D1$raised_amount_usd)  # total  investment  in USA 
sum(D2$raised_amount_usd) # total  investment  in GBR 
sum(D3$raised_amount_usd) # total  investment  in IND 


#3.Top Sector name (no. of investment-wise)

#USA
arrange(distinct(D1,main_sector,Total_num_investments),desc(Total_num_investments))[1,"main_sector"]
#sorting and  taking the top sector name , count investment wise

#GBR
arrange(distinct(D2,main_sector,Total_num_investments),desc(Total_num_investments))[1,"main_sector"]
#sorting and  taking the top sector name , count investment wise

#IND
arrange(distinct(D3,main_sector,Total_num_investments),desc(Total_num_investments))[1,"main_sector"]
#sorting and  taking the top sector name , count investment wise



#4.Second Sector name (no. of investment-wise)
#USA
arrange(distinct(D1,main_sector,Total_num_investments),desc(Total_num_investments))[2,"main_sector"]
#sorting the results and taking the Main second sector  , count investment wise

#GBR
arrange(distinct(D2,main_sector,Total_num_investments),desc(Total_num_investments))[2,"main_sector"]
#sorting the results and taking the Main second sector  , count investment wise

#IND
arrange(distinct(D3,main_sector,Total_num_investments),desc(Total_num_investments))[2,"main_sector"]
#sorting the results and taking the Main second sector  , count investment wise

#5.Third Sector name (no. of investment-wise)
#USA
arrange(distinct(D1,main_sector,Total_num_investments),desc(Total_num_investments))[3,"main_sector"]
#sorting the results and taking the Main third sector  , count investment wise

#GBR
arrange(distinct(D2,main_sector,Total_num_investments),desc(Total_num_investments))[3,"main_sector"]
#sorting the results and taking the Main third sector  , count investment wise

#IND
arrange(distinct(D3,main_sector,Total_num_investments),desc(Total_num_investments))[3,"main_sector"]
#sorting the results and taking the Main third sector  , count investment wise

#6.Number of investments in top sector (3)
#USA
arrange(distinct(D1,main_sector,Total_num_investments),desc(Total_num_investments))[1,"Total_num_investments"]
#sorting the results and taking the count top sector  , count investment wise
#GBR
arrange(distinct(D2,main_sector,Total_num_investments),desc(Total_num_investments))[1,"Total_num_investments"]
#sorting the results and taking the count top sector  , count investment wise
#IND
arrange(distinct(D3,main_sector,Total_num_investments),desc(Total_num_investments))[1,"Total_num_investments"]
#sorting the results and taking the count top sector  , count investment wise


#7.Number of investments in second sector (4)

#USA
arrange(distinct(D1,main_sector,Total_num_investments),desc(Total_num_investments))[2,"Total_num_investments"]
#sorting the results and taking the count second sector  , count investment wise
#GBR
arrange(distinct(D2,main_sector,Total_num_investments),desc(Total_num_investments))[2,"Total_num_investments"]
#sorting the results and taking the count second sector  , count investment wise
#IND
arrange(distinct(D3,main_sector,Total_num_investments),desc(Total_num_investments))[2,"Total_num_investments"]
#sorting the results and taking the count second sector  , count investment wise


#8.Number of investments in third sector (5)
#USA
arrange(distinct(D1,main_sector,Total_num_investments),desc(Total_num_investments))[3,"Total_num_investments"]
#sorting the results and taking the count third sector  , count investment wise
#GBR
arrange(distinct(D2,main_sector,Total_num_investments),desc(Total_num_investments))[3,"Total_num_investments"]
#sorting the results and taking the count third sector  , count investment wise
#IND
arrange(distinct(D3,main_sector,Total_num_investments),desc(Total_num_investments))[3,"Total_num_investments"]
#sorting the results and taking the count third sector  , count investment wise


#9.For point 3 (top sector count-wise), which company received the highest investment?

#USA
top_sector_D1 <- D1[D1$main_sector == "Others",]
#using the results from the above code hard coding the value for the top sector in USA
comp_name_group_D1 <- group_by(top_sector_D1,name)
#grouping by the company name 
arrange(summarise(comp_name_group_D1,Investment_recieved = sum(raised_amount_usd,rm.na = T)),desc(Investment_recieved))[1,"name"]
#sorting using arrange function and taking the top value from the list 

#GBR
top_sector_D2 <- D2[D2$main_sector == "Others",]
#using the results from the above code hard coding the value for the top sector in GBR
comp_name_group_D2 <- group_by(top_sector_D2,name)
#grouping by the company name 
arrange(summarise(comp_name_group_D2,Investment_recieved = sum(raised_amount_usd,rm.na = T)),desc(Investment_recieved))[1,"name"]
#sorting using arrange function and taking the top value from the list 

#IND
top_sector_D3 <- D3[D3$main_sector == "Others",]
#using the results from the above code hard coding the value for the top sector in IND
comp_name_group_D3 <- group_by(top_sector_D3,name)
#grouping by the company name 
arrange(summarise(comp_name_group_D3,Investment_recieved = sum(raised_amount_usd,rm.na = T)),desc(Investment_recieved))[1,"name"]
#sorting using arrange function and taking the top value from the list 



#10.For point 4 (second best sector count-wise), which company received the highest investment?

#USA
second_sector_D1 <- D1[D1$main_sector == "Social..Finance..Analytics..Advertising",]
#using the results from the above code hard coding the value for the second best sector in USA
comp_name_group_D1_2 <- group_by(second_sector_D1,name)
#grouping by the company name 
arrange(summarise(comp_name_group_D1_2,Investment_recieved = sum(raised_amount_usd,rm.na = T)),desc(Investment_recieved))[1,"name"]
#sorting using arrange function and taking the top value from the list 

#GBR
second_sector_D2 <- D2[D2$main_sector == "Social..Finance..Analytics..Advertising",]
#using the results from the above code hard coding the value for the second best sector in GBR
comp_name_group_D2_2 <- group_by(second_sector_D2,name)
#grouping by the company name 
arrange(summarise(comp_name_group_D2_2,Investment_recieved = sum(raised_amount_usd,rm.na = T)),desc(Investment_recieved))[1,"name"]
#sorting using arrange function and taking the top value from the list 

#IND
second_sector_D3 <- D3[D3$main_sector == "Social..Finance..Analytics..Advertising",]
#using the results from the above code hard coding the value for the second best sector in IND
comp_name_group_D3_2 <- group_by(second_sector_D3,name)
#grouping by the company name 
arrange(summarise(comp_name_group_D3_2,Investment_recieved = sum(raised_amount_usd,rm.na = T)),desc(Investment_recieved))[1,"name"]
#sorting using arrange function and taking the top value from the list 
