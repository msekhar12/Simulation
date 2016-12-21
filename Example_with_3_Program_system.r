rm(list=ls())

#To run the presentation example use seed as 1
set.seed(1)

Transfer_Speed <- 1

#To run the presentation example, use n=3
n= 3

#Generate the list of programs
Program_ID <- 1:n

#Program_depth <- rpois(lambda=1,n)

#Generate the number of tables accessed by each of the program
Total_Tables_Accessed <- rpois(lambda=3,n)

#Generate the program slowness cost due to remote table access
Slowness_Cost_per_unit_time <- rpois(lambda=3,n)

#Generate the downtime cost of the program
Down_time_cost_per_unit_time <- rpois(lambda=50,n)

#Prepare the program, the number of tables it accesses, slowness cost per unit time, downtime cost per unit time
Programs_df = data.frame(Program_ID,Total_Tables_Accessed,Slowness_Cost_per_unit_time,Down_time_cost_per_unit_time)


#Generate the required number of tables 
Total_Tables = sum(Total_Tables_Accessed)


Table_ID <- 1:Total_Tables
#Size <- rpois(lambda=1.5,Total_Tables)+0.5*abs(rnorm(Total_Tables))

#Generate the size of tables
#Size <- rpois(lambda=1.5,Total_Tables) 
Size <- abs(rnorm(Total_Tables,mean=3,sd=1)) 

Table_df <- data.frame(Table_ID, Size)

#Generate the table's local access and remote access time
Table_df$Local_Access_Time <- Table_df$Size * 0
Table_df$Remote_Access_Time <- Table_df$Size * 3 

#Now prepare the remote table access cost
Atleast_One_Tab <- Programs_df[Programs_df$Total_Tables_Accessed>0,]

Pgm_Tab_df <- data.frame()

for(i in 1:nrow(Atleast_One_Tab))
{
  
  x = rep(Atleast_One_Tab$Program_ID[i],Atleast_One_Tab$Total_Tables_Accessed[i])
  #Local_Cost = rep(0,Atleast_One_Tab$Total_Tables_Accessed[i])
  #Remote_Cost = rep(3,Atleast_One_Tab$Total_Tables_Accessed[i])
  #In_Transit_Cost = rep(10,Atleast_One_Tab$Total_Tables_Accessed[i])
  
  y = sample(1:Total_Tables,Atleast_One_Tab$Total_Tables_Accessed[i])
  
  z = Table_df$Remote_Access_Time[y]
  Remote_Access_Loss = z * Atleast_One_Tab$Slowness_Cost_per_unit_time[i]   
  Program_Downtime_Loss_Per_Unit_Time = Atleast_One_Tab$Down_time_cost_per_unit_time[i]   
  Pgm_Tab_df <- rbind(Pgm_Tab_df,data.frame(Program_ID=x,Table_ID=y,Remote_Access_Time=z,
                                            Remote_Access_Loss,Program_Downtime_Loss_Per_Unit_Time))   
  
}

#Assuming the transit speed as 100 MBps, with some random speed fluctuation

#Display the tables list and programs data frames
#Table_df
#Programs_df

#Display the programs-tables mapping
Pgm_Tab_df

#The above data frame has the following columns:
#Program_ID - Program ID
#Table_ID - Unique table accessed by the program
#Remote_Access_Time - Time to access the remote table
#Remote_Access_Loss - Loss incurred by the program while accessing the remote table once
#Program_Downtime_Loss_Per_Unit_Time - Loss incurred by the program due to downtime/sec


library(dplyr)

Table_Remote_Access_Cost <- Pgm_Tab_df %>%
  group_by(Table_ID) %>%
  summarise(Remote_Access_Loss=sum(Remote_Access_Loss))

temp <- Table_df[-which(Table_df$Table_ID %in% Table_Remote_Access_Cost$Table_ID),]

if(nrow(temp) > 0)
{
  Table_Remote_Access_Cost <- rbind(data.frame(Table_ID=temp$Table_ID,Remote_Access_Loss=0),Table_Remote_Access_Cost)
}

Table_Remote_Access_Cost <- Table_Remote_Access_Cost[order(Table_Remote_Access_Cost$Table_ID),]

#The Table_Remote_Access_Cost data frame has the following columns:
#Table_ID - Unique identifier of the table
#Remote_Access_Loss - Loss incurred to access the table remotely by all the programs for one time

#---------------------------------------------------
##Now generating the cost for maintaining the RI 
#---------------------------------------------------

Table_df$Number_of_Related_Tables = rpois(lambda=3,nrow(Table_df))

Table_Down_Time_Cost <- Pgm_Tab_df %>%
  group_by(Table_ID) %>%
  summarise(Pgms_Down_Time_Loss_Per_Unit_Time=sum(Program_Downtime_Loss_Per_Unit_Time))

temp <- Table_df[-which(Table_df$Table_ID %in% Table_Down_Time_Cost$Table_ID),]

if(nrow(temp) > 0)
{
  Table_Down_Time_Cost <- rbind(data.frame(Table_ID=temp$Table_ID,Pgms_Down_Time_Loss_Per_Unit_Time=0),Table_Down_Time_Cost)
}

Table_Down_Time_Cost <- Table_Down_Time_Cost[order(Table_Down_Time_Cost$Table_ID),]

#The Table_Down_Time_Cost data frame has the following columns. This df has the overall cost 
#associated with a table unavailability.

#Table_ID - Table identifier
#Pgms_Down_Time_Loss_Per_Unit_Time - Loss to business due to program unavailability

Tables = nrow(Table_df)
df <- data.frame()
i=2
for(i in 1:Tables)
{
  x  <-  sample(1:Tables)
  x <- x[-which(x %in% i)]
  
  if(Table_df$Number_of_Related_Tables[i] > 0)
  {
    y <- sample(x,Table_df$Number_of_Related_Tables[i])  
    dep_tab_sz <- Table_df$Size[y]
    
  }
  else{
    next
  }
  #y <- ifelse(Table_df$Number_of_Related_Tables[i] > 0,sample(x,Table_df$Number_of_Related_Tables[i]),0)
  
  z <- rep(i,length(y))
  parent_tab_sz <- rep(Table_df$Size[i],length(y))
  df <- rbind(df,data.frame(Parent_Table=z,Child_Table=y,Parent_Table_Size=parent_tab_sz,Child_Table_Size=dep_tab_sz))
  
}

df$Parent_Table_Size = ifelse(df$Parent_Table_Size == 0, 1, df$Parent_Table_Size)

df$Parent_Remote_Time = df$Child_Table_Size / Transfer_Speed
df$Child_Remote_Time = df$Parent_Table_Size / Transfer_Speed

df$RI_Cost = df$Parent_Table_Size * df$Child_Table_Size * .01

Table_RI_Cost <- df

Child_Table_Remote_Time <- Table_RI_Cost %>%
  group_by(Child_Table) %>%
  summarise(Remote_Time=sum(Child_Remote_Time))

names(Child_Table_Remote_Time)[1] <- c("Table")

Parent_Table_Remote_Time <- Table_RI_Cost %>%
  group_by(Parent_Table) %>%
  summarise(Remote_Time=sum(Parent_Remote_Time))
names(Parent_Table_Remote_Time)[1] <- c("Table")

df <- rbind(Child_Table_Remote_Time,Parent_Table_Remote_Time)

df <- df %>%
  group_by(Table) %>%
  summarise(Remote_Time=sum(Remote_Time))

x = Table_df$Table_ID[-which(df$Table %in% Table_df$Table_ID)]
if(length(x) > 0){
  df = rbind(df,data.frame(Table=x,Remote_Time=0))
}

df=df[order(df$Table),]
Table_Remote_Time = df

Table_RI_Cost_Cumulative <- Table_RI_Cost %>%
  group_by(Parent_Table) %>%
  summarise(Total_RI_Cost=sum(RI_Cost),Table_Size=max(Parent_Table_Size))


temp <- Table_df[-which(Table_df$Table_ID %in% Table_RI_Cost_Cumulative$Parent_Table),]

if(nrow(temp) > 0)
{
  Table_RI_Cost_Cumulative <- rbind(data.frame(Parent_Table=temp$Table_ID,Total_RI_Cost=0,Table_Size=temp$Size),
                                    Table_RI_Cost_Cumulative)
}

Table_RI_Cost_Cumulative <- Table_RI_Cost_Cumulative[order(Table_RI_Cost_Cumulative$Parent_Table),]

#The data frame Table_RI_Cost_Cumulative has Parent table, the total RI cost for the table
#and table size. The RI cost is in dollar/sec
#Parent_Table - Parent table ID
#Total_RI_Cost - RI Cost / sec
#Table_Size - Parent table size


#Final data sets:
head(Table_RI_Cost_Cumulative)
head(Table_Remote_Access_Cost)
head(Table_Down_Time_Cost)
head(Table_Remote_Time)




Cost_df <- data.frame(Table_ID=Table_Remote_Access_Cost$Table_ID,
                      Loss_Due_To_Program=Table_Remote_Access_Cost$Remote_Access_Loss,
                      Loss_Due_To_RI = Table_RI_Cost_Cumulative$Total_RI_Cost,
                      Remote_Access_Cost_Per_Unit_Time = Table_Remote_Access_Cost$Remote_Access_Loss + 
                        Table_RI_Cost_Cumulative$Total_RI_Cost,
                      Programs_Downtime_Cost_Per_Unit_Time=Table_Down_Time_Cost$Pgms_Down_Time_Loss_Per_Unit_Time,
                      Table_Size=Table_RI_Cost_Cumulative$Table_Size
)



Cost_df$Time_To_Transfer <- Cost_df$Table_Size/Transfer_Speed
head(Cost_df)
Cost_df$Total_Downtime_Cost = Cost_df$Time_To_Transfer * Cost_df$Programs_Downtime_Cost_Per_Unit_Time

Cost_df$Min_Remote_Time <- Table_Remote_Time$Remote_Time

#View(Cost_df)

#Cost_df$Programs_Downtime_Cost_Per_Unit_Time * Cost_df$Time_To_Transfer

library(lpSolve)

Constraint = Cost_df$Min_Remote_Time
dir <- rep(">=",length(Constraint))

Optimization = lp(direction="min",objective.in=Cost_df$Remote_Access_Cost_Per_Unit_Time,const.mat=diag(rep(1,length(Constraint))),
                  const.dir=dir,const.rhs=Constraint)

df = data.frame(Table_ID = 1:length(Constraint)
                , Remote_Time=Optimization$solution)
#,Constraint)
df <- df[order(df$Remote_Time,decreasing=TRUE),]
head(df)


#If completely shutdown and transferred via WAN:
#sum(Cost_df$Time_To_Transfer) * sum(Cost_df$Programs_Downtime_Cost_Per_Unit_Time)
sum(Table_df$Size)* sum(Programs_df$Down_time_cost_per_unit_time)

#If transferred in piecemeal fashon:
sum(Cost_df$Programs_Downtime_Cost_Per_Unit_Time * Cost_df$Time_To_Transfer)+Optimization$objval

#As per simio, the total travel time (for fork lift process) is 20.2 hours. Hence loss is:
sum(Cost_df$Programs_Downtime_Cost_Per_Unit_Time) * 17.322*60*60

#Optimization$solution
