

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
# Data Visualization Case Study 
 
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$  
  



# Setting the working directoryss
setwd("E:/Bharat/Case Studies/Graph Visualisation")

# Reading the file sales data
Sales_Data <- read.csv("E:/Bharat/Case Studies/Graph Visualisation/SalesData.csv")

# Checking for NA's
sum(is.na(Sales_Data))

str(Sales_Data)

# Qns 1 Calculating Sales by region for 2016 with 2015 by using bar chart

# Loading package dplyr
require(dplyr)
Sales <- Sales_Data %>% dplyr::group_by(Region) %>% dplyr::summarise(Salesin2015 = sum(Sales2015),Salesin2016 = sum(Sales2016))

# Loading Reshape2 Package
require(reshape2)
# Changing the wide format to long format using reshape2::melt() function
Sales_melt <- reshape2::melt(Sales,variable.name="Years",value.name="Sales")

# Loading GGplot2 Package
require(ggplot2)

# Using ggplot2 for plotting bar graph and comparing sales in 2015 and 2016
Sales_Graph <- ggplot2::ggplot(data = Sales_melt)+aes(x=Region,y=Sales,fill=Years)+geom_bar(stat="identity",color="black",position = "dodge") +geom_text(aes(label=round(Sales,0)), vjust=-0.3, size=4.5)  #+facet_grid(.~variable)
Sales_Graph

# Loading Plotly
require(plotly)

# Using Plotly to plot thr graph
plotly::ggplotly(Sales_Graph,height=500,width=900)

# Qns 2 What are the contributing factors to the sales for each region in 2016. Visualize it using a Pie Chart.

# Calculating Region Wise Sales in 2016
Sales_in_2016 <- Sales_Data %>% dplyr::group_by(Region) %>% dplyr::summarise(Salesin2016 = sum(Sales2016))

# Calculating Percentage
Percent <-round((Sales_in_2016$Salesin2016/sum(Sales_in_2016$Salesin2016))*100,2)

# Adding % to Labels
lbls <- paste0(Percent,"%")

# Adding Region to Labels and separating it with :
lbls <- paste(lbls,Sales_in_2016$Region,sep = ":")

# Loading package plotrix
require(plotrix)

# using plotrix for plotting pie chart in 3D
plotrix::pie3D(Sales_in_2016$Salesin2016,labels = lbls,explode = 0.1, main ="Pie Chart of Sales in 2016",col=rainbow(length(Sales_in_2016$Salesin2016)))

# Qns 3 Compare the total sales of 2015 and 2016 with respect to Region and Tiers

# Calculating Region wise and Tier Wise Sales in 2015 and 2016
Region_Tier_Sales <- Sales_Data %>% dplyr::group_by(Region,Tier) %>% dplyr::summarise(Salesin2015 = sum(Sales2015),Salesin2016 = sum(Sales2016))

# Changing it into Long format using reshape2::melt()
#Region_Tier <- reshape2::melt(Region_Tier_Sales,variable.name)
Region_Tier <- reshape2::melt(Region_Tier_Sales,variable.name="Years",value.name="Sales")

# Plotting the Graph and Comparing it with respect to Sales in 2015 and 2016
Region_Tier_Graph <- ggplot2::ggplot(data = Region_Tier)+aes(x=Tier,y=Sales,fill=Years)+geom_bar(stat="identity",color="black",position = "dodge") +facet_grid(.~Region)
Region_Tier_Graph

# Qns In East region, which state registered a decline in 2016 as compared to 2015?
State_Region <- Sales_Data %>% dplyr::group_by(State,Region) %>% dplyr::summarise(Salesin2015 = sum(Sales2015),Salesin2016 = sum(Sales2016))

# Extracting the States which have region East
East_Region <- State_Region[State_Region$Region=="East",]

# Changing it into Long format using reshape2::melt()
East_Tier_Region <- reshape2::melt(East_Region,variable.name="Years",value.name="Sales")

# Plotting the Graph and Comparing it with respect to Sales in 2015 and 2016 in East Region
East_Tier_Region_Graph <- ggplot2::ggplot(data = East_Tier_Region)+aes(x=State,y=Sales,fill=Years)+geom_bar(stat="identity",color="black",position = "dodge")+ggtitle(" So In East Region only Newyork city registered decline in sales as compared to 2015 in 2016")
East_Tier_Region_Graph

# So in East Region only Newyork city registered decline in sales as compared to 2015 in 2016

# Qns 5 In all the High tier, which Division saw a decline in number of units sold in 2016 compared to 2015

# Calculating Sales in 2015 and 2016 with respect to Division and Tier
Divison_Tier <- Sales_Data %>% dplyr::group_by(Division,Tier) %>% dplyr::summarise(Salesin2015 = sum(Sales2015),Salesin2016 = sum(Sales2016))

# Calculating Sales where tier is High
High_Tier <- Divison_Tier[Divison_Tier$Tier=="High",]

# Changing it into Long format using reshape2::melt()
High_Division <- reshape2::melt(High_Tier,variable.name="Years",value.name="Sales")

# Plotting the Graph and Comparing it with respect to Sales in 2015 and 2016 where Tier is High
High_Tier_Graph <- ggplot2::ggplot(data = High_Division)+aes(x=Division,y=Sales,fill=Years)+geom_bar(stat="identity",color="black",position = "dodge")+theme(text = element_text(size=15),axis.text.x = element_text(angle=120, hjust=1))


# Qns 6 Create a new column Qtr using ifelse() or any suitable utility in the imported dataset. The Quarters are based on months and defined as -
# Jan - Mar : Q1
# Apr - Jun : Q2
# Jul - Sep : Q3
# Oct - Dec : Q4

# Matching the month abbreviation with month number
Sales_Data$Month_Numeric <- match(Sales_Data$Month,month.abb)


# Adding a Column Qtr by binning as per the Question
Sales_Data$Qtr <- ifelse(Sales_Data$Month_Numeric<=3,"Q1",ifelse(Sales_Data$Month_Numeric<=6,"Q2",ifelse(Sales_Data$Month_Numeric<=9,"Q3","Q4")))


# Qns 7 Compare Qtr wise sales in 2015 and 2016 in a bar plot

# Calculating Quarter wise Sales of the Year 2015 and 2016
Qtr_Wise_Sales <- Sales_Data %>% dplyr::group_by(Qtr) %>% dplyr::summarise(Salesin2015 = sum(Sales2015),Salesin2016 = sum(Sales2016))

# Changing it into Long format using reshape2::melt()
Qtr_Sales <- reshape2::melt(Qtr_Wise_Sales,variable.name="Years",value.name="Sales")

# Plotting the Graph and comparing Qtr wise sales in 2015 and 2016
Qtr_Sales_Graph <-ggplot2:: ggplot(data = Qtr_Sales)+aes(x=Qtr,y=Sales,fill=Years)+geom_bar(stat = "identity",color="black",position = "dodge")
Qtr_Sales_Graph


# Qns 8 Determine the composition of Qtr wise sales in and 2016 with regards to all the Tiers in a pie chart.

# Calculating Quarterwise and Tier Wise Sales in 2016
Qtr_2016 <- Sales_Data %>% dplyr::group_by(Qtr,Tier) %>% dplyr::summarise(Salesin2016 = abs(sum(Sales2016)))

# Extracting Rows where Quarter is Q1
Qtr_Q1 <- Qtr_2016[Qtr_2016$Qtr=="Q1",]

# Plotting the Pie Chart for Quarter Q1
pie(Qtr_Q1$Salesin2016,labels = Qtr_Q1$Tier,main = "Q1")

# Extracting Rows where Quarter is Q2
Qtr_Q2 <- Qtr_2016[Qtr_2016$Qtr=="Q2",]

# Plotting the Pie Chart for Quarter Q2
pie(Qtr_Q2$Salesin2016,labels = Qtr_Q2$Tier,main = "Q2")

# Extracting Rows where Quarter is Q3
Qtr_Q3 <- Qtr_2016[Qtr_2016$Qtr=="Q3",]

# Plotting the Pie Chart for Quarter Q3
pie(Qtr_Q3$Salesin2016,labels = Qtr_Q3$Tier,main = "Q3")

# Extracting Rows where Quarter is Q4
Qtr_Q4 <- Qtr_2016[Qtr_2016$Qtr=="Q4",]

# Plotting the Pie Chart for Quarter Q4
pie(Qtr_Q4$Salesin2016,labels = Qtr_Q3$Tier,main = "Q4")
