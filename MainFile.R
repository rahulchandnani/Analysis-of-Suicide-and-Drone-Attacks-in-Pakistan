

# Data Mining Project
# Topic :Exploratory Analysis of Suicide attacks and drone attacks in Pakistan




#firstly preparing my workspace

rm(list=ls())

#loading first data set (Since I have multiple data sets to merge)

myData=read.csv("globalterrorismdb.csv")
summary(myData) 
colnames(myData)
ncol(myData) # to check total number of columns in my data set
nrow(myData) # to check total number of rows of my data set


##############################Data Preparation (Data Merging)##############################

#now from the above data set, I'm extracting data related to my home country "Pakistan"

myDataPak=myData[which(myData$country_txt=="Pakistan"),] # all the data points related to my country
nrow(myDataPak)
colnames(myDataPak)
head(myDataPak)
write.csv(myDataPak,file="globalterrorismdbPak.csv")

#Now from the above data set extracting all suicide attacks because I want to integrate "suicide attacks" from this data set with
#other data sets of suicide attacks

mydataPak_Suicide=myDataPak[which(myDataPak$suicide==1),]
nrow(mydataPak_Suicide)
head(mydataPak_Suicide)


#write.csv(mydataPak_Suicide,file="globalterrorismdbPak_Suicide.csv")

#extracting columns of use i.e discarding un important attributes


#now cheking for other files for merging records i.e data set of drone attacks in Pakistan and data set of Suicide attacks in Pakistan

drone=read.csv("DroneAttacks_Pakistan.csv")
suicide=read.csv("SuicideAttacks_Pakistan.csv")
colnames(drone)
ncol(drone)
colnames(suicide)
ncol(suicide)

#to check which are the common and uncommon attributes in both the data sets

intersect(names(drone),names(suicide)) #checking the common attributes 
setdiff(names(drone),names(suicide)) # checking the uncommon attributes



# adding aditional necessary attriutes in data sets, 
#so that data sets can be merged
#suicide$Taliban=0
#suicide$Al.Qaeda=0
suicide$Is_Suicide=1
suicide$Is_Drone=0
drone$Is_Drone=1
drone$Is_Suicide=0
drone$Blast.Day.Type=NA
drone$Holiday.Type=NA
drone$Location.Category=NA
drone$Location.Sensitivity=NA
drone$Influencing.Event.Event=NA
drone$Target.Type=NA
drone$Targeted.Sect.if.any=NA



# now dropping un necessary columns from both the data sets because of the merging and keeping the most relevant columns
# for exploratoty analyis

suicide$Islamic.Date=NULL
suicide$Hospital.Names=NULL
suicide$Explosive.Weight..max.=NULL
suicide$Open.Closed.Space=NULL
drone$Al.Qaeda=NULL
drone$Taliban=NULL
drone$Civilians.Min=NULL
drone$Civilians.Max=NULL
drone$Foreigners.Min=NULL
drone$Foreigners.Max=NULL
drone$Blast.Day.Type=NA
drone$No..of.Suicide.Blasts=NA
drone$References=NULL
drone$Comments=NULL
drone$Women.Children=NULL
drone$Special.Mention..Site.=NULL



#changing column names of similar data for data integration from the above mentioned data sets

colnames(drone)[8]='Total.Died.Max'
colnames(suicide)[18]='Injured.Min'
colnames(suicide)[17]='Total.Died.Max'
colnames(suicide)[16]='Total.Died.Min'
colnames(suicide)
colnames(drone)

#finally merging rows of data sets using Rbind()

mergedData=rbind(suicide,drone)
intersect(names(drone),names(suicide))
setdiff(names(drone),names(suicide))
head(suicide)
ncol(drone)
ncol(suicide)
mergedData
colnames(mergedData)
mergedData[,'S.']
mergedData$S.=NULL
head(mergedData)


mergedData=mergedData[-c(476),] #removing this row since this row was the last row of the first datatset containing aggregate info for some columns
mergedData[476,]
write.csv(mergedData,file="mergedData_Suicide_Drone.csv") #so this is the final merged data I will work on it

############################################Now Pre Processing this merged dataset########################################

mergedData= apply(mergedData,2,function(x)gsub('\\s+', '',x))
mergedData=as.data.frame(mergedData)
levels(mergedData$City)= tolower(levels(mergedData$City))
levels(mergedData$Target.Type)= tolower(levels(mergedData$Target.Type))
levels(mergedData$City)
levels(mergedData$Target.Type)
mergedData$Blast.Day.Type=as.factor(mergedData$Blast.Day.Type)
mergedData$City=as.factor(mergedData$City)
unique(mergedData$Target.Type)
mergedData$Total.Died.Max=as.numeric(mergedData$Total.Died.Max)
mergedData$Latitude=as.numeric(mergedData$Latitude)
mergedData$Longitude=as.numeric(mergedData$Longitude)


# Working with dates
library(stringr)

# Since date is in un organised format like "Wednesday-May 8-2002"
# So I'm extracting the last 4 characters from the date attribute

mergedData$Date= str_sub(mergedData$Date, start= -4)
head(mergedData$Date)


#firstly for plotting the loactions of attacks(suicide and drone both) on the world map

# checking if the columns "Longitude" and "Latitude" contains missing values or not if yes 
# then removing those specific rows

mergedData= mergedData[!is.na(mergedData$Latitude),]
mergedData= mergedData[!is.na(mergedData$Longitude),]

library(ggplot2)
library(ggmap)
win.graph(800,600,10)
plt = NULL
world_map = borders("world", colour="grey") 
plt = ggplot() + world_map
Longitude = mergedData$Longitude
Latitude = mergedData$Latitude
plt = plt + geom_point(aes(x=Longitude, y=Latitude) ,color="green", size=3) + 
scale_x_continuous(limits = c(-50,75)) +
scale_y_continuous(limits = c(-50,75)) +
xlab("Longitude") + ylab("Latitude") + ggtitle("Attack Locations")
plt


#Seperate analysis of the suicide bombs 
#plotting only the suicide bombs

#just for the scak of simplicity extracting the data seperately in a temp variable by using "Is_Suicide" attribute
temp_suicide=mergedData[which(mergedData$Is_Suicide==1),]
head(temp_suicide)


#Using the maps library

library(maps)
win.graph(800,600,10)
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-80, 90), mar=c(0,0,0,0))
points(temp_suicide$Longitude,temp_suicide$Latitude, col="green", pch=16)

# finding total number of people lost their lives in suicide attacks in Pakistan
#handling missing values if any in the 'Total.Died.Max'


temp_suicide$Total.Died.Max[is.na(temp_suicide$Total.Died.Max)] = 0 #setting the missing values to zero
temp_suicide[,'Total.Died.Max']
total_people_died_Suicide=sum(temp_suicide$Total.Died.Max) #total people died in all the suicide attacks in Pakistan
total_people_died_Suicide
#13088 people have been died in suicide attacks in Pakistan

#finding Top 5 Cities with most number of killings by suicide attacks using dplyr library

library(dplyr)
by_city = group_by(temp_suicide,City)
df_summarized = summarize(by_city, v1_sum = sum(Total.Died.Max))  
df_summarized

#now extracting top five cities
List_of_top_five_cities=df_summarized[order(df_summarized$v1_sum,decreasing=T)[1:5],]


Total_killings=sum(List_of_top_five_cities$v1_sum) # Total Killings in top 5 most attacked cities
Total_killings #5467

win.graph(800,600,10)
slices= List_of_top_five_cities$v1_sum 
lbls = List_of_top_five_cities$City
lbls = paste(lbls, slices)  
lbls = paste(lbls,sep="") # adding count of killings to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),main="Top 5 Cities with most number of killings")
slices

# year wise killings in suicide bomb attacks
deaths_year_wise = aggregate(Total.Died.Max ~ Date, data = temp_suicide, FUN = sum)
head(deaths_year_wise)
win.graph(800,600,10)
barplot(height = deaths_year_wise$Total.Died.Max, horiz = FALSE, names.arg = deaths_year_wise$Date, las = 2.5, space = 1.5, main = "Year Wise Deaths in Suicide Bomb Blasts in Pakistan", xlab = "Year", ylab = "Number of Deaths", border = "green")
class(deaths_year_wise$Total.Died.Max)
#Observation: From 2007 to 2010 number of attacks increased
# now working on target types i.e who were on the target of terrorists
# finding frequency of attacks on different targets
target_frequency= table(temp_suicide$Target.Type)
target_frequency=data.frame(target_frequency)
class(target_frequency$Var1)
target_frequency
win.graph(800,600,10)
barplot(height = target_frequency$Freq,col = rainbow(8),ylim = c(0,125),cex.names = 0.55, horiz = FALSE, names.arg =target_frequency$Var1 ,
        las = 2, space = 1, main = "Number of Attacks Per Target", xlab = "", 
        ylab = "Number of Attacks(Suicide Bombs)", border = "purple")

unique(temp_suicide$Target.Type)

win.graph(800,600,10)
qplot(x = City, data = temp_suicide, xlab="Name of Cities", color = I('green'), fill = I('#F9B3B3')) +
  ggtitle("Cities and thier count of attacks") +
  scale_y_continuous(breaks = seq(0,150, 25), limits = c(0,90)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))




################################################ now working with drone attacks##############################


#analysis of the done attacks 
#plotting only the drone attacks

#just for the sake of simplicity extracting the data seperately in a temp variable by using "Is_Drone" attribute
# temp_drone=mergedData[which(mergedData$Is_Drone==1),]
# temp_drone$Total.Died.Max[is.na(temp_drone$Total.Died.Max)] = 0 #setting the missing values to zero


# but why not directly load the file which I created above after pre processing regaridng drone attacks after the data set merge
temp_drone=read.csv("DroneAttacks_Pakistan.csv")
temp_drone$Date= str_sub(temp_drone$Date, start= -4)
temp_drone$Total.Died.Mix[is.na(temp_drone$Total.Died.Mix)] = 0 #setting the missing values to zero

# total killings in Drone attacks
temp_drone$Total.Died.Mix
temp_drone=temp_drone[-c(398),]
temp_drone=temp_drone[-c(397),]
total_killings_Drone=sum(temp_drone$Total.Died.Mix)
temp_drone$Total.Died.Mix
total_killings_Drone
# till now 3556 people have been killed in drone attacks in Pakistan

# now we are finding number of drone attacks on millitant groups as reported by US
target_frequency_drone= table(temp_drone$Date)
target_frequency_drone=as.data.frame(target_frequency_drone)

#Year Wise plot of Drone attacks
win.graph(800,600,10)
barplot(height = target_frequency_drone$Freq,col = rainbow(15), horiz = FALSE, names.arg = target_frequency_drone$Var1, las = 2.5, space = 1.5, main = "Number of Drone attacks/Year", xlab = "Year", ylab = "Number of Drone Attacks", border = "green")
legend("topright", legend="from 2008 to 2016 (Obama Administration)", pch=21, pt.cex=1, cex=1)

drone_strikes = subset(temp_drone, (!(is.na(temp_drone$Al.Qaeda)) | !(is.na(temp_drone$Taliban))))
round(100*(nrow(drone_strikes)/nrow(temp_drone)), 2)

#37% drone strikes succueeded in all drone attacks carried out by US government

#most affected cities
worst_cities=table(temp_drone$City)
worst_cities=as.data.frame(worst_cities)




#now plotting the cities with their corresponding number of drone attacks
temp_drone$City= tolower(temp_drone$City)
temp_drone$City = gsub('\\s+', '_', temp_drone$City)
temp_drone$City=factor(temp_drone$City)

frequency_of_Dattack=table(temp_drone$City) #per city frequency of drone attack
frequency_of_Dattack=as.data.frame(frequency_of_Dattack)
# 
# win.graph(800,600,10)
# qplot(x = frequency_of_Dattack$Freq, data = frequency_of_Dattack, xlab="Name of Cities", color = I('green'), fill = I('#07582f')) +
#   ggtitle("Cities and thier count of Drone attacks") +
#   scale_y_continuous(breaks = seq(0,150, 25), limits = c(0,90)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5))


win.graph(800,600,10)
x=barplot(height = frequency_of_Dattack$Freq,col= rainbow(20),ylim = c(0,300), horiz = FALSE, 
         las =2.0, space = 1.0,cex.axis = 0.6,
         names.arg = frequency_of_Dattack$Var1,cex.names = 0.55, main = "Number of Drone attacks Per Region", 
         ylab = "Number of Drone Attacks", border = "green",xpd = TRUE,srt=30)


frequency_of_Dattack$Var1
#Stacked Bar Plot of Suicide attacks and Drone Attacks
win.graph(800,600,10)
counts <- c(total_killings_Drone,total_people_died_Suicide) # count previuosly found
barplot(counts, main="Comparative Study of Number of People killed in Both Attacks",
        xlab="Attacks",ylab = "Number of People Killed", col=c("purple","lightgreen"),
        legend = c("Drone Attacks","Suicide Attacks"),beside = FALSE, ylim = c(0,14000))


#################################################END#################################################
