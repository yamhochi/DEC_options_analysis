#Optimised Capacity Allocation Model
setwd("/Users/hoecheeyam/desktop/simulation")
library(gdata)
library(dplyr)
library(reshape)
# install.packages('xlsx')
inputs=read.xls("inputs.xlsx")
school=read.xls("oschool.xlsx")
studentenroll=read.xls("studentenroll.xlsx")
studentseifa=read.xls("studentseifa.xlsx")
traveltime=read.xls("traveltime.xlsx")

############ all tables first row is row name ###############

############ DEFINE ROW NAMES ###############
row.names(school)<-as.vector(unlist(school[1]))#rename rows
row.names(inputs)<-as.vector(unlist(inputs[1])) #rename rows
row.names(studentenroll)<-as.vector(unlist(studentenroll[1])) 
row.names(studentseifa)<-as.vector(unlist(studentseifa[1])) #rename rows

############ SELECT SCHOOLS IN THAT CLUSTER ONLY ###############
oschool<-school[which(school["cluster"]=="A"),]
p=oschool[which(oschool["school.type"]=="p"),] #primary schools
s=oschool[which(oschool["school.type"]=="s"),] #secondary schools

############ SELECT STUDENTS IN THAT CLUSTER ONLY ###############
clusterenroll=studentenroll[which(studentenroll["cluster"]=="A"),]#

############ EXISTING SCHOOL CAPACITY ###############
oscapacity=data.frame(s[1],s[2]) #secondary schools
opcapacity=data.frame(p[1],p[2]) #primary schools


############ CONSTRUCT CLUSTER TRAVEL TIME MATRIX ############# (includes new schools)
clusterorigin=clusterenroll[1]
clusterdestination=inputs["MB"]
colnames(clusterorigin)="origin"
colnames(clusterdestination)="destination"
origin=merge(clusterorigin,traveltime,by.x="origin",by.y="origin")
clustertraveltime_m=merge(clusterdestination,origin,by.x="destination",by.y="destination")
clustertraveltime_m=cast(clustertraveltime_m,origin~destination)

############ CONSTRUCT CATCHMENT ############# 
catchment_m=clustertraveltime_m
catchment_m[which(sapply(catchment_m, is.numeric)=='TRUE')][catchment_m[which(sapply(catchment_m, is.numeric)=='TRUE')]>0]<-0

############ INPUTS WITH CHANGE ############
firstcol=which(sapply(inputs, is.numeric)=='TRUE')[1] 
seconcol=which(sapply(inputs, is.numeric)=='FALSE')[2]#find the first column that is not numeric (aka MB)
inputcapacity=inputs[firstcol:(seconcol-1)] 
inputwithchange=inputcapacity[which(colSums(inputcapacity)>0)]

inputwithchange=cbind('X'=rownames(inputwithchange),inputwithchange) #put names back to the first column
colnames(inputwithchange)[1]=colnames(opcapacity[1]) #make sure the col names are same as opcapacity

############ CHANGE YEARS ############
changeyears=colnames(inputwithchange)

#find new MB and SEIFA
#identify all schools (including new schools and their MB)
############ OTHER INPUTS TBA ############
inputMB=data.frame(inputs[1],inputs["MB"])
inputSEIFA=data.frame(inputs[1],inputs["SEIFA"])

############ OTHER PLACEHOLDERS ############

# catchment for each year (3 outputs)
catchmentlist<-NULL

# seifa for each year - then average (1 output)

# travel time for each year - then average (1 output)
# ## basically, average TT = catchment * clustertraveltime / total students allocated to school
# ### catchment * clustertraveltime 
# catchment[,sapply(catchment,is.numeric)]*clustertraveltime_m[,sapply(clustertraveltime_m,is.numeric)]
# ### colSums(catchment * clustertraveltime)
# colSums(catchment[,sapply(catchment,is.numeric)]*clustertraveltime_m[,sapply(clustertraveltime_m,is.numeric)])
# ### total students allocated to school
# colSums(catchment[,sapply(catchment,is.numeric)]))
# #### calcaulate average
averagett<-NULL

# unallocated students for each year (3 outputs)
unallocstudent<-NULL

# unallocated capacity for each year (3 outputs)
unalloccap<-NULL

# compare against max for each year (3 outputs)

############ FOR EACH YEAR THAT HAS BEEN UPDATED ############
for(i in 2:length(changeyears)){
# for(i in 2:length(changeyears)){

	#step1 - calculate school capacity of that year
	newcapacity=merge(opcapacity,data.frame(inputwithchange[1],inputwithchange[changeyears[i]]), all='T') # remember to retain row names
	newcapacity[is.na(newcapacity)]<-0 #omit NAs
	capacityofyear=data.frame(newcapacity[2]+newcapacity[3])
	newcapacityofyear=data.frame(newcapacity[1],capacityofyear)
	newcapacityofyear=merge(newcapacityofyear,inputMB,by.x=colnames(newcapacityofyear)[1],by.y="cluster.") #name column by MB
	row.names(newcapacityofyear)<-as.vector(unlist(newcapacityofyear[ncol(newcapacityofyear)]))

	#step2 - find available students
	availstudents=data.frame(clusterenroll[1],clusterenroll[changeyears[i]])
	
	#step3 - make duplicates of cluster and catchment
	catchment=catchment_m
	clustertraveltime=clustertraveltime_m
	#loop
	while (sum(newcapacityofyear[2])&&sum(availstudents[2])){#there is still capacity AND has available students
		
		#### FIND MB PAIRS WITH SHORTEST TRAVEL TIME ####
		clustertraveltime[,sapply(clustertraveltime, is.numeric)]#exclude non numeric
		coordinate = which(clustertraveltime == min(clustertraveltime[,sapply(clustertraveltime, is.numeric)]), arr.ind = TRUE) #find shortest travel time
		# print(coordinate[1,])
		originMBmintravel=as.character(clustertraveltime[coordinate[1,1],1]) #"MB2". if without "as.character, then [1] MB2"
		destMBmintravel=colnames(clustertraveltime)[coordinate[1,2]] #"MB2"	
		#### FIND NUMBER OF STUDENTS IN THIS MB PAIR ####
		numberofstudents=availstudents[which(availstudents["MB"]==originMBmintravel),2]

		#### CHECK IF THIS NUMBER OF STUDENTS CAN FIT IN THIS DESTINATION ####

		#### FIND SCHOOL IN THIS MB PAIR ####
		which(newcapacityofyear["MB"]==destMBmintravel)
		#### WHAT IS THE REMINING CAPACITY? ####
		destschoolcapacity=newcapacityofyear[which(newcapacityofyear["MB"]==destMBmintravel),2]

		#IF HAS SUFFICIENT CAPACITY
		if(numberofstudents<destschoolcapacity){
			#### ALLOCATE THESE STUDENTS TO SCHOOL ####
			catchment[coordinate[1,1],coordinate[1,2]]=numberofstudents
			#### MAKE CAPACITY IN THIS SCHOOL SMALLER ####
			newcapacityofyear[which(newcapacityofyear["MB"]==destMBmintravel),2]=destschoolcapacity-numberofstudents
			#### MAKE AVAIL STUDENTS = 0 #####
			availstudents[which(availstudents["MB"]==originMBmintravel),2]=0
			clustertraveltime[coordinate[1,1],coordinate[1,2]]=99999
			# print(clustertraveltime)
		}
		else{ #numberofstudents>destschoolcapacity
			#### ALLOCATE PARTIAL STUDENTS TO SCHOOL ####
			catchment[coordinate[1,1],coordinate[1,2]]=destschoolcapacity
			#### MAKE CAPACITY IN THIS SCHOOL 0 ####
			#in this case, 0
			newcapacityofyear[which(newcapacityofyear["MB"]==destMBmintravel),2]=0
			#### MAKE AVAIL STUDENTS SMALLER #####
			availstudents[which(availstudents["MB"]==originMBmintravel),2]=numberofstudents-destschoolcapacity
			clustertraveltime[coordinate[1,1],coordinate[1,2]]=99999
			# print(clustertraveltime)
		}
		#### MAKE SHORTEST TRAVEL TIME IRRELEVANT ####	
		#if multiple shortest, use the first one
		# print(newcapacityofyear)
	}
	#create [catchment'i']
	# assign(paste("catchment",i,sep=""),catchment)
	catchmentlist<-append(catchmentlist,list(catchment))
	unallocstudent<-append(unallocstudent,list(availstudents))
	unalloccap<-append(unalloccap,list(newcapacityofyear))
	#tt calcs for 1 year
	averagettbyyear=colSums(catchment[,sapply(catchment,is.numeric)]*clustertraveltime_m[,sapply(clustertraveltime_m,is.numeric)])/colSums(catchment[,sapply(catchment,is.numeric)])
	averagett<-append(averagett,list(averagettbyyear))

}
print(catchmentlist)
print(unallocstudent)
print(unalloccap)
print(averagett)

