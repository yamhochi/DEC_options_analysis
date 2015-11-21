Step1 - select geography
(selection 1 - LGA1)(selection 2 - LGA2)(selection 3 - TZ1)(selection 4 - cluster1)

Step2 - how many scenarios to test?


require(gdata)
setwd("/Users/hoecheeyam/desktop/simulation")
students = read.xls("students.xlsx")
capacity=read.xls("capacity.xlsx")
traveltime=read.xls("traveltime.xlsx")
catchment=read.xls("catchment.xlsx")

simulate <- function(students,capacity,traveltime,catchment){
	while(sum(students)>0.1&&sum(capacity)>0.1){
	for(i in 1:ncol(capacity)){
		if(capacity[[i]]>0.1){
			targetTZ=which(traveltime[i]==min(traveltime[i])) #table3[i] with shortest distance
			numberofstudents = students[targetTZ,] #this is correct

			#check if the number of students can fit in remaining capacity
			capacityaspctstudent = capacity[i]/numberofstudents
			studentaspctcapacity=numberofstudents/capacity[i]

			if(numberofstudents==0){
				capacityaspctstudent = 0
			}
			if(capacity[i]==0){
				studentaspctcapacity = 0
			}

			#assign student number to catchent	
			catchment[targetTZ,i]=catchment[targetTZ,i]+min(capacity[[i]],numberofstudents)

			capacity[i]=capacity[[i]]*(1- min(1,studentaspctcapacity[1,]))
			students[targetTZ,] = students[targetTZ,] *(1-min(1,capacityaspctstudent[1,]))	
			
			if(students[targetTZ,]==0){
				traveltime[targetTZ,i]=999
			}			
		}
		else{capacity[[i]]=capacity[[i]]}
	}
}
return(catchment)
}


simulate(students,capacity,traveltime,catchment)


