#Tarea 1 - Sebastian Sanchez

#A) Leer el archivo de IceCube data
data <- read.table(file="NuAstro_4yr_IceCube_Data.txt",header=1)

#B) Remover entradas con datos incompletos
discriminator <- complete.cases(data)
data_buena    <- data[discriminator,]

#C) Mean Angular resolution
	#Metodo1: For loop
	print("Mean Angular Resolution with method 1: for loop")
	s <- data_buena[,8:9]
	
	shower_data <- NA
	track_data  <- NA

	for(i in 1:length(s$Topology))
	{
		if(s$Topology[i]=="Shower") shower_data[[i]] <- s$Med_Ang_Res_deg[i]
		if(s$Topology[i]=="Track") track_data[[i]]   <- s$Med_Ang_Res_deg[i]
	}


	discriminator1 <- complete.cases(shower_data)
	print("Shower-like topologies: ")
	print( mean(shower_data[discriminator1]))

	discriminator2 <- complete.cases(track_data)
	print("Track-like topologies: ")
	print( mean(track_data[discriminator2]))

	#Metodo2: with subset
	print("Mean Angular Resolution with method 2: with subset")
	
	print("Shower-like topologies: ")
	shower_data <- subset(data_buena, Topology=='Shower', select=Med_Ang_Res_deg)
	print(mean(shower_data[,]))
	
	print("Track-like topologies: ")
	track_data <- subset(data_buena, Topology=='Track', select=Med_Ang_Res_deg)
	print(mean(track_data[,]))

#D) MJD to POSIX function
mjd2posix <- function(mjd)
{
	
	#Convert to julian day first
	jd <- mjd + 2400000.5

	#Now to gregorian calendar	
	l1 <- jd + 68569
    n  <- (4*l1) %/% 146097
    l2 <- l1 - (146097*n+3) %/% 4
	i  <- (4000*(l2+1)) %/% 1461001
	l3 <- l2 - (1461*i) %/% 4 + 31
	j  <- (80*l3) %/% 2447
	
	D  <- round(l3-(2447*j) %/% 80, digits=0 )
	l4 <- j %/% 11
	M  <- j + 2 - (12*l4)
	Y  <- 100*(n-49) + i + l4
		   
	posix <- paste(c(Y,M,D),collapse = "-")
    #posix <- as.POSIXlt(posix)	
	return(posix)
}


#E) Add a posix date column to the data
data_buena$POSIX_time = mapply(mjd2posix,data_buena$Time_MJD)

#F) Function to return number-of-events in a month
evt_month <- function(data,month)
{
	a    <- table(as.POSIXlt(data$POSIX_time)$mon)
	freq <- as.vector(a[names(a)==month])
	return(freq)
}


#Now find the month with greater event count
dummy_date <- as.POSIXlt("2000-01-01")
mon <- which.max(table(as.POSIXlt(data_buena$POSIX)$mon)) - 1 
dummy_date$mon <- mon
print("El mes con mayor cantidad de eventos es: ")
print(months(dummy_date))

#G)Plot the events distribution by coordinates
plot(data_buena$RA_deg , data_buena$Declination_deg )

#H)An interesting quantity... Let's plot the 
