
# __________________________________________________________
# //////////////////////////////////////////////////////////
#
#   AUTHOR - ANUPAMA RAJARAM
#   MODULE 1 - STATISTICAL SEGMENTATION
#   PROGRAM DESCRIPTION - 
#   This program computes recency, frequency and monetary 
#       value of customers based on last purchase, and 
#       average amount of purchases made. Customers are
#       also grouped into segments based on common
#       characteristics.
#   This program also includes code for dendogram, histogram
#       and
# __________________________________________________________
# //////////////////////////////////////////////////////////

# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))


# =================================================================
# Loading data from the OSHA files for years 2008-2011
# =================================================================
# Load text file into local variable called 'data'
OSHA_data9 = read.delim(file = 'ODI2009.txt', header = TRUE, 
                  sep = ',', dec = '.')

OSHA_data8 = read.delim(file = 'ODI2008.txt', header = TRUE, 
                       sep = ',', dec = '.')

OSHA_data10 = read.delim(file = 'ODI2010.txt', header = TRUE, 
                        sep = ',', dec = '.')

OSHA_data11 = read.delim(file = 'ODI2011.txt', header = TRUE, 
                         sep = ',', dec = '.')


# =================================================================
# Re-arranging the NAICS column to comply with the 2-digit
# codes set by OSHA, in a two step process
# =================================================================

# First, select only the first two characters of the NAICS value
OSHA_data10$NAICS = substr(OSHA_data10$NAICS,1,2)
OSHA_data9$NAICS = substr(OSHA_data9$NAICS,1,2)
OSHA_data8$NAICS = substr(OSHA_data8$NAICS,1,2)
OSHA_data11$NAICS = substr(OSHA_data11$NAICS,1,2)

# second, convert string to numeric
OSHA_data10$NAICS = as.numeric(as.character(OSHA_data10$NAICS))
OSHA_data11$NAICS = as.numeric(as.character(OSHA_data11$NAICS))
OSHA_data8$NAICS = as.numeric(as.character(OSHA_data8$NAICS))
OSHA_data9$NAICS = as.numeric(as.character(OSHA_data9$NAICS))


# =================================================================
# checkpoint 1 - optional
# =================================================================
# Display the data after transformation, 
head(OSHA_data8) 
summary(OSHA_data8)



# =================================================================
# Compute total injury count by state using SQL language
# =================================================================
install.packages("sqldf")  # installing library first
library(sqldf)
# this may add/load all other required libraries, 
#eg: gsubfn,  proto, RSQLite, DBI.

# Compute number of accidents per state, for all 4 years.
city_stats8 = sqldf("SELECT STATE, 
                          COUNT(*) AS 'injury_count8'
                          FROM OSHA_data8 GROUP BY 1")

city_stats9 = sqldf("SELECT STATE, 
                          COUNT(*) AS 'injury_count9'
                          FROM OSHA_data9 GROUP BY 1")

city_stats10 = sqldf("SELECT STATE, 
                          COUNT(*) AS 'injury_count10'
                          FROM OSHA_data10 GROUP BY 1")

city_stats11 = sqldf("SELECT STATE, 
                          COUNT(*) AS 'injury_count11'
                    FROM OSHA_data11 GROUP BY 1")

# Adding an year variable to city_statsN tables, so that we can compare
# at a later stage
# this is optional since we have different variable names
# city_stats8$year8 = 2008
# city_stats9$year9 = 2009
# city_stats10$year10 = 2010
# city_stats11$year11 = 2011
    

# collating total injuries per state, for all 4 years together.
# resulting table has 45 states.
# missing states are AK, MN, OR, SC, WA, WY.
injury_ct_all = sqldf("SELECT city_stats8.STATE, city_stats8.injury_count8, 
                    city_stats9.injury_count9, city_stats10.injury_count10,
                    city_stats11.injury_count11
                    from city_stats8, city_stats9, city_stats10, city_stats11
                    where city_stats8.STATE = city_stats9.STATE
                    AND city_stats8.STATE = city_stats10.STATE
                    and city_stats8.STATE = city_stats11.STATE")


# =================================================================
# save results to file
# =================================================================

# write this data to a txt file.
# note if this file does not exist, there is an error.
# Code will NOT create a file.
write.table(injury_ct_all, "c:/ma/OSHA_ANLY_REPORT.txt", sep=",")


# =================================================================
# Here we come to the interesting part - graphs, and 
# data transformations! 
# =================================================================

# First create a range of values (or bins) for the injury counts
bins = c(0,5,10,25,50,100,250,500, 750,1000,1500,2000)

# divide the injury count by these bin values
# this will help with charting later
injct = cut(injury_ct_all$injury_count8,bins)

# view values
injct

# create a histogram
hist(injury_ct_all$injury_count11, breaks = 100)

# data transformation to create frequency distribution table
xout <- as.data.frame(table(injct))
xout <- transform(xout, cumFreq = cumsum(Freq), relative = prop.table(Freq))
xout

# an easy way to view a simple range table
transform(table(injct))



h = hist(injury_ct_all$injury_count11)

# =================================================================
# some more graphs
# =================================================================

# temporary data frame
td = data.frame(city_stats8)

# load ggplot2 library
library(ggplot2)

p1 <- ggplot(td)
p <- ggplot(td, aes(STATE, injury_count8))

p + geom_point()

p + geom_jitter(aes(colour = year))




# =================================================================
# To group states by injury count based on NAICS code
# =================================================================
# dividing into bins
bins = c(11, 21, 22, 23, 31, 42, 44, 48, 51, 52, 
         53, 54, 55, 56, 61, 62, 71, 72, 81, 92)
na10 = cut(OSHA_data10$NAICS ,bins)

transform(table(na10))

hist(OSHA_data10$NAICS , breaks = 100)
xout <- as.data.frame(table(injct))
xout <- transform(xout, cumFreq = cumsum(Freq), relative = prop.table(Freq))
xout
transform(table(injct))


summary(OSHA_data10)


# temporary practice
nac8 = sqldf("SELECT STATE, NAICS, 
                    COUNT(*) AS 'injury_count8'
                    FROM OSHA_data8 GROUP BY 1,2")

nac9 = sqldf("SELECT STATE, NAICS, 
                    COUNT(*) AS 'injury_count9'
             FROM OSHA_data9 GROUP BY 1,2")

nac10 = sqldf("SELECT STATE, NAICS, 
             COUNT(*) AS 'injury_count10'
             FROM OSHA_data10 GROUP BY 1,2")


nac11 = sqldf("SELECT STATE, NAICS, 
             COUNT(*) AS 'injury_count11'
             FROM OSHA_data11 GROUP BY 1,2")

gp8 = data.frame(nac8)
gp9 = data.frame(nac9)

y.rng <- range( c(gp8$injury_count8, gp9$injury_count9) )

# load ggplot2 library
library(ggplot2)

plot(nac8$STATE,nac8$injury_count8,col="red")
plot(gp9$STATE,gp9$injury_count9,type = "1" ,col="green")
lines(nac9$STATE,nac9$injury_count9, col="green")

plot(city_stats8$STATE,city_stats8$injury_count8,type = "2",col="red")
lines(city_stats9$STATE,city_stats9$injury_count9, col="green")
lines(city_stats8$STATE,city_stats8$injury_count8, col="red")


# Code will NOT create a file, unless these files exist.
write.table(nac8, "c:/ma/osha-nac8.txt", sep=",")
write.table(nac9, "c:/ma/osha-nac9.txt", sep=",")
write.table(nac10, "c:/ma/osha-nac10.txt", sep=",")
write.table(nac11, "c:/ma/osha-nac11.txt", sep=",")
plot()
p1 <- ggplot(gp8)

p <- ggplot(gp8, aes(STATE, injury_count8))
p + geom_point()
p + geom_point(position = "jitter")
p + geom_jitter(aes(colour = NAICS))

p <- ggplot(td, aes(STATE, injury_count8))

p + geom_point()
y.rng <- range( c(dat1b$Oats, dat1b$Barley) )
p + geom_jitter(aes(colour = NAICS))
