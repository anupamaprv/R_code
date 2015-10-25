
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
#   This program also includes code for dendogram, histogram,
#   aggregrate, barplot and cluster functions.
# __________________________________________________________
# //////////////////////////////////////////////////////////

# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))


# --- COMPUTING RECENCY, FREQUENCY, MONETARY VALUE ---------


# Load text file into local variable called 'data'
data = read.delim(file = 'purchases.txt', header = FALSE, 
                  sep = '\t', dec = '.')

# Add headers and interpret the last column as a date, 
# extract year of purchase
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')
# this commnad renamed the column headers from V1, V2, V3 to new titles

data$date_of_purchase = as.Date(data$date_of_purchase, "%Y-%m-%d")
# this specifies date in a particular format, so we can use individual 
# components (date, month or year) at a later time. 

data$days_since       = as.numeric(difftime(time1 = "2016-01-01",
                                            time2 = data$date_of_purchase,
                                            units = "days"))
# above command adds a new column "days_since" to object data.
# this column to calculate recency. As value increases, recency decreases.

# Display the data after transformation
head(data)
summary(data)

# Compute key marketing indicators using SQL language
install.packages("sqldf")  # installing library first
library(sqldf)
# this may add/load all other required libraries, 
#eg: gsubfn,  proto, RSQLite, DBI.

# Compute recency, frequency, and average purchase amount
customers = sqldf("SELECT customer_id,
                          MIN(days_since) AS 'recency',
                          COUNT(*) AS 'frequency',
                          AVG(purchase_amount) AS 'amount'
                   FROM data GROUP BY 1")
# there are only 18k records in "customers", as opposed to 51k in "data"
# implying that some customers bought more than one purchase.


# Explore the data


# ============Quiz 1 - step1=============
# =======================================
# modify the segmentation variable "frequency" to replace 
# by its log (before it is scaled) 
customers$frequency = log(customers$frequency)

# Plotting the data for visual conclusions/ summary
hist(customers$recency)
hist(customers$frequency)
hist(customers$amount)
hist(customers$amount, breaks = 25)

# the histogram for cust-amt is left-skewed, so plotting on log scale,
# for better spread-out results.
hist(log(customers$amount) )

# --- PREPARING AND TRANSFORMING DATA ----------------------


# Copy customer data into new data frame
new_data = customers

# Remove customer id as a variable, store it as row names
# the cust-id column is now obsolete and hence deleted.
head(new_data)
row.names(new_data) = new_data$customer_id
new_data$customer_id = NULL
head(new_data)

# Take the log-transform of the amount, and plot
new_data$amount = log(new_data$amount)
hist(new_data$amount)

# Standardize variables, so all values are now on a log scale.
new_data = scale(new_data)
head(new_data)


# --- RUNNING A HIERARCHICAL SEGMENTATION ------------------


# Compute distance metrics on standardized data
# This will likely generate an error on most machines
# d = dist(new_data)
# anu: yes, it does!
# my error message = "Error: cannot allocate vector of size 1.3 Gb"

# Take a 10% sample
sample = seq(1, 18417, by = 10)
head(sample)
customers_sample = customers[sample, ]
new_data_sample  = new_data[sample, ]

# Compute distance metrics on standardized data
d = dist(new_data_sample)
# note from ANU: if sample divided by 2, there are 42398236 elts, 323.8MB
# if sample divided by 2, there are 1695561 elts, 13MB

# Perform hierarchical clustering on distance metrics
c = hclust(d, method="ward.D2")

# Plot de dendogram
plot(c)
# dendogram is the tree structure which shows at what clustering do
# you start losing valuable information.


# Cut at 9 segments , i.e customers divided into 9 buckets.
members = cutree(c, k = 9)

# Show 30 first customers, frequency table
members[1:30]
table(members)

# Show profile of each segment
aggregate(customers_sample[, 2:4], by = list(members), mean)


# try with a diff set of clusters
members3 = cutree(c, k = 3)
members3[1:50]
table(members3)
aggregate(customers_sample[, 2:4], by = list(members3), mean)


# and another grouping with 25 clusters
# try with a diff set of clusters
members25 = cutree(c, k = 25)
members25[1:50]
table(mmembers25)
aggregate(customers_sample[, 2:4], by = list(members25), mean)



# ============Quiz 1 - step2=============
# =======================================
# select a 5-segment solution instead of a 9-segment solution.
members5 = cutree(c, k = 5)
members5[1:30]
table(members5)
aggregate(customers_sample[, 2:4], by = list(members5), mean)



# Simple Bar Plot 
counts <- table(customers$amount)
barplot(counts, main="Count", 
        xlab="Avg. car purchases")

