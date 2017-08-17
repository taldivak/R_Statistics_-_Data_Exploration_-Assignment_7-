#mins Look at the data given below. Plot the data, find the outliers and find out mean, sigma and 


#Name of company    Measure X
#Allied Signal      24.23%
#Bankers Trust      25.53%
#General Mills      25.41%
#ITT Industries     24.14%
#J.P.Morgan & Co.   29.62%
#Lehman Brothers    28.25%
#Marriott           25.81%
#MCI                24.39%
#Merrill Lynch      40.26%
#Microsoft          32.95%
#Morgan Stanley     91.36%
#Sun Microsystems   25.99%
#Travelers          39.42%
#US Airways         26.71%
#Warner-Lambert     35.00%


Companies<-read.csv("C://Users/User/Desktop/Company_Names.csv",header = TRUE,sep=",")
View(Companies)

str(Companies)

# Converting the table to a dataframe
tab_conv<-data.frame(Companies)
View(tab_conv)

percent_ages<-tab_conv[,2] <- as.numeric(gsub("%", "",tab_conv[,2]))/100
percent_ages


outliers <- boxplot.stats(tab_conv$Measure.X)$out  # outlier values.
outliers


# Plotting of Graphs and Charts

boxplot(tab_conv$Measure.X, main="Valuation",boxwex=0.1)
mtext(paste("Outliers: ", paste(outliers, collapse=",")), cex=0.6)

plot(tab_conv)



plot(tab_conv, type="o", col="black",
     main="Company's Review", col.main="purple", font.main=4)

# Box Plot

boxplot(tab_conv$Measure.X, main="Box plot", ylab="Measure.X")
boxplot(Measure.X ~ Name.of.company , data=df,
        main="Box Plot", xlab="Name.of.company",ylab="Measure.X")
boxplot(tab_conv)

mean(tab_conv$Measure.X)
sd(tab_conv$Measure.X)
var(tab_conv$Measure.X)

#Ans 2(i) 
UQ<-12
LQ<-5
IR<-(UQ-LQ)
IR

# IR Val is 7
#Inter-quartile range is the difference between Upper and Lower Quartiles

# Ans 2(ii)
#  The distribution  of values in the given dataset is negatively skewed 

# Ans 2(iii)
# Mean and Median values would differ.Also Sample Means and Variance values
#would differ.
# F Statistics or ANOVA would be impacted as a result.


# Ans 3(i)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(IR)

# Ans 3(ii)
# Ans The given dataset is positively skewed.majority of the values in the
# data set are on the right side of the mean and the median.

# Ans 3(iii)
# Histogram depicts frequency distribution
# Box-plot depicts the summary of the distributed values.
# As both Histogram and Box-plot can be used for Numeric values, they
# complement each other.

