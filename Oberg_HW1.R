#' Homework 01 - Tests R datastructures knowledge 
#' Due on: 09/06/2021 

#' Q1. Create two vectors yearsOfExperience	and annualSalary, using the below values: 

#' 1.1	39343.00
#' 1.3	46205.00
#' 1.5	37731.00
#' 2.0	43525.00
#' 2.2	39891.00
#' 2.9	56642.00
#' 3.0	60150.00
#' 3.2	54445.00
#' 3.2	64445.00
#' 3.7	57189.00
#' 3.9	63218.00
#' 4.0	55794.00
#' 4.0	56957.00
#' 4.1	57081.00
#' 4.5	61111.00
#' 4.9	67938.00
#' 5.1	66029.00
#' 5.3	83088.00
#' 5.9	81363.00
#' 6.0	93940.00
#' 6.8	91738.00
#' 7.1	98273.00
#' 7.9	101302.00
#' 8.2	113812.00
#' 8.7	109431.00
#' 9.0	105582.00
#' 9.5	116969.00
#' 9.6	112635.00
#' 10.3	122391.00
#' 10.5	121872.00

yearsOfExperience <- c(1.1, 1.3, 1.5, 2.0, 2.2, 2.9, 3.0, 3.2, 3.2, 3.7, 3.9, 4.0, 4.0, 4.1, 4.5, 4.9, 5.1, 5.3, 5.9, 6.0, 6.8, 7.1, 7.9, 8.2, 8.7, 9.0, 9.5, 9.6, 10.3, 10.5)
annualSalary <- c(39343, 46205, 37731, 43525, 39891, 56642, 60150, 54445, 64445, 57189, 63218, 55794, 56957, 57081, 61111, 67938, 66029, 83088, 81363, 93940, 91738, 98273, 101302, 113812, 109431, 105582,116969, 112635, 122391, 121872)

#' Q2. Take a screenshot of the environment variables created.
#' (Include the screenshot in the word document you upload to moodle)
#' Please describe what is the view in which you can see these variables. 

# includes information that there are 30 values in each variable, 2 variables are identified

#' Q3. Print type of each vector

print(yearsOfExperience)
print(annualSalary)

#' Q4. Create a dataframe as employees using the same two vectors created in Q1 and 
#' name the columns of the dataframe as yearsOfExperience	and annualSalary

employees <- data.frame(yearsOfExperience, annualSalary) 
print(employees)

#' Q5. Create a new column perYearExperience to the dataframe. 
#' The value for this column should be annualSalary/yearsOfExperience

employees$perYearExperience <- c(annualSalary/yearsOfExperience)
print(employees)

#' Q6. Create a logical vector extractRows of five elements with all the element values as TRUE 

extractRows <- c(TRUE, TRUE, TRUE, TRUE, TRUE)

#' Q7. Extract first five rows of employees dataframe using the logical vector extractRows
#' Compare and validate the output using head function. Provide a screenshot in the word document. 

employees[extractRows,]

head(employees,5)


#' Q8. Create an integer vector of values 1 to 30 using a sequence operator and name it as filterCriteria

filterCriteria <- c(1:30)
print(filterCriteria)

#' Q9. Create a logical vector of 30 elements with every 5th element as TRUE value and rest of the elements as FALSE. 
#' Name the vector the same as filterCriteria 
#' (Hint) filterCriteria can be created with a logical operation such as filterCriteria <- filterCriteria < 8 
#' With the above command, first seven elements are TRUE and rest all are FALSE. Use such an arithmatic operation 
#' to create filterCriteria. 
#' e.g: First 6 elements of the newly created filterCriteria vector should look like 
#' filterCriteria : FALSE FALSE FALSE FALSE TRUE FALSE 
#' 

filterCriteria <- filterCriteria == 5 | filterCriteria == 10 | filterCriteria == 15 | filterCriteria == 20 | filterCriteria == 25 | filterCriteria == 30
print(filterCriteria)


#' Q10. Create a new dataframe filteredEmployees from the original dataframe employees using the logical vector
#' filterCriteria created in Q9. 

filteredEmployees <- data.frame(filterCriteria)


#' Q11. Display the first 6 records of the dataframe filteredEmployees using head function 

head(filteredEmployees)

#' Q12. Validate that the first record of filteredEmployees dataframe should be the fifth record of the original 
#' dataframe employees. It should be same as the record extracted using the filter criteria 
#' filteredEmployees[filteredEmployees$yearsOfExperience == 2.2,]

filteredEmployees[filteredEmployees$yearsofExperience == 2.2,]


#' Q13. Display the structure of the prebuilt dataset diamonds 

install.packages("tidyverse")
library(tidyverse)
data("diamonds")
View(diamonds)


#' Q14. Display the first 10 records of the diamonds dataset using head() function and override the parameters 
#' Provide explanation for what do I mean by overriding default parameters. 
#' Hint: Look for online help provided by RStudio 

head(diamonds, n = 10L)
# the default length of head() being the first 6 records, I lengthened the length to 10


#' Q15. Create dataframe goodDiamonds from diamonds dataframe with each diamond cut being "Good" 

goodDiamonds <- data.frame(diamonds[diamonds$cut == "Good",])
print(goodDiamonds)               

#' Q16. display unique values of cut columns of diamonds dataframe 

unique(diamonds[c("cut")])

#' Q17. Assume that diamonds is a sales dataset. You would like to give discount as follow: 
#' 10%, 15%, 20% discount on price of Fair, Good, Very Good diamonds and 25% on Premium & Ideal diamonds 
#' Create a new dataframe column with the updated price 


discountedPrice <- function (category, price) {
  if ( category == "Fair") {
    price <- price - 0.10*price
  } else if ( category == "Good") {
    price <- price - 0.15*price
  } else if ( category == "Very Good") {
    price <- price - 0.20*price
  } else if ( category == "Premium") {
    price <- price - 0.25*price
  } else if ( category == "Ideal") {
    price <- price - 0.25*price
  } else {
    print("No Discount")
  }
}

diamonds$discountedPrice <- discountedPrice(diamonds$cut, diamonds$price)

for (i in 1:53940) { 
  diamonds[i,"discountedPrice"] <- discountedPrice(diamonds[i,2], diamonds[i,7])
} 



#' Q18. Group by diamonds cut and display the count. Output should look like below: 
#' cut       countDiamonds
#' 
#' 1 Fair               1610
#' 2 Good               4906
#' 3 Very Good         12082
#' 4 Premium           13791
#' 5 Ideal             21551 

table(diamonds$cut)


#' Q19. Only display data from diamonds that have a cut value of Fair or Good and a price at or under $600

# data[diamonds$cut == "Fair" |diamonds$cut == "Good") & diamonds$price <= 600),]

library(dplyr)
filter(diamonds, (cut == "Fair" & price <= 600) | (cut == "Good" & price <= 600))

#' Q20. Display the dimensions of preloaded mtcars dataset 

data("mtcars")
View(mtcars)
print(mtcars)

#' Q21. Use preloaded mtcars dataset and create a character vector of cars whose mileage is 21.0

mileage <- c(mtcars$mpg == 21.0)
print(mileage)

#' Q22. Create a factor variable factorCyl using the cyl column of mtcars 
#' with labels as "Four-Cyl", "Six-Cyl", "Eight-Cyl"

factorCyl <- factor(mtcars$cyl, labels = c("Four-Cyl", "Six-Cyl", "Eight-Cyl"))
print(factorCyl)

#' Q23. Create a factor variable factorCyl using the cyl column of mtcars 
#' with labels as "Four-Cyl", "Six-Cyl", "Eight-Cyl". Make the factor of order type 

factorCyl <- ordered(factorCyl, levels = c("Four-Cyl", "Six-Cyl", "Eight-Cyl"))
print(factorCyl)

#' Q24. Display unique values of new column factorCyl and write what you observe 

unique(mtcars[c(factorCyl)])
# every unique car model has a different or unique mpg

#' Q25. Use subset function to extract automatic cars into autoCars and manual cars into manualCars dataframe
#' Use the am column of mtcars dataset to separate the records  
autoCars <- subset(x = mtcars, subset = am == 1)
manualCars <- subset(x = mtcars, subset = am == 0)
print(autoCars)
print(manualCars)
