colors = c("Yellow","Violet","Black")
Schemes <- c("PMGSY-I", "PMGSY-II", "RCPLWEA", "PMGSY-III")
Completed <- c("No.of Roads","Road Length","No.of Bridges")
Values <- matrix(c(15978,6130,5864,5755,46468,562,363,5310,135,1984,29773,96), nrow = 3, ncol = 4, byrow = TRUE)
barplot(Values, main = "Completed Projects", names.arg = Schemes, xlab = "Schemes", ylab = "Sections", col = colors)


x <- c(21723.92,23935.01,24239.66)
Year <- c(2019,2020,2021)
pie(x, Year, main = "Expenditure in Cr", col = rainbow(length(x)))


colors = c("Green","blue")
Years <- c("2014","2015","2016","2017","2018","2019","2020","2021")
Sanctioned <- c("No.of Intake","No of placement")
Values <- matrix(c(43038,21446,236471,109512,162586,147883,131527,75787,241509,137251,247177,150214,38289,49563,23186,22067), nrow = 2, ncol = 8, byrow = TRUE)
barplot(Values, main = "No.of Jobs and placed in each year", names.arg = Years, xlab = "Years", ylab = "Divisions", col = colors)
legend("topright",legend=c("Total No. of intake","No.of jobs"),fill=c("Green","Blue"),border="black")

set.seed(0)
> rular <- c(rnorm(1000,mean = 360 , sd = 9))
> Urban <- c(rnorm(1000,mean=76,sd = 8))
> t.test(rular,Urban,paired = TRUE)
