library(tidyverse)

# Q1) If you invested $1000 in a mutual fund with an average 8% annual yield, how much would you have in 50 years?


# Q2) How much less would you have if the yield were only 7.5%?


# Q3) What if the yield were 8% but you waited five years before investing your $1000?



# Q4) Imagine you spent $1000 on a terrific holiday with friends and put it on your credit card with a 19.99% APR. How long would it take to pay off if you paid $100 per month? How much would that $1000 trip cost you overall? [Use the Repay function]

# Just copy and paste this function into R
Repay <- function(InitialBalance, APR, MonthlyPayment){
  MPR <- APR/12
  # Starting values
  Time <- 0
  Balance <- InitialBalance
  Interest <- 0

  while(last(Balance) > 0){
    Time[Time+2] <- Time + 1
    Interest[last(Time)+1] <- last(Balance)*MPR
    Balance[last(Time)+1] <- last(Balance) + last(Interest) - MonthlyPayment
  }
  df <- data.frame(Time, Interest, Balance, CumInterest = cumsum(Interest))
  df$Balance[df$Balance < 0] <- 0
  return(df)

}
# Example: call like this:
Balances <- Repay(InitialBalance = 500, APR = 0.5, MonthlyPayment = 100)
Balances
# Example: Plot like this
ggplot(Balances, aes(x=Time, y=Balance)) +
  geom_line() +
  geom_line(aes(y=CumInterest), color = "red") +
  scale_x_continuous("Months", breaks = 12*(1:20), minor_breaks = 1:400)



# Q5) What if you only paid $50 per month? The minimum balance of $25 per month?




# Q6) Why is the black line flatter at first and steeper towards the end (ignoring the last payment)?


# Q7) Imagine you buy a house and takout a 20 year loan for $100,000 at 4.5% APR. How much interest would you have paid in total? [Note, adjust the monthly payment until you get 240 months]


# Q8) What if you were able to reduce the rate to 4.25%? How much money would you save?



