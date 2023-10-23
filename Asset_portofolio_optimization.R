# use Yahoo Finance API
install.packages("quantmod") #installing quantmod 
library(quantmod)

#step1:
#Get assets info.
stock1 <- getSymbols("IXN", auto.assign=FALSE)
stock2 <- getSymbols("QQQ", auto.assign=FALSE)
stock3 <- getSymbols("BBAX", auto.assign=FALSE) # IEF->BBAX
stock4 <- getSymbols("VNQ", auto.assign=FALSE)
stock5 <- getSymbols("GLD", auto.assign=FALSE)

# Load required libraries
library(quantmod)
library(ggplot2)
library(cowplot)


#step2:
#Let's combine all 5 frames together into 1
# Each asset has OHLC, and Adjusted prices
joined_prices <- merge.xts(stock1, stock2, stock3, stock4, stock5)
# Checking
print(joined_prices)


#step3:
#we just need the Adjusted prices, take out all the noise
# Adjusted price is stored at 6th index
joined_prices_only <- joined_prices[ , seq(from=6, to=ncol(joined_prices), by=6)]

##################################################################################
#### Let's plot these tickers and prices
## which is the best from the rest?
##################################################################################

#step4:
#Ratio of current price relative to asset price on business day one year ago; ratio of increase/decrease in asset price
install.packages("dplyr")
library(dplyr)
library(xts)

#1 What is the most recent 12M*, 18M, 24M (months) return for each of the securities 
#Most recent 12M*, 18M, 24M (months) return for each of the securities

#12M
joined_returns <- as.data.frame(joined_prices_only) %>%
  mutate( IXN_ROR = (IXN.Adjusted - lag(IXN.Adjusted, 250))/lag(IXN.Adjusted), 250) %>%
  mutate( QQQ_ROR = (QQQ.Adjusted - lag(QQQ.Adjusted, 250))/lag(QQQ.Adjusted), 250) %>%
  mutate( IEF_ROR = (BBAX.Adjusted - lag(BBAX.Adjusted, 250))/lag(BBAX.Adjusted), 250) %>% # IEF->BBAX
  mutate( VNQ_ROR = (VNQ.Adjusted - lag(VNQ.Adjusted, 250))/lag(VNQ.Adjusted), 250) %>%
    mutate( GLD_ROR = (GLD.Adjusted - lag(GLD.Adjusted, 250))/lag(GLD.Adjusted), 250)
     #Checking
print(joined_returns)

##18M
joined_returns_18M <- as.data.frame(joined_prices_only) %>%
  mutate( IXN_ROR = (IXN.Adjusted - lag(IXN.Adjusted, 375))/lag(IXN.Adjusted), 375) %>%
  mutate( QQQ_ROR = (QQQ.Adjusted - lag(QQQ.Adjusted, 375))/lag(QQQ.Adjusted), 375) %>%
  mutate( IEF_ROR = (IEF.Adjusted - lag(IEF.Adjusted, 375))/lag(IEF.Adjusted), 375) %>%
  mutate( VNQ_ROR = (VNQ.Adjusted - lag(VNQ.Adjusted, 375))/lag(VNQ.Adjusted), 375) %>%
    mutate( GLD_ROR = (GLD.Adjusted - lag(GLD.Adjusted, 375))/lag(GLD.Adjusted), 375)
#Checking
print(joined_returns_18M)

#24M
joined_returns_24M <- as.data.frame(joined_prices_only) %>%
  mutate( IXN_ROR = (IXN.Adjusted - lag(IXN.Adjusted, 500))/lag(IXN.Adjusted), 500) %>%
  mutate( QQQ_ROR = (QQQ.Adjusted - lag(QQQ.Adjusted, 500))/lag(QQQ.Adjusted), 500) %>%
  mutate( IEF_ROR = (IEF.Adjusted - lag(IEF.Adjusted, 500))/lag(IEF.Adjusted), 500) %>%
  mutate( VNQ_ROR = (VNQ.Adjusted - lag(VNQ.Adjusted, 500))/lag(VNQ.Adjusted), 500) %>%
  mutate( GLD_ROR = (GLD.Adjusted - lag(GLD.Adjusted, 500))/lag(GLD.Adjusted), 500)
#Checking
print(joined_returns_24M)



## Annual return
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

joined_annualreturns <- joined_returns

#2. What are the correlations between your assets? Are there any interesting correlations?
##################################################################
### Covariance matrix and corelations
##################################################################

cor(joined_annualreturns, use='complete.obs')

#######################################################################################

install.packages("forecast")
library(forecast)

# Step a: Convert joined_annualreturns into time series objects
ts_joined_annualreturns <- ts(joined_annualreturns, frequency = 1)

# Step b: Fit ARIMA models to each time series (asset) individually
arima_models <- lapply(ts_joined_annualreturns, function(ts) {
  forecast::auto.arima(ts)
})

# Step c: Use the fitted ARIMA models to generate predicted values
predicted_values <- lapply(arima_models, function(model) {
  forecast(model, h = 1)$mean
})

# Step d: Calculate the correlation matrix based on the predicted values
cor_matrix_predicted <- cor(as.data.frame(predicted_values), use = 'complete.obs')

# Step e: Replace the relevant entries in the original correlation matrix
joined_annualreturns_corr_replaced <- joined_annualreturns
joined_annualreturns_corr_replaced[rownames(cor_matrix_predicted), colnames(cor_matrix_predicted)] <- cor_matrix_predicted

# Display the updated correlation matrix
print(joined_annualreturns_corr_replaced)


# Calculate the date that is one year ago from the current date
current_date <- tail(index(joined_annualreturns), 1)
one_year_ago <- as.Date(current_date) - 365

# Subset the data for the last one year
recent_data <- joined_annualreturns[index(joined_annualreturns) >= one_year_ago, ]

# Calculate the correlation matrix for the last one year
cor_matrix_recent <- cor(recent_data, use = 'complete.obs')

# Display the correlation matrix for the last one year
print(cor_matrix_recent)

#######################################################################################


#Checking
print(joined_annualreturns)

#################################################################
##Calcutate the entire portofolio performance
#################################################################

#calculate the entire portofolio performance

#Base
IXN_alloc <-0.175
QQQ_alloc <-0.221
IEF_alloc <-0.285
VNQ_alloc <-0.089
GLD_alloc <-0.23


joined_portofolio_ret <- as.data.frame(joined_annualreturns) %>%
  mutate(portfolio=IXN_alloc*IXN_ROR+
           QQQ_alloc*QQQ_ROR+
           IEF_alloc*IEF_ROR+
           VNQ_alloc*VNQ_ROR+
           GLD_alloc*GLD_ROR)


##################################################################
### Risk calculation using standard divation 12M
##################################################################
#3. What is the most recent 12M sigma (risk) for each of the securities

time_index <- nrow(joined_annualreturns)

IXN_sigma <- sd(joined_annualreturns$IXN_ROR[time_index:(time_index-11)]) # not -12
QQQ_sigma <- sd(joined_annualreturns$QQQ_ROR[time_index:(time_index-11)]) # not -12
IEF_sigma <- sd(joined_annualreturns$IEF_ROR[time_index:(time_index-11)]) # not -12
VNQ_sigma <- sd(joined_annualreturns$VNQ_ROR[time_index:(time_index-11)]) # not -12
GLD_sigma <- sd(joined_annualreturns$GLD_ROR[time_index:(time_index-11)]) # not -12
GLD_sigma <- sd(joined_annualreturns$GLD_ROR[time_index:(time_index-11)]) # not -12

print(IXN_sigma)
print(QQQ_sigma)
print(IEF_sigma)
print(VNQ_sigma)
print(GLD_sigma)

##################################################################
### Calculating Sharp Risk Metric
##################################################################

riskfree <- 0.0015

#we will calculate the expected returns with the mean()

IXN_sharp <- (mean(joined_annualreturns$IXN_ROR[time_index:(time_index-11)]) - riskfree)/IXN_sigma
QQQ_sharp <- (mean(joined_annualreturns$QQQ_ROR[time_index:(time_index-11)]) - riskfree)/QQQ_sigma
IEF_sharp <- (mean(joined_annualreturns$IEF_ROR[time_index:(time_index-11)]) - riskfree)/IEF_sigma
VNQ_sharp <- (mean(joined_annualreturns$VNQ_ROR[time_index:(time_index-11)]) - riskfree)/VNQ_sigma
GLD_sharp <- (mean(joined_annualreturns$GLD_ROR[time_index:(time_index-11)]) - riskfree)/GLD_sigma

print(IXN_sharp)
print(QQQ_sharp)
print(IEF_sharp)
print(VNQ_sharp)
print(GLD_sharp)

# install and load the necessary libraries
install.packages(c("depmixS4", "ggplot2", "reshape2"))
library(depmixS4)
library(ggplot2)
library(reshape2)

##########################################################################
#Comparing market sentiment and stock price trends using hidden Markov models
##########################################################################

returns <- joined_annualreturns$IEF_ROR

# initialize a two-state model
mod <- depmix(returns ~ 1, nstates=2, data=data.frame(returns=returns))

fit.mod <- fit(mod)

summary(fit.mod)

smoothed_probs <- posterior(fit.mod)

df_probs <- as.data.frame(smoothed_probs)

df_probs$Date <- index(joined_annualreturns)

df_probs_long <- reshape2::melt(df_probs, id.vars = "Date")

# Create a dataframe with IEF_ROR
df_prices <- data.frame(Date=index(joined_annualreturns), Price=joined_annualreturns$IEF_ROR)

# Use ggplot2's facet_wrap feature to plot the stock prices and the smoothed probabilities on the same x-axis (Date).
ggplot() +
  geom_line(data=df_prices, aes(x=Date, y=Price), color='blue') +
  geom_line(data=df_probs_long, aes(x=Date, y=value, color=variable)) +
  facet_wrap(.~variable, scales='free', ncol=1) +
  labs(x="Date", y="Value", color="State",
       title="Stock Price and Smoothed Probabilities of the Hidden States")


##################################################################
### Covariance matrix and corelations
##################################################################

cov(joined_annualreturns, use='complete.obs')

##################################################################

#step5:
#Portfolio optimization and verification of its effectiveness through efficient frontier visualization

library(data.table)
library(scales)
library(ggplot2)
library(quantmod)
library(reshape2)

ticker1 <- "IXN"
ticker2 <- "QQQ"
ticker3 <- "BBAX"
ticker4 <- "VNQ"
ticker5 <- "GLD"

mydf1 <- as.data.frame(getSymbols(ticker1, auto.assign=FALSE))
mydf2 <- as.data.frame(getSymbols(ticker2, auto.assign=FALSE))
mydf3 <- as.data.frame(getSymbols(ticker3, auto.assign=FALSE))
mydf4 <- as.data.frame(getSymbols(ticker4, auto.assign=FALSE))
mydf5 <- as.data.frame(getSymbols(ticker5, auto.assign=FALSE))

combined_df <- cbind(mydf1[,6], mydf2[,6], mydf3[,6], mydf4[,6], mydf5[,6])

dt <- as.data.frame(combined_df)
colnames(dt) <- c(ticker1, ticker2, ticker3, ticker4, ticker5)
dt$date = as.Date(rownames(mydf1))
dt <- melt(dt, id="date")
colnames(dt) <- c("date", "ticker","price")
dt <- data.table(dt)
# create indexed values
dt[, idx_price := price/price[1], by = ticker]
# plot the indexed values
ggplot(dt, aes(x = date, y = idx_price, color = ticker)) +
  geom_line() +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Price Developments") +
  xlab("Date") + ylab("Pricen(Indexed 2000 = 1)") +
  scale_color_discrete(name = "Company")

# calculate the arithmetic returns
dt[, ret := price / shift(price, 1) - 1, by = ticker]

# summary table
# take only non-na values
tab <- dt[!is.na(ret), .(ticker, ret)]

# calculate the expected returns (historical mean of returns) and volatility (standard deviation of returns)
tab <- tab[, .(er = round(mean(ret), 4),
               sd = round(sd(ret), 4)),
           by = "ticker"]

ggplot(tab, aes(x = sd, y = er, color = ticker)) +
  geom_point(size = 5) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Risk-Return Tradeoff") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, 0.03)) +
  scale_x_continuous(label = percent, limits = c(0, 0.1))


#####################################################
## Three risky assets portfolio:
#####################################################
#Use when picking up and evaluating three assets.
# load the data
# select 3 assets
combined_df_select <- cbind(mydf1[,6], mydf2[,6], mydf5[,6])

df <- as.data.frame(combined_df_select)
colnames(df) <- c("x","y", "z")
df$date = as.Date(rownames(mydf1))


# calculate the necessary values:
# I) expected returns for the two assets
er_x <- mean(df$x)
er_y <- mean(df$y)
er_z <- mean(df$z)

# II) risk (standard deviation) as a risk measure
sd_x <- sd(df$x)
sd_y <- sd(df$y)
sd_z <- sd(df$z)

# III) covariance
cov_xy <- cov(df$x, df$y)
cov_xz <- cov(df$x, df$z)
cov_yz <- cov(df$y, df$z)

# create portfolio weights (omegas)
x_weights <- seq(from = 0, to = 1, length.out = 1000)

# create a data.table that contains the weights for the three assets
three_assets <- data.table(wx = rep(x_weights, each = length(x_weights)),
                           wy = rep(x_weights, length(x_weights)))

three_assets[, wz := 1 - wx - wy]


# calculate the expected returns and standard deviations for the 1000 possible portfolios
three_assets[, ':=' (er_p = wx * er_x + wy * er_y + wz * er_z,
                     sd_p = sqrt(wx^2 * sd_x^2 +
                                   wy^2 * sd_y^2 +
                                   wz^2 * sd_z^2 +
                                   2 * wx * wy * cov_xy +
                                   2 * wx * wz * cov_xz +
                                   2 * wy * wz * cov_yz))]

# take out cases where we have negative weights (shortselling)
three_assets <- three_assets[wx >= 0 & wy >= 0 & wz >= 0]
three_assets

# lastly plot the values
ggplot() +
  geom_point(data = three_assets, aes(x = sd_p, y = er_p, color = wx - wz)) +
  geom_point(data = data.table(sd = c(sd_x, sd_y, sd_z), mean = c(er_x, er_y, er_z)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Possible Portfolios with Three Risky Assets") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(three_assets$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(three_assets$sd_p) * 1.2)) +
  scale_color_gradientn(colors = c("red", "blue", "yellow"),
                        name = expression(omega[x] - omega[z]), labels = percent)

