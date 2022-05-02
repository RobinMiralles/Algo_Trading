# Robin MIRALLES - 25/03/2022
# S&P trading automation with VIX and moving average

# packages for trading automation : 
# https://bookdown.org/kochiuyu/technical-analysis-with-r-second-edition/intra-day-trading-rule.html
# but here, we are going to implement the strategy directly "from scratch"
# benchmark : HOLD the S&P --> can we beat the market ?

#------------------------------------------------------------------------------#
# PACKAGES : 
library(BatchGetSymbols) 
library(tseries) 
library(lubridate)
library(quantmod)              # <3
library(ggplot2)
library(grid)  
library(gridExtra)
library(gtools)
library(ggpubr)
library(e1071)
library(ggthemes)
library(furrr)
library(purrr)


#------------------------------------------------------------------------------#
# FUNCTIONS : 
`%!in%` = function(x,y)!('%in%'(x,y))

signal_take_position = function(MA_i_t, MA_j_t, VIX_t, k, l){
  # cf strategy description
  # return "buy", "sell" or "nothing"
  signal_position_t = ifelse((MA_i_t > MA_j_t) & (VIX_t < k | VIX_t > l), "buy",
                             ifelse((MA_i_t < MA_j_t) & (VIX_t >= k & VIX_t <= l), "sell",
                                    "do nothing"))
  return(signal_position_t)
}

signal_close_position = function(SP_t, stoploss, takeprofit, type_open, quantity_open, cash_flow_open,position_value_open){
  # return "close" or "do nothing" on the opening position, knowing market's data
  current_value = SP_t*quantity_open
  possible_type_close = c("buy","sell")
  if_close_type = possible_type_close[-which(possible_type_close == type_open)]
  if_close_cash_flow = current_value*ifelse(if_close_type == "buy", -1, 1)
  if_close_profit = cash_flow_open + if_close_cash_flow
  threshold_loss = -stoploss*abs(position_value_open)
  threshold_gain = takeprofit*abs(position_value_open)
  signal_close_position_t = ifelse((if_close_profit < threshold_loss) | (if_close_profit > threshold_gain), "close", "do nothing")
  return(signal_close_position_t)
}

get_data = function(start,end){
  VIX = getSymbols("^VIX",src = 'yahoo', from= as.Date(start), to = end, auto.assign = F)
  VIX = na.omit(VIX)
  SP = getSymbols("^GSPC",src = 'yahoo', from= as.Date(start)-200, to = end, auto.assign = F)
  # minus 200 so that we can compute Moving average
  SP = na.omit(SP)
  return(list("SP" = SP, "VIX" = VIX))
}

strategy_diffusion = function(start, end, i, j, k, l, wealth, stoploss, takeprofit){
  # main function : trading strategy implementation
  # return the argument of the strategy (start, end, i, j, k, l, wealth, max_exposure, stoploss, takeprofit)
  # and the record of all the position taken over time
  
  cat("\n")
  cat("\n DATA ACQUISITION")
  cat("\n START : ", start)
  cat("\n END : ", end)
  
  data = get_data(start,end)            # get S&P500 and VIX between start and end
  SP = data$SP
  VIX = data$VIX
  
  dates = index(SP)
  
  open_position = FALSE                  # at the beginning, we have no current position
  
  t = dates[which(dates == start)]       # first date of trading 
  
  SP$SMA_i = SMA(Cl(SP), n = i)          # moving average of S&P on i lookback window days
  SP$SMA_j = SMA(Cl(SP), n = j)          # moving average of S&P on j lookback window days
  
  nb_position = 0                        # number of position taken : 0 at the begining
  positions = c()                        # data frame where we save positions we take
  
  portfolio_value = c(wealth)            # vector where we save the value of the position over time
  portfolio_value_dates = c(t)
  
  # strategy : with i<j and k<l
  #
  #             if (MA_i > MA_j) & (VIX < k or VIX > l)    --> buy
  #             if (MA_i < MA_j) & (k < VIX < l)           --> sell
  
  cat("\n BEGINING OF THE DIFFUSION : (i,j,k,l,wealth,stoploss,takeprofit) = (", i,
      ",", j,",", k,",", l,",",wealth,",", stoploss,",", takeprofit,")", sep = "")
  
  while(t < (as.Date(end)-1)){
    # values we need at time = t to compute the strategy : 
    VIX_t = VIX$VIX.Adjusted[which(index(VIX) == t)][[1]]
    SP_t = SP$GSPC.Adjusted[which(index(SP) == t)][[1]]
    MA_i_t = SP$SMA_i[which(index(SP) == t)][[1]]
    MA_j_t = SP$SMA_j[which(index(SP) == t)][[1]]
    
    # if no current position : 
    if(open_position == FALSE){
      
      signal_position_t = signal_take_position(MA_i_t, MA_j_t, VIX_t, k, l)
      
      # if signal tells us to do nothing :
      if(signal_position_t == "do nothing"){
        portfolio_value = c(portfolio_value, portfolio_value[length(portfolio_value)]) # portfolio value doesn't change : no money invested
        portfolio_value_dates = c(portfolio_value_dates, t)
        t = dates[which(dates == t) + 1] 
        next
      }
      
      # if signal tells us to buy or sell :
      if(signal_position_t != "do nothing"){
        position_value = wealth                                                    # max money we can invest 
        quantity = position_value/SP_t                                             # quantity of S&P we can sell/buy with a maximum amount of wealth*max_exposure
        cash_flow = position_value*ifelse(signal_position_t == "buy",-1,+1)        # cash flow : negative if long, positive if short
        wealth = wealth + cash_flow                                                # wealth changes
        nb_position = nb_position + 1                                              # we take a position --> number of position = +1
        new_position = list(t, SP_t, VIX_t, MA_i_t, MA_j_t, signal_position_t,    
                            position_value,quantity, cash_flow, wealth)            # position's characteristics
        positions[[nb_position]] = new_position                                    # storage of the position's characteristics
        open_position = TRUE                                                       # we are now in a "current open position" phase
        portfolio_value = c(portfolio_value, portfolio_value[length(portfolio_value)]) # portfolio value doesn't change : no loss or gain at the opening position
        portfolio_value_dates = c(portfolio_value_dates, t)
        t = dates[which(dates == t) + 1]                                           # next date of the while loop
        next
      }
    }
    
    # if current open position :
    if(open_position == TRUE){
      # we need some informations about the open position : 
      type_open = as.character(positions[[length(positions)]][6])                # did we take a long or short position at the begining of the position
      position_value_open = as.numeric(positions[[length(positions)]][7])        # amount of the open position
      quantity_open = as.numeric(positions[[length(positions)]][8])              # quantity of the current position
      cash_flow_open = as.numeric(positions[[length(positions)]][9])             # opening cash flow involved in the current position
      wealth_open = as.numeric(positions[[length(positions)]][10])               # amount of wealth at the opening
      # computation of the close signal : 
      signal_close_position_t = signal_close_position(SP_t, stoploss, takeprofit, type_open,
                                                      quantity_open, cash_flow_open,position_value_open) # do nothing or close the position
      
      portfolio_value = c(portfolio_value, SP_t*quantity_open) # portfolio value change with the actual S&P value
      portfolio_value_dates = c(portfolio_value_dates, t)
      
      # if it's not the moment to close our current position : 
      if(signal_close_position_t == "do nothing"){
        t = dates[which(dates == t) + 1] 
        next
      }
      
      # if it's the moment to close our current position : 
      if(signal_close_position_t == "close"){
        current_value = SP_t*quantity_open                                         # current value of the quantity sold/both at the opening of the position
        possible_type_close = c("buy","sell")                                      # since open position : either we buy either we sell
        close_type = possible_type_close[-which(possible_type_close == type_open)] # we take the opposite type of the opening position at the closing
        close_cash_flow = current_value*ifelse(close_type == "buy", -1, 1)         # cash flow at the closing
        wealth = wealth_open + close_cash_flow                                     # wealth after closing the position 
        nb_position = nb_position + 1  
        new_position = list(t, SP_t, VIX_t, MA_i_t, MA_j_t, close_type,    
                            current_value, quantity_open, close_cash_flow, wealth)  # position's characteristics
        positions[[nb_position]] = new_position                                     # storage of the position's characteristics
        open_position = FALSE                                                       # since position closed, no more current open position
        t = dates[which(dates == t) + 1] 
        next
      }
    }
  }
  position_history = as.data.frame(do.call(rbind, positions))
  colnames(position_history) = c("date","SP_t",'VIX_t',"MA_i_t","MA_j_t","type","position_value","quantity","cash_flow","wealth")
  position_history$date = as_date(unlist(position_history$date))
  position_history$SP_t = as.numeric(unlist(position_history$SP_t))
  position_history$VIX_t = as.numeric(unlist(position_history$VIX_t))
  position_history$MA_i_t = as.numeric(unlist(position_history$MA_i_t))
  position_history$MA_j_t = as.numeric(unlist(position_history$MA_j_t))
  position_history$type = as.character(unlist(position_history$type))
  position_history$position_value = as.numeric(unlist(position_history$position_value))
  position_history$quantity = as.numeric(unlist(position_history$quantity))
  position_history$cash_flow = as.numeric(unlist(position_history$cash_flow))
  position_history$wealth = as.numeric(unlist(position_history$wealth))
  if(nrow(position_history)%%2 == 1){
    position_history = position_history[-nrow(position_history),]                # make sense for a couple of transaction --> if the last one is alone we delete it
  }
  
  portfolio_value = data.frame("date" = portfolio_value_dates, "value" = portfolio_value)
  
  cat("\n END OF THE DIFFUSION")
  cat("\n")
  
  return(liste = list("start" = start, "end" = end, "i" = i, "j" = j, "k" = k,
                      "l" = l, "wealth" = wealth,
                      "stoploss" = stoploss, "takeprofit" = takeprofit,
                      "position_history" = position_history,
                      "portfolio_value" = portfolio_value))
}

performance_strategy_diffusion = function(position_history, portfolio_value, initial_wealth){
  # compute the P&L dataframe and different performance indicators of the strategy
  
  # first we compute the Profit and Loss dataframe : 
  date_open = rep(0,nrow(position_history)/2)
  date_close = rep(0,nrow(position_history)/2)
  profit = rep(0,nrow(position_history)/2)
  SP_open = rep(0,nrow(position_history)/2)
  SP_close = rep(0,nrow(position_history)/2)
  VIX_open = rep(0,nrow(position_history)/2)
  VIX_close = rep(0,nrow(position_history)/2)
  quantity = rep(0,nrow(position_history)/2)
  type_open = rep(0,nrow(position_history)/2)
  type_close = rep(0,nrow(position_history)/2)
  j = 1
  for(i in seq(1,nrow(position_history)-1,2)){
    date_open[j] = position_history$date[i]
    date_close[j] = position_history$date[i+1]
    profit[j] = position_history$cash_flow[i] + position_history$cash_flow[i+1] 
    SP_open[j] = position_history$SP_t[i]
    SP_close[j] = position_history$SP_t[i+1]
    VIX_open[j] = position_history$VIX_t[i]
    VIX_close[j] = position_history$VIX_t[i+1]
    quantity[j] = position_history$quantity[i]
    type_open[j] = position_history$type[i]
    type_close[j] = position_history$type[i+1]
    j = j + 1
  }
  PL = data.frame("date_open" = date_open, "date_close" = date_close, "SP_open" = SP_open,
                  "SP_close" = SP_close, "quantity" = quantity, "type_open" = type_open,
                  "type_close" = type_close,"VIX_open" = VIX_open, "VIX_close" = VIX_close, 
                  "profit" = profit)
  PL$cumulated_profit = cumsum(PL$profit)
  PL$wealth = PL$cumulated_profit + initial_wealth
  PL$position_length = PL$date_close - PL$date_open
  PL$date_open = as_date(PL$date_open)
  PL$date_close = as_date(PL$date_close)
  
  
  # then we compute few statistics :
  end_profit = round(PL$cumulated_profit[nrow(PL)],2)
  mean_position_length = mean(PL$position_length) # mean length (days) of the position
  percentage_good = round(length(PL$profit[PL$profit > 0])/length(PL$profit),4)
  gain_pain_ratio = round(sum(PL$profit[PL$profit > 0])/sum(abs(PL$profit[PL$profit < 0])),4)
  sd_gain = round(sd(PL$profit),2)
  mean_gain = round(mean(PL$profit),2)
  skew = round(skewness(PL$profit),4)
  kurtosis = round(kurtosis(PL$profit),4)
  
  daily_return = diff(portfolio_value$value)/portfolio_value$value[1:(nrow(portfolio_value)-1)]
  volatility = sqrt(252)*sd(daily_return)
  mean_return = 252*mean(daily_return)
  sharpe_ratio = round(mean_return/volatility,4)
  
  initial_wealth = PL$wealth[1] - PL$profit[1]
  ending_wealth = initial_wealth + end_profit
  return = round((ending_wealth/initial_wealth) - 1,4)
  period = as_date(portfolio_value$date[length(portfolio_value$date)]) - as_date(portfolio_value$date[1])
  return_y = round((1+return)^(365/as.numeric(period))-1,4)
  
  # Value at Risk, Conditional Tail Expectation, Expected Shortfall
  VaR = quantile(PL$profit, probs = c(0.01, 0.05))
  VaR_99 = round(VaR[1],2)
  VaR_95 = round(VaR[2],2)
  CTE_95 = round(mean(PL$profit[PL$profit <= VaR_95]),2)
  ES_95 = round(mean(ifelse(PL$profit < VaR_95, 1,0)*PL$profit),2)    # ES Stop loss premium with a retention equal to the value at risk
  
  return(liste = list("PL" = PL, "portfolio_value" = portfolio_value,
                      "end_profit" = end_profit, "ending_wealth" = ending_wealth,
                      "return" = return, "period" = period,
                      "return_y" = return_y,
                      "sharpe_ratio" = sharpe_ratio,
                      "mean_return" = mean_return,
                      "volatility" = volatility,
                      "mean_position_length" = mean_position_length,
                      "percentage_good" = percentage_good, 
                      "gain_pain_ratio" = gain_pain_ratio,
                      "sd_profit" = sd_gain,
                      "mu_profit" = mean_gain,
                      "skew" = skew,
                      "kurtosis" = kurtosis,
                      "VaR_99" = VaR_99,
                      "VaR_95" = VaR_95,
                      "CTE_95" = CTE_95,
                      "ES_95" = ES_95))
}

get_benchmark_hold = function(start, end, initial_wealth){
  SP = getSymbols("^GSPC",src = 'yahoo', from= start, to = end, auto.assign = F)
  date = index(SP)
  SP = as.data.frame(na.omit(SP))
  SP$date = date
  quantity = initial_wealth/coredata(SP$GSPC.Adjusted[1])    # quantity of S&P500 we can afford at the begining with initial_wealth
  hold = data.frame("date"=SP$date, "value" = SP$GSPC.Adjusted*quantity)
  
  end_profit = round(hold$value[length(hold$value)] - hold$value[1],2)
  ending_wealth = initial_wealth + end_profit
  return = round((hold$value[length(hold$value)]/hold$value[1] - 1),4)
  period = as_date(hold$date[length(hold$date)]) - as_date(hold$date[1])
  return_y = round((1+return)^(365/as.numeric(period))-1,4)
  
  daily_return = diff(hold$value)/hold$value[1:(nrow(hold)-1)]
  sharpe_ratio = round(sqrt(252)*mean(daily_return)/sd(daily_return),4)
  volatility = round(sqrt(252)*sd(daily_return),4)
  mean_return = round(252*mean(daily_return),4)
  
  return(liste = list("df" = hold, "end_profit" = end_profit, "ending_wealth" = ending_wealth,
                      "return" = return,
                      "period" = period, "return_y" = return_y,
                      "sharpe_ratio" = sharpe_ratio, "volatility" = volatility,
                      "mean_return" = mean_return))
    
}

strategy_graph = function(start, end, i, j, k, l, stoploss, takeprofit, PSD){
  # display synthetic graphs of the strategy
  theme_Bloomberg <-   theme(plot.background=element_rect(fill="#000000"),
                             panel.background=element_rect(fill="grey75"),
                             panel.grid.minor=element_line(linetype=3,size=0.1),
                             panel.grid.major=element_line(linetype=3,size=0.4),
                             panel.border = element_rect(colour = "white", fill=NA, size=0.5),
                             plot.title = element_text(colour = "white",size=15,face="bold"),
                             axis.text=element_text(size=15, colour="white"),
                             axis.title=element_text(size=15,face="bold",colour="white"),
                             legend.text=element_text(size=10))
  
  PL = PSD$PL
  PV = PSD$portfolio_value[-1,]
  p_profit_hist = ggplot(PL, aes(x=profit)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 50) +
    geom_density(alpha=.2, fill="#FF6666") +
    geom_vline(aes(xintercept=mean(profit)),
               color="blue", linetype="dashed", size=1) +
    theme_Bloomberg
  
  
  hold = get_benchmark_hold(PV$date[1], PV$date[length(PV$date)], PV$value[1])
  p_profit = ggplot() +
    geom_step(data = PL, aes(x = date_close, y = wealth), color = "darkblue") + 
    geom_line(data = PV, aes(x = date, y = value), color = "firebrick", linetype = "dashed") +
    geom_line(data = hold$df, aes(x = date, y =value), color = "darkgreen") +
    xlab("Date") +
    ylab("Wealth over time") +
    theme_Bloomberg
  
  infos_strat = paste(paste("TRADING FROM", start, "TO", end),
                "MA/VOL TRADING STRATEGY",
                paste("begining wealth :", PSD$portfolio_value$value[1]),
                paste("ending wealth :", PSD$ending_wealth),
                paste("end profit : ", PSD$end_profit), 
                paste("return : ", PSD$return*100,"%"),
                paste("period : ", PSD$period, "days"),
                paste("annualized return : ", PSD$return_y*100,"%"),
                paste("sharpe ratio : ", PSD$sharpe_ratio),
                paste("mean return : ", round(PSD$mean_return,4)),
                paste("volatility : ", round(PSD$volatility,4)),
                paste("mean position length : ", round(PSD$mean_position_length,2), "days"),
                paste("percentage good position : ", PSD$percentage_good*100, "%"),
                paste("gain/pain ratio : ", PSD$gain_pain_ratio),
                paste("standard deviation of profits : ", PSD$sd_profit),
                paste("mean profit : ", PSD$mu_profit),
                paste("skewness : ", PSD$skew),
                paste("kurtosis : ", PSD$kurtosis),
                paste("VaR99% : ", PSD$VaR_99),
                paste("VaR95% : ", PSD$VaR_95),
                paste("CTE95% : ", PSD$CTE_95),
                paste("Expected Shortfall 95% : ", PSD$ES_95),
                sep="\n")
  p_info_strat = ggplot() +                     
    annotate("text",
             x = 1,
             y = 1,
             size = 3,
             label = infos_strat,
             color = "darkblue") + 
    theme_void()
  

  infos_hold = paste("BENCHMARK : HOLD STRATEGY",
                paste("begining wealth :", hold$df$value[1]),
                paste("ending wealth :", hold$ending_wealth),
                paste("end profit : ", hold$end_profit), 
                paste("return : ",hold$return*100,"%"),
                paste("period : ", as.numeric(hold$period) , "days"),
                paste("annualized return : ", hold$return_y*100,"%"),
                paste("sharpe ratio : ", hold$sharpe_ratio),
                paste("mean return : ", hold$mean_return),
                paste("volatility : ", hold$volatility),
                sep="\n")
  p_info_hold = ggplot() +                     
    annotate("text",
             x = 1,
             y = 1,
             size = 3,
             label = infos_hold,
             color = "darkgreen") + 
    theme_void()
  
  title = paste("S&P500/VIX strategy based trading : (i,j,k,l,stoploss,takeprofit) = (", i,
                ",", j,",", k,",", l,",", stoploss,",", takeprofit,")", sep = "")
  p = grid.arrange(p_profit, p_info_strat,  p_profit_hist, p_info_hold, ncol= 2, nrow=2, heights=c(4, 2),
                   widths = c(5,2),top = textGrob(title, gp=gpar(fontsize=10,font=3)))
  
}

VOL_MA_TRADING = function(start, end, i, j, k, l, wealth, stoploss, takeprofit){
  # mother function : gather all the function of the trading strategy so that
  # user just have to run this function
  
  SD = strategy_diffusion(start, end, i, j, k, l, wealth, stoploss, takeprofit)
  PSD = performance_strategy_diffusion(SD$position_history, SD$portfolio_value,wealth)
  strategy_graph(start,end,i, j, k, l, stoploss, takeprofit, PSD)
  return(liste = list("SD" = SD, "PSD" = PSD))
}


#------------------------------------------------------------------------------#
# DATA VALIDATION : 

start = "1990-01-03"
end = "2020-12-31"
data = get_data(start,end)
SP = data$SP
VIX = data$VIX
sum(index(VIX) %!in% index(SP)) # ok then : dates match in both dataset


#------------------------------------------------------------------------------#
# GRAPHS : S&P500 AND VIX

chartSeries(SP, theme = "white", type = "line", TA = NULL)
addTA(VIX$VIX.Adjusted, legend = "VIX")
# note : we took more S&P value in order to compute MA

#------------------------------------------------------------------------------#
# EXAMPLE OF A DIFFUSION OF THIS STRATEGY : 

start = "1990-01-03"
end = "2021-12-31"
i = 15                              # MA_i
j = 75                              # MA_j
k = 25                              # limit inf for VIX
l = 35                              # limit sup for VIX
wealth = 10000                      # initial capital invested in the trading strategy
stoploss = 0.1                      # % max of the position at t we tolerate to loose
takeprofit = 0.1                    # % of the position at which we take the profit

SD = strategy_diffusion(start, end, i, j, k, l, wealth, stoploss, takeprofit)
PSD = performance_strategy_diffusion(SD$position_history, SD$portfolio_value, wealth)
strategy_graph(start,end,i,j,k,l,stoploss,takeprofit,PSD)
SD$position_history
SD$portfolio_value
PSD$PL

#------------------------------------------------------------------------------#
# OPTIMISATION OF PARAMETERS : 

no_cores = availableCores() 
future::plan(multicore, workers = no_cores)

VOL_MA_TRADING_for_optimization = function(i, spreadj, k, spreadl, stoploss, takeprofit){
  start_train = "1990-01-03"
  end_train = "2005-12-31"
  j = i + spreadj
  l = k + spreadl
  SD = strategy_diffusion(start_train, end_train, i, j, k, l, 10000, stoploss, takeprofit)
  PSD = performance_strategy_diffusion(SD$position_history, SD$portfolio_value,wealth)
  characteristics = c(i,j,k,l,stoploss,takeprofit,unlist(PSD[3:length(PSD)]))
  return(characteristics)
}

vec_i = c(10)                              
vec_spreadj = c(55,60,65)                           
vec_k = c(24,25,26)                        
vec_spreadl = c(5,7,8,10)                    
vec_stoploss = c(0.07,0.08,0.09,0.1)        
vec_takeprofit = c(0.025, 0.026, 0.027, 0.028, 0.029 ,0.03)    
grid = expand.grid(vec_i, vec_spreadj, vec_k, vec_spreadl, vec_stoploss, vec_takeprofit)
colnames(grid) = c("i","spreadj","k","spreadl","stoploss","takeprofit")
nrow(grid)
# take at least 20 :
optim = pmap(list(grid[,1], grid[,2], grid[,3], grid[,4], grid[,5], grid[,6]),
             VOL_MA_TRADING_for_optimization) # or future_pmap but parallelization doesn't work...
optim_df = as.data.frame(do.call(rbind, optim))
optim_df[order(optim_df$ending_wealth, decreasing = T),]


#------------------------------------------------------------------------------#
# BEST I CAN DO : 

i = 10                    # MA_10
j = i + 60                # MA_70
k = 25                    # VIX_inf = 25
l = k + 10                # VIX_sup = 30
wealth = 10000            # amount we want to invest
stoploss = 0.1            # stoploss = 10% of the opening position = threshold of pain
takeprofit = 0.03         # takeprofit = 3% of the opening position = threshold of happiness
  
start_train = "1990-01-03"
end_train = "2005-12-31"
train = VOL_MA_TRADING(start_train, end_train, i, j, k, l, wealth, stoploss, takeprofit)

start_test = "2006-01-03"
end_test = "2022-04-01"
test = VOL_MA_TRADING(start_test, end_test, i, j, k, l, wealth, stoploss, takeprofit)


# we can test the normality of the profit : 
ggqqplot(test$PSD$PL$profit)                    # profit are not normaly distributed
shapiro.test(test$PSD$PL$profit)                # pvalue <<< 0.01 --> reject HO --> reject normality hypothesis
ks.test(test$PSD$PL$profit, 'pnorm')            # pvalue <<< 0.01 --> reject HO --> reject normality hypothesis


#------------------------------------------------------------------------------#

# mawdrawdown refaire 
# identifier un rectificateur de signal par rapport aux mauvais paris : ML ou NN
# autre strat : achat close, vente à louverture en guise de 2nd benchmark
# modele de Cox 
# autre strat : utiliser le RSI, ou bien distinguer (MAi1,MAj1) et (MAi2,MAj2)
# pour deux regimes de VIX differents (adaptative MA)















