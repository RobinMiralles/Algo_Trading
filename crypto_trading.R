# CRYPTO TRADING BITCOIN - Robin Miralles # 

# high volatility, open access --> algo trading <3
# connect R to a broker : 
# https://towardsdatascience.com/how-to-build-an-automated-trading-system-using-r-34892b6d3320

# dynamic trading : loop over time and computation of information at each time
# allows better understanding of code (example : boolean --> are we currently in an
# open position, etc...)

# philosophy : "divide for better reign" --> small function to "mother" function
#              at the end, run one function with parameters of the strategy to get 
#              graphs and statistics. Variable's name quite understandable.

# parameters optimization on the train set has been implemented, but not here 
# in the code because of the constraint of running time (5min max)

#------------------------------------------------------------------------------#
# PACKAGES
library(dplyr)
library(lubridate)
library(TTR)
library(ggplot2)
library(crypto2)
library(tictoc)
library(e1071)
library(grid)  
library(gridExtra)
library(data.table)
library(gtools)
library(cowplot)
library(PerformanceAnalytics)

#------------------------------------------------------------------------------#
# FUNCTIONS (base)

`%!in%` = function(x,y)!('%in%'(x,y))

get_data = function(start_date){
  # get bitcoin data from start_date to today
  coins =  crypto_list(only_active = TRUE)
  coin_hist = as.data.frame(crypto_history(coins,
                              limit = 1,
                              start_date = start_date, 
                              interval = "daily"))
  coin_hist = coin_hist[c("timestamp","open","close")]
  colnames(coin_hist) = c("date","price.open","price.close")
  coin_hist$date = as_date(coin_hist$date)
  return(data.table(coin_hist))
}

plot_data = function(dt){
  # plot dt --> plot bitcoin historical price
  dt %>% ggplot(aes(x = date, y = price.close)) + geom_line() + 
    theme_bw() + xlab("") + ylab("BTC close price")
}

data_enriching =function(dt,i,j,n){
  # add moving average and volatility to the data
  dt$MAi = SMA(dt[,"price.close"],i)
  dt$MAj = SMA(dt[,"price.close"],j)
  dt$VOL = volatility(dt[,"price.close"], n = n)
  return(dt)
}

get_current_infos = function(cash_account,ptf_value,dt,t){
  # get the current (at time t) informations of the data, portfolio value and cash account
  VOL_t = dt[date == t, VOL]
  MAi_t = dt[date == t, MAi]
  MAj_t = dt[date == t, MAj]
  price.open_t = dt[date == t, price.open]
  price.close_t = dt[date == t, price.close]
  cash = cash_account[length(cash_account)]
  ptf_value = ptf_value[length(ptf_value)]
  return(liste = list("t"=t,"price.open"=price.open_t,"price.close"=price.close_t,"MAi"=MAi_t,"MAj"=MAj_t,"VOL"=VOL_t,
                      "cash"=cash,"ptf_value"=ptf_value))
}

next_date = function(dates,t){
  # get the next date of the data
  return(dates[which(dates == t)+1])
}

signal_open_VOLMA = function(current,k,l){
  # return "buy", "sell" or "do nothing" for the current informations
  # based on crossing moving average and underlying volatility
  signal_position = ifelse((current$MAi > current$MAj) & (current$VOL < k | current$VOL > l), "buy",
                             ifelse((current$MAi < current$MAj) & (current$VOL >= k & current$VOL <= l), "sell",
                                    "do nothing"))
  return(signal_position)
}

open_new_position = function(current,signal_position_t,price){
  # set up a new position, for a given signal
  position_value = current$cash                                                    
  quantity = position_value/price                                            
  cash_flow = position_value*ifelse(signal_position_t == "buy",-1,+1)        
                                          
 return(list("t"=current$t, "price"=price, "VOL"=current$VOL, "MAi"=current$MAi,
             "MAj"=current$MAj, "signal_position_t"=signal_position_t,"position_value"=position_value,
             "quantity"=quantity, "cash_flow"=cash_flow))         
  
}

get_current_position = function(positions){
  # get the last position, i.e the position we are currently in 
  return(positions[[length(positions)]])
}

signal_close_VOLMA = function(current,current_position,stoploss,takeprofit){
  # decides whether or not we should close the current position. based on stoploss and takeprofit %.
  current_value = current$price.close*current_position$quantity
  possible_type_close = c("buy","sell")
  if_close_type = possible_type_close[-which(possible_type_close == current_position$signal_position_t)]
  if_close_cash_flow = current_value*ifelse(if_close_type == "buy", -1, 1)
  if_close_profit = current_position$cash_flow + if_close_cash_flow
  threshold_loss = -stoploss*abs(current_position$position_value)
  threshold_gain = takeprofit*abs(current_position$position_value)
  signal_close_position_t = ifelse((if_close_profit < threshold_loss) | (if_close_profit > threshold_gain), "close", "do nothing")
  return(signal_close_position_t)
}

close_position = function(current,current_position,price){
  # for a given closing signal, set up the closing position informations
  current_value = price*current_position$quantity                                        
  possible_type_close = c("buy","sell")                                      
  close_type = possible_type_close[-which(possible_type_close == current_position$signal_position_t)]
  close_cash_flow = current_value*ifelse(close_type == "buy", -1, 1)         
  return(liste=list("t"=current$t, "price"=price, "VOL"=current$VOL,
                    "MAi"=current$MAi,"MAj"=current$MAj,"signal_position_t"=close_type,    
                      "position_value"=current_value, "quantity"=current_position$quantity,
                    "cash_flow"=close_cash_flow))  
}

format_positions = function(positions){
  # format the dataframe of the positions set up during the diffusion of the strategy
  # since positions is a list of list
  position_history = as.data.frame(do.call(rbind, positions))
  colnames(position_history) = c("date","price",'VOL',"MAi","MAj","type",
                                 "position_value","quantity","cash_flow")
  position_history$date = as_date(unlist(position_history$date))
  position_history$price = as.numeric(unlist(position_history$price))
  position_history$VOL = as.numeric(unlist(position_history$VOL))
  position_history$MAi = as.numeric(unlist(position_history$MAi))
  position_history$MAj = as.numeric(unlist(position_history$MAj))
  position_history$type = as.character(unlist(position_history$type))
  position_history$position_value = as.numeric(unlist(position_history$position_value))
  position_history$quantity = as.numeric(unlist(position_history$quantity))
  position_history$cash_flow = as.numeric(unlist(position_history$cash_flow))
  return(data.table(position_history))
}

performance_strategy = function(position_history, ptf_value, cash_init){
  # compute all the metrics of the positions we took, knowing the 
  # evolution of the portfolio value.
  
  # first we compute the Profit and Loss dataframe : 
  if(nrow(position_history)%%2==1){
    position_history = position_history[-nrow(position_history),]
  }
  date_open = rep(0,nrow(position_history)/2)
  date_close = rep(0,nrow(position_history)/2)
  profit = rep(0,nrow(position_history)/2)
  price_open = rep(0,nrow(position_history)/2)
  price_close = rep(0,nrow(position_history)/2)
  VOL_open = rep(0,nrow(position_history)/2)
  VOL_close = rep(0,nrow(position_history)/2)
  quantity = rep(0,nrow(position_history)/2)
  type_open = rep(0,nrow(position_history)/2)
  type_close = rep(0,nrow(position_history)/2)
  j = 1
  for(i in seq(1,nrow(position_history)-1,2)){
    date_open[j] = position_history$date[i]
    date_close[j] = position_history$date[i+1]
    profit[j] = position_history$cash_flow[i] + position_history$cash_flow[i+1] 
    price_open[j] = position_history$price[i]
    price_close[j] = position_history$price[i+1]
    VOL_open[j] = position_history$VOL[i]
    VOL_close[j] = position_history$VOL[i+1]
    quantity[j] = position_history$quantity[i]
    type_open[j] = position_history$type[i]
    type_close[j] = position_history$type[i+1]
    j = j + 1
  }
  PL = data.frame("date_open" = date_open, "date_close" = date_close, "price_open" = price_open,
                  "price_close" = price_close, "quantity" = quantity, "type_open" = type_open,
                  "type_close" = type_close,"VOL_open" = VOL_open, "VOL_close" = VOL_close, 
                  "profit" = profit)
  PL$cumulated_profit = cumsum(PL$profit)
  PL$cash = PL$cumulated_profit + cash_init
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
  
  daily_return = diff(ptf_value$value)/ptf_value$value[1:(nrow(ptf_value)-1)]
  volatility = sqrt(365)*sd(daily_return)
  mean_return = 365*mean(daily_return)
  sharpe_ratio = round(mean_return/volatility,4)
  max_drawdown = round(maxDrawdown(xts(x = daily_return,order.by = ptf_value$date[-1])),4)
  skew = round(skewness(daily_return),4)
  kurtosis = round(kurtosis(daily_return),4)
  
  ending_cash = cash_init + end_profit
  return = round((ending_cash/cash_init) - 1,4)
  period = as_date(ptf_value$date[length(ptf_value$date)]) - as_date(ptf_value$date[1])
  return_y = round((1+return)^(365/as.numeric(period))-1,4)
  
  # Value at Risk, Conditional Tail Expectation, Expected Shortfall
  VaR = quantile(daily_return, probs = c(0.01, 0.05))
  VaR_99 = round(VaR[1],2)
  VaR_95 = round(VaR[2],2)
  CTE_95 = round(mean(PL$profit[PL$profit <= VaR_95]),2)
  ES_95 = round(mean(ifelse(PL$profit < VaR_95, 1,0)*PL$profit),2)    # ES Stop loss premium with a retention equal to the value at risk
  
  return(liste = list("PL" = data.table(PL), "ptf_value" = ptf_value,
                      "end_profit" = end_profit, "ending_cash" = ending_cash,
                      "return" = return, "period" = period,
                      "return_y" = return_y,
                      "sharpe_ratio" = sharpe_ratio,
                      "max_drawdown" = max_drawdown,
                      "mean_return" = round(mean_return,4),
                      "volatility" = round(volatility,4),
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

strategy_graph = function(diffusion,diffusion_perf,strategy_name){
  # plot all the information of a strategy diffusion
  
  theme_Bloomberg <-   theme(plot.background=element_rect(fill="#000000"),
                             panel.background=element_rect(fill="grey75"),
                             panel.grid.minor=element_line(linetype=3,size=0.1),
                             panel.grid.major=element_line(linetype=3,size=0.4),
                             panel.border = element_rect(colour = "white", fill=NA, size=0.5),
                             plot.title = element_text(colour = "white",size=15,face="bold"),
                             axis.text=element_text(size=15, colour="white"),
                             axis.title=element_text(size=15,face="bold",colour="white"),
                             legend.text=element_text(size=10))
  start = diffusion$start
  end = diffusion$end
  i = diffusion$i
  j = diffusion$j
  k = diffusion$k
  l = diffusion$l
  stoploss = diffusion$stoploss
  takeprofit = diffusion$takeprofit
    
  PL = diffusion_perf$PL
  PV = diffusion$ptf_value[-1,]
  
  p_profit_hist = ggplot(PL, aes(x=profit)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 50) +
    geom_density(alpha=.2, fill="#FF6666") +
    geom_vline(aes(xintercept=mean(profit)),
               color="blue", linetype="dashed", size=1) +
    theme_Bloomberg
  
  p_profit = ggplot() +
    geom_step(data = PL, aes(x = date_close, y = cash), color = "darkblue") + 
    geom_line(data = PV, aes(x = date, y = value), color = "firebrick", linetype = "dashed") +
    xlab("Date") +
    ylab("Ptf value over time") +
    theme_Bloomberg
  
  infos_strat = paste(paste("TRADING FROM", start, "TO", end),
                      paste(strategy_name,"TRADING STRATEGY"),
                      paste("begining cash :", diffusion_perf$ptf_value$value[1]),
                      paste("ending cash :", diffusion_perf$ending_cash),
                      paste("end profit : ", diffusion_perf$end_profit), 
                      paste("return : ", diffusion_perf$return*100,"%"),
                      paste("period : ", diffusion_perf$period, "days"),
                      paste("annualized return : ", diffusion_perf$return_y*100,"%"),
                      paste("sharpe ratio : ", diffusion_perf$sharpe_ratio),
                      paste("max drawdown : ", diffusion_perf$max_drawdown),
                      paste("mean return : ", round(diffusion_perf$mean_return,4)),
                      paste("volatility : ", round(diffusion_perf$volatility,4)),
                      paste("mean position length : ", round(diffusion_perf$mean_position_length,2), "days"),
                      paste("percentage good position : ", diffusion_perf$percentage_good*100, "%"),
                      paste("gain/pain ratio : ", diffusion_perf$gain_pain_ratio),
                      paste("standard deviation of profits : ", diffusion_perf$sd_profit),
                      paste("mean profit : ", diffusion_perf$mu_profit),
                      paste("skewness : ", diffusion_perf$skew),
                      paste("kurtosis : ", diffusion_perf$kurtosis),
                      paste("VaR99% : ", diffusion_perf$VaR_99),
                      paste("VaR95% : ", diffusion_perf$VaR_95),
                      paste("CTE95% : ", diffusion_perf$CTE_95),
                      paste("Expected Shortfall 95% : ", diffusion_perf$ES_95),
                      sep="\n")
  p_info_strat = ggplot() +                     
    annotate("text",
             x = 1,
             y = 1,
             size = 3,
             label = infos_strat,
             color = "darkblue") + 
    theme_void()
  
  
  
  title = paste(strategy_name)
  p = grid.arrange(p_profit, p_info_strat,  p_profit_hist, ncol= 2, nrow=2, heights=c(4, 2),
                   widths = c(5,2),top = textGrob(title, gp=gpar(fontsize=10,font=3)))
  return(list("infos"=infos_strat))
}


#------------------------------------------------------------------------------#
# FUNCTIONS (strategy diffusion)

VOL_MA_strategy = function(start,end,cash_init,i,j,k,l,stoploss,takeprofit,dt_full){
  # diffusion of the strategy based on moving average and volatility
  
  dt = subset(dt_full, date >= start & date <= end)
  
  nb_position = 0                    
  positions = c()                        
  ptf_value = c(cash_init)            
  ptf_value_dates = c(start-1)
  cash_account = c(cash_init)
  open_position = FALSE 
  dates = dt[,date]
  t = start
  
  while(t < (end-1)){
    
    current = get_current_infos(cash_account,ptf_value,dt,t)
    
    if(open_position == FALSE){
      
      signal_position_t = signal_open_VOLMA(current,k,l)
      
      if(signal_position_t == "do nothing"){
        ptf_value = c(ptf_value, current$ptf_value)
        ptf_value_dates = c(ptf_value_dates, t)
        cash_account = c(cash_account, current$cash)
        t = next_date(dates,t) 
        next
      }
      
      if(signal_position_t != "do nothing"){
        price = current$price.close
        new_position = open_new_position(current,signal_position_t,price)
        nb_position = nb_position + 1 
        positions[[nb_position]] = new_position     
        ptf_value = c(ptf_value, current$ptf_value) 
        ptf_value_dates = c(ptf_value_dates, t)
        cash_account = c(cash_account, current$cash + new_position$cash_flow)
        t = next_date(dates,t)    
        open_position = TRUE
        next
      }
    }
    
    if(open_position == TRUE){
      
      current_position = get_current_position(positions)
      signal_close_position_t = signal_close_VOLMA(current,current_position,stoploss,takeprofit) 
      ptf_value = c(ptf_value, current$price.close*current_position$quantity) 
      ptf_value_dates = c(ptf_value_dates, t)
      
      if(signal_close_position_t == "do nothing"){
        cash_account = c(cash_account, current$cash)
        t = next_date(dates,t) 
        next
      }
      
      if(signal_close_position_t == "close"){
        price = current$price.close
        new_position = close_position(current,current_position,price) 
        nb_position = nb_position + 1 
        positions[[nb_position]] = new_position
        cash_account = c(cash_account, current$cash + new_position$cash_flow)
        t = next_date(dates,t) 
        open_position = FALSE 
        next
      }
    }
  }
  position_history = format_positions(positions)
  ptf_value = data.table("date" = ptf_value_dates, "value" = ptf_value)
  cash_account = data.table("date" = ptf_value_dates, "cash" = cash_account)
  
  return(liste=list("start"=start,"end"=end,"cash_init"=cash_init,
                    "i"=i,"j"=j,"k"=k,"l"=l,"sotploss"=stoploss,
                    "takeprofit"=takeprofit,"dt_full"=dt_full,"dt"=dt,
                    "position_history"=position_history,"ptf_value"=ptf_value,
                    "cash_account"=cash_account))
}

BUYCLOSE_SELLOPEN_strategy = function(start,end,cash_init,dt_full){
  # diffusion of the strategy based on buying the closing and selling the opening
  
  dt = subset(dt_full, date >= start & date <= end)
  
  nb_position = 0                    
  positions = c()                        
  ptf_value = c(cash_init)            
  ptf_value_dates = c(start-1)
  cash_account = c(cash_init)
  open_position = FALSE 
  dates = dt[,date]
  t = start
  
  while(t < (end-1)){
    
    current = get_current_infos(cash_account,ptf_value,dt,t)
    
    if(open_position == FALSE){
      # buying the closing day
      price = current$price.close
      new_position = open_new_position(current,"buy",price)
      nb_position = nb_position + 1 
      positions[[nb_position]] = new_position     
      ptf_value = c(ptf_value, current$ptf_value) 
      ptf_value_dates = c(ptf_value_dates, t)
      cash_account = c(cash_account, current$cash + new_position$cash_flow)
      t = next_date(dates,t)    
      open_position = TRUE
      next
    }

    if(open_position == TRUE){
      # selling the opening day : closing the position took the day before
      price = current$price.open
      current_position = get_current_position(positions)
      new_position = close_position(current,current_position,price)
      nb_position = nb_position + 1 
      positions[[nb_position]] = new_position
      ptf_value = c(ptf_value, price*new_position$quantity) 
      ptf_value_dates = c(ptf_value_dates, t)
      cash_account = c(cash_account, new_position$cash_flow)
      #open_position = FALSE # useless because we are going to open a position at the end of the day
      
      # buying the closing day
      current = get_current_infos(cash_account,ptf_value,dt,t)
      price = current$price.close
      new_position = open_new_position(current,"buy",price)
      nb_position = nb_position + 1 
      positions[[nb_position]] = new_position     
      ptf_value = c(ptf_value, current$ptf_value) 
      ptf_value_dates = c(ptf_value_dates, t)
      cash_account = c(cash_account, current$cash + new_position$cash_flow)
      
      # loop
      t = next_date(dates,t) 
      open_position = TRUE #we have an open position overnight 
      next
    }
  }
  
  position_history = format_positions(positions)
  ptf_value = data.table("date" = ptf_value_dates, "value" = ptf_value)
  cash_account = data.table("date" = ptf_value_dates, "cash" = cash_account)
  
  return(liste=list("start"=start,"end"=end,"cash_init"=cash_init,
                    "dt_full"=dt_full,"dt"=dt, "position_history"=position_history,
                    "ptf_value"=ptf_value,"cash_account"=cash_account))
}


#------------------------------------------------------------------------------#
# FUNCTIONS (mother : gather all the above functions)

VOL_MA_TRADING = function(start,end,cash_init,i,j,k,l,stoploss,takeprofit,dt_full){
  # synthetic function that compute the moving average/volatility based strategy
  
  # diffusion of the strategy
  diffusion = VOL_MA_strategy(start,end,cash_init,i,j,k,l,stoploss,takeprofit,dt_full)
  # computation of the statistics of the diffusion
  performance = performance_strategy(diffusion$position_history, diffusion$ptf_value,cash_init)
  # display graph of the diffusion
  graph = strategy_graph(diffusion,performance,"MA/VOL")
  
  return(liste = list("diffusion" = diffusion, "performance" = performance, "infos" = graph$infos))
}

BUYCLOSE_SELLOPEN_TRADING = function(start,end,cash_init,dt_full){
  # synthetic function that compute the buy close/sell open strategy
  diffusion = BUYCLOSE_SELLOPEN_strategy(start,end,cash_init,dt_full)
  performance = performance_strategy(diffusion$position_history,diffusion$ptf_value,cash_init)
  graph = strategy_graph(diffusion,performance,"BUY:CLOSE-SELL:OPEN")
  
  return(liste = list("diffusion" = diffusion, "performance" = performance, "infos" = graph$infos))
}

HOLD_TRADING = function(start,end,cash_init,dt_full){
  # compute all the information and graph of the buy and hold strategy
  
  dt = subset(dt_full, date >= start & date <= end)
  
  ptf_value = c(cash_init)            
  ptf_value_dates = c(start-1)
  cash_account = c(cash_init)
  dates = dt[,date]
  t = start
  
  # buy at the begining : 
  price.begining.hold = dt[date == t, price.close]
  quantity.hold = cash_init/price.begining.hold
  ptf_value = c(ptf_value,quantity.hold*price.begining.hold)            
  ptf_value_dates = c(ptf_value_dates,t)
  cash_account = c(cash_account,cash_init-quantity.hold*price.begining.hold)
 
   # diffusion
  while(t < (end-1)){
    current = get_current_infos(cash_account,ptf_value,dt,t)
    price = current$price.close
    ptf_value = c(ptf_value,quantity.hold*price) 
    ptf_value_dates = c(ptf_value_dates,t)
    cash_account = c(cash_account,current$cash)
    t = next_date(dates,t) 
  }
  ptf_value = data.table("date" = ptf_value_dates, "value" = ptf_value)
  ptf_value = ptf_value[-c(1:2),]
  cash_account = data.table("date" = ptf_value_dates, "cash" = cash_account)
  PL = ptf_value
  colnames(PL) = c("date_close","cash")
  
  # performance
  end_profit = round(ptf_value$value[length(ptf_value$value)]-ptf_value$value[1],2)
  daily_return = diff(ptf_value$value)/ptf_value$value[1:(nrow(ptf_value)-1)]
  volatility = sqrt(365)*sd(daily_return)
  mean_return = 365*mean(daily_return)
  sharpe_ratio = round(mean_return/volatility,4)
  max_drawdown = round(maxDrawdown(xts(x = daily_return,order.by = ptf_value$date[-1])),4)
  ending_cash = cash_init + end_profit
  return = round((ending_cash/cash_init) - 1,4)
  period = as_date(ptf_value$date[length(ptf_value$date)]) - as_date(ptf_value$date[1])
  return_y = round((1+return)^(365/as.numeric(period))-1,4)
  profit = diff(ptf_value$value)
  VaR = quantile(profit, probs = c(0.01, 0.05))
  VaR_99 = round(VaR[1],2)
  VaR_95 = round(VaR[2],2)
  CTE_95 = round(mean(profit[profit <= VaR_95]),2)
  ES_95 = round(mean(ifelse(profit < VaR_95, 1,0)*profit),2)    # ES Stop loss premium with a retention equal to the value at risk
  percentage_good = round(length(profit[profit > 0])/length(profit),4)
  gain_pain_ratio = round(sum(profit[profit > 0])/sum(abs(profit[profit < 0])),4)
  sd_gain = round(sd(profit),2)
  mean_gain = round(mean(profit),2)
  skew = round(skewness(profit),4)
  kurtosis = round(kurtosis(profit),4)
  
  # plot part 
  theme_Bloomberg <-   theme(plot.background=element_rect(fill="#000000"),
                             panel.background=element_rect(fill="grey75"),
                             panel.grid.minor=element_line(linetype=3,size=0.1),
                             panel.grid.major=element_line(linetype=3,size=0.4),
                             panel.border = element_rect(colour = "white", fill=NA, size=0.5),
                             plot.title = element_text(colour = "white",size=15,face="bold"),
                             axis.text=element_text(size=15, colour="white"),
                             axis.title=element_text(size=15,face="bold",colour="white"),
                             legend.text=element_text(size=10))
  
  p_hold = ggplot() +
    geom_line(data = ptf_value, aes(x = date, y =value), color = "darkgreen") +
    xlab("Date") +
    ylab("Hold over time") +
    theme_Bloomberg
  
  infos_hold = paste(paste("TRADING FROM", start, "TO", end),
                     "HOLD STRATEGY",
                     paste("begining cash :", cash_init),
                     paste("ending cash :", ending_cash),
                     paste("end profit : ", end_profit), 
                     paste("return : ",return*100,"%"),
                     paste("period : ", as.numeric(period) , "days"),
                     paste("annualized return : ", return_y*100,"%"),
                     paste("sharpe ratio : ", sharpe_ratio),
                     paste("max drawdown : ", max_drawdown),
                     paste("mean return : ", round(mean_return,4)),
                     paste("volatility : ", round(volatility,4)),
                     paste("percentage good position : ", percentage_good*100,"%"),
                     paste("gain/pain ratio : ", gain_pain_ratio),
                     paste("standard deviation of profits : ", sd_gain),
                     paste("mean profit : ", mean_gain),
                     paste("skewness : ", skew),
                     paste("kurtosis : ", kurtosis),
                     paste("VaR99% : ", VaR_99),
                     paste("VaR95% : ", VaR_95),
                     paste("CTE95% : ", CTE_95),
                     paste("Expected Shortfall 95% : ", ES_95),
                     sep="\n")
  
  p_infos_hold = ggplot() +                     
    annotate("text",
             x = 1,
             y = 1,
             size = 3,
             label = infos_hold,
             color = "darkgreen") + 
    theme_void()
  
  title = paste("HOLD STRATEGY")
  p = grid.arrange(p_hold, p_infos_hold, ncol= 2, nrow=1,
                   widths = c(5,2),top = textGrob(title, gp=gpar(fontsize=10,font=3)))
 
   performance = list("cash_init"=cash_init,"ending_cash"=ending_cash,"end_profit"=end_profit,
                     "return"=return,"period"=period,"return_y"=return_y,
                     "sharpe_ratio"=sharpe_ratio, "max_drawdown" = max_drawdown,
                     "mean_return"=mean_return, "volatility"=volatility, "percentage_good" = percentage_good,
                     "gain_pain_ratio" = gain_pain_ratio, "sd_profit" = sd_gain, "mu_profit" = mean_gain,
                     "skew" = skew, "kurtosis" = kurtosis, "VaR_99" = VaR_99, "VaR_95" = VaR_95,
                     "CTE_95" = CTE_95, "ES_95" = ES_95)

  return(liste=list("ptf_value"=ptf_value,"PL"=PL,"performance"=performance,"infos"=infos_hold))
}

COMPARE_STRAT = function(strats){
  # plot the graph with all the strategy
  
  no_strats = length(strats)
  theme_Bloomberg <-   theme(plot.background=element_rect(fill="#000000"),
                             panel.background=element_rect(fill="grey75"),
                             panel.grid.minor=element_line(linetype=3,size=0.1),
                             panel.grid.major=element_line(linetype=3,size=0.4),
                             panel.border = element_rect(colour = "white", fill=NA, size=0.5),
                             plot.title = element_text(colour = "white",size=15,face="bold"),
                             axis.text=element_text(size=15, colour="white"),
                             axis.title=element_text(size=15,face="bold",colour="white"),
                             legend.text=element_text(size=10))
  infos = c()
  p_ptf = ggplot() + 
    xlab("Date") +
    ylab("Ptf value over time") +
    theme_Bloomberg
  plist = list()
  for(i in 1:no_strats){
    infos =strats[[i]][[2]]
    p_infos = ggplot() +                     
      annotate("text",x = 1,y = 1,size = 3,label = infos,color = "black") + 
      theme_void()
    #plist = append(plist,p_infos)
    plist[[i]] = p_infos
    p_ptf = p_ptf + geom_line(data = strats[[i]][[1]]$ptf_value, aes(x = date, y =value), color = "firebrick",linetype="dashed")+
      geom_step(data = strats[[i]][[1]]$PL, aes(x = date_close, y =cash), color = i)
  }
  p_ptf = p_ptf + scale_colour_manual("", 
                      breaks = c("MA/VOL", "BUYCLOSE/SELLOPEN", "BUY & HOLD"),
                      values = c("black", "red", "green"))
  plist[[no_strats+1]] = p_ptf
  n = length(plist)
  cowplot::plot_grid(plotlist = plist, ncol = n, rel_widths = c(rep(2,(n-1)),5) )
}

#------------------------------------------------------------------------------#
# RUN mother functions
# don't hesitate to expand pans to better see strategies diffusion and statistics

# get data BTC : 
dt_full = get_data("20131101") # run it once : take one minute

# plot BTC price : 
plot_data(dt_full)

# parameters of the strategies :
cash_init = 100          # begining cash we are investing
i = 5                    # moving average i
j = 40                   # moving average j
k = 40                   # lower bound of the volatility we are looking at
l = 90                   # upper bound of the volatility we are looking at
n = 30                   # 1 month volatility computation
stoploss = 0.1           # stoploss = -10% * opening position value
takeprofit = 0.01        # takeprofit = 3% * opening position value
dt_full = data_enriching(dt_full,i,j,n)

##############################
# TRAIN SET : train on 1500 days

# dates : 
start = today()-3000   # -3000 days to -1500 days in the past
end = today() - 1500

# diffusion :
# for each line you can see the diffusion of each strategy implemented : 
volma_train = VOL_MA_TRADING(start,end,cash_init,i,j,k,l,stoploss,takeprofit,dt_full)
buysell_train = BUYCLOSE_SELLOPEN_TRADING(start,end,cash_init,dt_full)
hold_train = HOLD_TRADING(start,end,cash_init,dt_full)

# synthetic graph gathering all the informations : 
strats_train = list(list(volma_train$performance[c("PL","ptf_value")],volma_train$infos),
                    list(buysell_train$performance[c("PL","ptf_value")],buysell_train$infos),
                    list(hold_train[c("PL","ptf_value")],hold_train$infos))
COMPARE_STRAT(strats_train)

# green = hold, black = vol/ma strategy, red = buy closing and sell opening.
# the red line near the black one is the evolution of the vol/ma strategy portfolio 
# overall, indeed holding the bitcoin seems to optimize the profit because its 
# directly linked to the fact that bitcoin "stonks"
# However, the MA/VOL strategy present better volatility, better maxdrawdown,
# and a pretty good return
# which lead this strategy to a sharpe ratio of 0.989. Furthermore, for lower volatility 
# (2014-2017) the MA/VOL strategy and the buy closing/sell opening strategy 
# outperform the buy and hold strategy


##############################
# TEST SET : test on 1500 days

# dates : 
start = today()-1500  # -1500 days to -1 days in the past
end = today() - 1

# diffusion
volma_test = VOL_MA_TRADING(start,end,cash_init,i,j,k,l,stoploss,takeprofit,dt_full)
buysell_test = BUYCLOSE_SELLOPEN_TRADING(start,end,cash_init,dt_full)
hold_test = HOLD_TRADING(start,end,cash_init,dt_full)

# synthetic graph
strats_test = list(list(volma_test$performance[c("PL","ptf_value")],volma_test$infos),
                   list(buysell_test$performance[c("PL","ptf_value")],buysell_test$infos),
                   list(hold_test[c("PL","ptf_value")],hold_test$infos))
COMPARE_STRAT(strats_test)

# green = hold, black = vol/ma strategy, red = buy closing and sell opening.
# the red line near the black one is the evolution of the vol/ma strategy portfolio 
# The MA/VOL strategy slightly outperform the buy and hold strategy but it present
# a better sharpe ratio because of a lower volatility and a way better maxdrawdown

# RQ : you can access the profit and loss dataframe :
volma_test$performance$PL

# RQ : you can access the portfolio value over time :
volma_test$performance$ptf_value

# RQ : you can access the cash account over time :
volma_test$diffusion$cash_account

# RQ : you can access the position history :
volma_test$diffusion$position_history

# to go further : Rshiny, purrr for parameters value (has been implemented but took
# 1h to compute), moving average length in function of the recent volatility




# END


