####################### Pair Trading - Robin MIRALLES #########################

setwd("/Users/robin/Documents/Dauphine 272/Pair_Trading")

#### packages ####

library(BatchGetSymbols)                   # import stocks : daily 
library(tseries)                           # manipulation of time series
library(xlsx)                              # writting excel (save data frame)
library(ggplot2)                           # visualisation
library(BETS)                              # function normalize
library(lubridate)                         # date manipulation
library(data.table)                        # dataframe manipulation
library(grid)                              # dataviz
library(gridExtra)                         # several ggplot
library(birk)                              # which.closest for dates
library(plot3D)                            # plot 3D graphs
library(gtools)                            # combinations
`%!in%` = function(x,y)!('%in%'(x,y))

################################################################################
###################### algorithm : Pair trading ################################
################################################################################

# at time t : 
#   if signal > upper limit u --> A is overvalued with respect to B 
#                             --> short A and long B
#                             --> quantity : -1/vol(R_1A(t)) for A and 1/vol(R_1B(t)) for B

#   if signal < lower limit d --> A is undervalued with respect to B 
#                             --> long A and short B
#                             --> quantity : 1/vol(R_1A(t)) for A and -1/vol(R_1B(t)) for B

# Calculation on normalized price
# algorithm : 
# if signal within [d,u] do nothing
# else if signal outside [d,u] : take a position described above
#             then while cointegration = TRUE and signal doesn't change it sign do nothing
#             if the signal change it sign then sell the position

################################################################################
############################### FUNCTIONS ######################################
################################################################################

import_SandP500 = function(lookback_window_years){
  lookback_window = lookback_window_years*365                
  first_date = Sys.Date()- 1 - lookback_window
  last_date = Sys.Date() - 1
  freq_data = 'daily'
  
  df_SP500 = GetSP500Stocks()
  tickers = df_SP500$Tickers
  
  df = BatchGetSymbols(tickers = tickers,
                       first.date = first_date,
                       last.date = last_date,
                       freq.data = freq_data,)
  
  df = df$df.tickers
  first_date = df$ref.date[1]
  last_date = df$ref.date[length(df$ref.date)]
  return(liste = list("df" = df, "first_date" = first_date, "last_date" = last_date))
}

pairs_identification = function(start, end, df){
  df = as.data.table(df)
  df = df[ref.date < end & ref.date >= start]
  cat("##################################################","\n",
      "first date cointegration calculus  : ", as.character(start),
      "\n last date cointegration calculus : ", as.character(end),
      "\n ##################################################")
  vec_p = c()
  tickers = unique(df$ticker)
  couples = combinations(n = length(tickers), r = 2, v = tickers, repeats.allowed = FALSE)
  for(i in 1:nrow(couples)){
    print(i)
    dfi = df[ticker == couples[i,1]]
    dfj = df[ticker == couples[i,2]]
    dftest = cbind(dfi$price.close, dfj$price.close)
    p = as.numeric(po.test(dftest)$p.value) # co-integration test of Phillips   
    vec_p = c(vec_p, p)
  }
  couples = cbind(couples, vec_p)
  colnames(couples) = c("A", "B", "pvalue")
  couples = as.data.frame(couples)
  couples$pvalue = as.numeric(couples$pvalue)
  couples = couples[couples$pvalue <= 0.01,]
  
  return(liste=list("cointegrated_stocks" = couples))
}

stocks_retained_visualization = function(A, B, df){
  
  stockA = df[df$ticker == A, c("ticker","price.close","ref.date")]
  colnames(stockA)[2] = "price.close.A"
  stockB = df[df$ticker == B, c("ticker","price.close","ref.date")]
  colnames(stockB)[2] = "price.close.B"
  stocks = cbind(stockA, stockB)
  stocks = stocks[,c("ref.date", "price.close.A", "price.close.B")]
  # visualisation of the stocks : 
  sA = df[df$ticker == A, c("ticker","price.close","ref.date")] 
  sB = df[df$ticker == B, c("ticker","price.close","ref.date")]
  s = rbind(sA, sB) 
  p1 = ggplot(s, aes(x=ref.date, y=price.close, group = ticker, colour = ticker )) +
    geom_line() + 
    xlab("date") +
    ylab("closing price")
  
  # visualisation of the normalized stocks : 
  sA$price.close.norm = BETS::normalize(sA$price.close, mode = "scale")
  sB$price.close.norm = BETS::normalize(sB$price.close, mode = "scale")
  s_norm = rbind(sA, sB) 
  p2 = ggplot(s_norm, aes(x=ref.date, y=price.close.norm, group = ticker, colour = ticker )) +
    geom_line() + 
    xlab("date") +
    ylab("normalized closing price")
  
  grid.arrange(p1, p2, ncol= 2, nrow=1)
  
}

pairs_data_formatting = function(A, B, df){
  sA = df[df$ticker == A, c("ticker","price.close","ref.date")] 
  sB = df[df$ticker == B, c("ticker","price.close","ref.date")]
  sA$price.close.norm = BETS::normalize(sA$price.close, mode = "scale")
  sB$price.close.norm = BETS::normalize(sB$price.close, mode = "scale")
  s_norm = rbind(sA, sB) 
  return(s_norm)
}

signal = function(data,t,j,A,B){
  # signal(t,j)_AB = R_jA(t)/vol(R_1A(t)) - R_jB(t)/vol(R_1B(t))
  # R_jX(t) = P_X(t)/P_X(t-j) - 1
  # vol(R_1X(t)) = standard deviation at time t of the previous month daily return of the stock X 
  # voir memoire HEC
  
  df = data[data$ref.date <= t,]
  dfA = df[df$ticker == A,]
  dfB = df[df$ticker == B,]
  dfA$return = c(NA,diff(dfA$price.close)/dfA$price.close[1:(length(dfA$price.close)-1)])
  dfB$return = c(NA,diff(dfB$price.close)/dfB$price.close[1:(length(dfB$price.close)-1)])
  
  one_month_before = dfA$ref.date[dfA$ref.date <= t - 30][length(dfA$ref.date[dfA$ref.date <= t - 30])]
  
  R_jA = (dfA$price.close[dfA$ref.date == t]/dfA$price.close[which(dfA$ref.date == t)-j]) - 1
  vol_R1A = sd(dfA$return[dfA$ref.date < t & dfA$ref.date >= one_month_before])
  R_jB = (dfB$price.close[dfB$ref.date == t]/dfB$price.close[which(dfB$ref.date == t)-j]) - 1
  vol_R1B = sd(dfB$return[dfB$ref.date < t & dfB$ref.date >= one_month_before])
  
  s = R_jA/vol_R1A - R_jB/vol_R1B
  return(liste = list("signal"=s,"Rja"=R_jA,"Rjb"=R_jB,"vol_R1A"=vol_R1A,"vol_R1B"=vol_R1B))
}

pair_trading = function(data, A, B, start_date_trading, end_date_trading, limit, j, stop_loss, transaction_cost, is_test_cointegration){
  dates = unique(data$ref.date)
  d = -limit                                      # lower limit of signal : parameter
  u = limit                                       # upper limit of signal : parameter
  t = start_date_trading
  test_cointegration = TRUE                       # boolean : TRUE --> A and B are cointegrated
  open_position = FALSE                           # boolean : TRUE --> a position has been taken
  signal_history = c()                            # signal history 
  signal_date_history = c()
  position_history = c()                          # position taken history
  
  if(is_test_cointegration == TRUE){
    dfi = data[data$ticker == A & data$ref.date <= t ,]
    dfj = data[data$ticker == B & data$ref.date <= t ,]
    dftest = cbind(dfi$price.close, dfj$price.close)
    test_cointegration = ifelse(as.numeric(po.test(dftest)$p.value) <= 0.05, TRUE, FALSE)
  }
  
  if(test_cointegration == FALSE){ 
    print(paste("STOP TRADING at : ", t , " --> stocks no more cointegrated"))
  }
  
  while(t < end_date_trading & !is.na(t) & test_cointegration == TRUE){
    
    signal_t = signal(data,t,j,A,B)
    signal_history = c(signal_history,signal_t$signal)
    signal_date_history = c(signal_date_history, as.character(t))
    priceA = data$price.close[data$ticker == A & data$ref.date == t]
    priceB = data$price.close[data$ticker == B & data$ref.date == t]
    quantityA = 1/signal_t$vol_R1A
    quantityB = 1/signal_t$vol_R1B
    
    if(open_position == FALSE){
      
      if(signal_t$signal > u){
        # A overvalued, B undervalued --> short A (-) and long B (+)
        open_position = TRUE
        new_position = list("date" = as.character(t), "position" = "open",
                            "quantityA" = -quantityA,
                            "quantityB" =  quantityB,
                            "valueA" = -quantityA*priceA,
                            "valueB" = quantityB*priceB,
                            "transaction_cost" = (transaction_cost/10000)*(abs(quantityA*priceA)+abs(quantityB*priceB)))
        position_history[[length(position_history)+1]] = new_position
      }
      
      if(signal_t$signal < d){
        # A undervalued, B overvalued --> long A (+) and short B (-)
        open_position = TRUE
        new_position = list("date" = as.character(t), "position" = "open",
                            "quantityA" = quantityA,
                            "quantityB" = -quantityB,
                            "valueA" = quantityA*priceA,
                            "valueB" = -quantityB*priceB,
                            "transaction_cost" = (transaction_cost/10000)*(abs(quantityA*priceA)+abs(quantityB*priceB)))
        position_history[[length(position_history)+1]] = new_position
      }
      t = dates[which(dates == t) + 1]
      next
    }
    
    
    # if a position is open and the signal modifies its sign --> close the position : 
    sign_signal_now = sign(signal_history[length(signal_history)])
    sign_signal_before = sign(signal_history[length(signal_history)-1])
    
    if(open_position == TRUE & sign_signal_now != sign_signal_before){
      open_position = FALSE
      quantityA_before = position_history[[length(position_history)]]$quantityA
      quantityB_before = position_history[[length(position_history)]]$quantityB
      new_position = list("date" = as.character(t), "position" = "close",
                          "quantityA" = -quantityA_before,
                          "quantityB" = -quantityB_before,
                          "valueA" = -quantityA_before * priceA,
                          "valueB" = -quantityB_before * priceB,
                          "transaction_cost" = (transaction_cost/10000)*(abs(quantityA_before * priceA)+abs(quantityB_before * priceB)))
      position_history[[length(position_history)+1]] = new_position
      t = dates[which(dates == t) + 1]
      next
    }
    
    # if a position is open, its signal is of the same sign, but the loss exceed the stop loss --> close the position :
    if(open_position == TRUE & sign_signal_now == sign_signal_before){
      # tricky short/long =  -/+ in position, versus short/long  = +/- in cash flow
      quantityA_before = position_history[[length(position_history)]]$quantityA
      quantityB_before = position_history[[length(position_history)]]$quantityB
      valueA_before = position_history[[length(position_history)]]$valueA
      valueB_before = position_history[[length(position_history)]]$valueB
      value_when_open_position = -valueA_before - valueB_before
      current_value = sign(valueA_before)*abs(quantityA_before)*priceA + sign(valueB_before)*abs(quantityB_before)*priceB
      current_result_if_close_now = value_when_open_position + current_value
      
      if(current_result_if_close_now <= -stop_loss){
        open_position = FALSE
        new_position = list("date" = as.character(t), "position" = "close",
                            "quantityA" = -quantityA_before,
                            "quantityB" = -quantityB_before,
                            "valueA" = -quantityA_before*priceA,
                            "valueB" = -quantityB_before*priceB,
                            "transaction_cost" = (transaction_cost/10000)*(abs(quantityA_before*priceA)+abs(quantityB_before*priceB)))
        position_history[[length(position_history)+1]] = new_position
        t = dates[which(dates == t) + 1]
        next
      }
    }
    
    # if no position is taken, and if a position is open but no change of sign coming from the signal
    # and no current loss exceeding the stop loss : 
    t = dates[which(dates == t) + 1] 
  }
  
  
  position_history = as.data.frame(do.call(rbind, position_history))
  position_history$date = as.Date(as.character(position_history$date))
  position_history$position = as.character(position_history$position)
  position_history$quantityA = as.numeric(position_history$quantityA)
  position_history$quantityB = as.numeric(position_history$quantityB)
  position_history$valueA = as.numeric(position_history$valueA)
  position_history$valueB = as.numeric(position_history$valueB)
  position_history$transaction_cost = as.numeric(position_history$transaction_cost)
  
  signal = as.data.frame(cbind(signal_date_history,signal_history))
  colnames(signal) = c("date","signal_value")
  signal$date = as.Date(signal$date)
  signal$signal_value = as.numeric(signal$signal_value)
  
  return(list("position_history" = position_history, "signal" = signal))
}

P_and_L = function(data, A, B, position_history, initial_capital){
  if((nrow(position_history) %% 2) == 1){
    position_history = position_history[-nrow(position_history),]
  }
  date_open = rep(0,nrow(position_history)/2)
  date_close = rep(0,nrow(position_history)/2)
  result = rep(0,nrow(position_history)/2)
  profit = rep(0,nrow(position_history)/2)
  j = 1
  for(i in seq(1,nrow(position_history)-1,2)){
    date_open[j] = as.character(position_history$date[i])
    date_close[j] = as.character(position_history$date[i+1])
    result[j] = -position_history$valueA[[i]] -position_history$valueA[i+1] -position_history$valueB[i] -position_history$valueB[i+1]
    profit[j] = result[j] - position_history$transaction_cost[i] - position_history$transaction_cost[i+1]
    j = j + 1
  }
  PL = data.frame("date_open" = date_open, "date_close" = date_close, "result" = result, "profit" = profit)
  PL$cumulated_profit = cumsum(PL$profit)
  PL$cash_account = PL$cumulated_profit + initial_capital
  
  PL$result = as.numeric(PL$result)
  PL$profit = as.numeric(PL$profit)
  PL$cumulated_profit = as.numeric(PL$cumulated_profit)
  PL$cash_account = as.numeric(PL$cash_account)
  PL$date_open = as.Date(as.character(PL$date_open))
  PL$date_close = as.Date(as.character(PL$date_close))
  
  PL$position_length = PL$date_close - PL$date_open
  return(PL)
}

plot_pair_trading = function(data, A, B, PL, signal, start_date_trading, end_date_trading, title){
  
  data = data[data$ref.date <= end_date_trading & data$ref.date >= start_date_trading,]
  data$is_open_position = ifelse(data$ref.date %in% PL$date_open, "position", "no_position" )
  data$is_open_position[data$ref.date %in% PL$date_close] = "position"
  vec_which = which(data$is_open_position == "position")
  vec_which_start = vec_which[seq(1, (length(vec_which) - (1-length(vec_which)%%2)), by=2)]
  vec_which_end = vec_which[seq(2, (length(vec_which) - length(vec_which)%%2), by=2)]
  vec_position = c()
  for(i in 1:length(vec_which_end)){
    start = vec_which_start[i]
    end = vec_which_end[i]
    start_end = start:end
    vec_position = c(vec_position, start_end)
  }         
  data$is_open_position[vec_position] = "position"
  
  signal = signal[signal$date <= end_date_trading & signal$date >= start_date_trading ,]
  
  
  
  
  
  p_norm = ggplot(data, aes(x=ref.date, y=price.close.norm, group = ticker, colour = ticker )) +
    geom_line() + 
    xlab("Date") +
    ylab("Normalized closing price") +
    theme(legend.position=c(0.2,0.7))
  
  p_signal = ggplot(signal, aes(x=date, y=signal_value)) +
    geom_line() + 
    xlab("Date") +
    ylab("Signal value") +
    geom_hline(yintercept = 0, color = 'red')
  
  p_profit_hist = ggplot(PL, aes(x=profit)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 30) +
    geom_density(alpha=.2, fill="#FF6666") +
    geom_vline(aes(xintercept=mean(profit)),
               color="blue", linetype="dashed", size=1)
  
  p_profit = ggplot(PL, aes(x=date_close, y=cumulated_profit)) +
    geom_step() + 
    xlab("Date") +
    ylab("Cumulated profit")
  
  
  ss = statistics_strategy(PL)
  kpi = names(ss) 
  test = round(unlist(ss),2)
  test.df = data.frame("kpi" = kpi, "value" = test)
  p_stat = ggplot(test.df, aes(kpi,value)) + geom_col() +
    theme(axis.text.x = element_text(face="bold", angle=45)) +
    geom_text(aes(label = value), vjust = -1)
  
  p = grid.arrange(p_norm, p_profit, p_signal,  p_profit_hist, ncol= 2, nrow=2, heights=c(4, 1.5),
                   top = textGrob(title, gp=gpar(fontsize=10,font=3)))
  return(p)
}

statistics_strategy = function(PL){
  
  end_profit = PL$cumulated_profit[nrow(PL)]
  max_drawdown = min(PL$profit[PL$profit < 0])
  sharpe_ratio = mean(diff(PL$wealth)/PL$wealth[1:(length(PL$wealth)-1)])/sd(diff(PL$wealth)/PL$wealth[1:(length(PL$wealth)-1)])
  
  mean_position_length = mean(PL$position_length) # mean length (days) of the position
  max_position_length = max(PL$position_length)
  min_position_length = min(PL$position_length)
  sd_position_length = sd(PL$position_length)
  
  percentage_good = length(PL$profit[PL$profit > 0])/length(PL$profit)
  gain_pain_ratio = sum(PL$profit[PL$profit > 0])/sum(abs(PL$profit[PL$profit < 0]))
  
  max_gain = max(PL$profit)
  min_gain = min(PL$profit)
  sd_gain = sd(PL$profit)
  
  # Value at Risk, Conditional Tail Expectation, Expected Shortfall
  VaR = quantile(PL$profit, probs = c(0.005, 0.01, 0.05))
  VaR_995 = VaR[1]
  VaR_99 = VaR[2]
  VaR_95 = VaR[3]
  CTE_95 = mean(PL$profit[PL$profit <= VaR_95])
  ES_95 = mean(ifelse(PL$profit < VaR_95, 1,0)*PL$profit)    # ES Stop loss premium with a retention equal to the value at risk
  
  return(liste = list("end_profit" = end_profit,
                      "max_drawdown" = max_drawdown, 
                      "sharpe_ratio" = sharpe_ratio,
                      "mean_position_length" = mean_position_length,
                      "max_position_length " = max_position_length,
                      "min_position_length" = min_position_length,
                      "sd_position_length" = sd_position_length,
                      "percentage_good" = percentage_good, 
                      "gain_pain_ratio" = gain_pain_ratio,
                      "max_gain" = max_gain,
                      "min_gain" = min_gain,
                      "sd_gain" = sd_gain,
                      "VaR_995" = VaR_995,
                      "VaR_99" = VaR_99,
                      "VaR_95" = VaR_95,
                      "CTE_95" = CTE_95,
                      "ES_95" = ES_95))
}

PAIR_TRADING_AUTOMATION = function(data, A, B, start_date_trading, end_date_trading, initial_capital,
                                   limit, j, stop_loss, transaction_cost, is_test_cointegration, title, display_graphs){
  pt = pair_trading(data, A, B, start_date_trading, end_date_trading, limit, j, stop_loss, transaction_cost, is_test_cointegration)
  pl = P_and_L(data, A, B, pt$position_history, initial_capital)
  stats = statistics_strategy(pl)
  if(display_graphs == TRUE){
    plot_pair_trading(data, A, B, pl, pt$signal, start_date_trading, end_date_trading, title)
  }
  return(liste = list("data" = data,"A" = A, "B" = B, "start_date_trading" = start_date_trading, 
                      "end_date_trading" = end_date_trading, "initial_capital" = initial_capital,
                      "limit" =  limit, "j" = j, "stop_loss" = stop_loss, "transaction_cost" = transaction_cost,
                      "is_test_cointegration" = is_test_cointegration, 
                      "position_history" = pt$position_history, "signal" = pt$signal,
                      "PL" = pl, "statistics_strategy" = stats))
}

parameters_optimization = function(pair_trading_object, vec_j, vec_limit, vec_stop_loss, to_optimize, display_graphs){
  title = "parameters optimization loop"
  df_decision = c()
  for(j in vec_j){
    for(limit in vec_limit){
      for(stoploss in vec_stop_loss){
        pta = PAIR_TRADING_AUTOMATION(pair_trading_object$data, pair_trading_object$A, 
                                      pair_trading_object$B, pair_trading_object$start_date_trading,
                                      pair_trading_object$end_date_trading, pair_trading_object$initial_capital,
                                      limit, j,  stoploss,
                                      pair_trading_object$transaction_cost,
                                      pair_trading_object$is_test_cointegration, title, display_graphs)  
        df_decision = rbind(df_decision, c(j, limit, stoploss, pta$statistics_strategy[[to_optimize]]))
      }
    }
  }
  df_decision = as.data.frame(df_decision)
  colnames(df_decision) = c("j", "limit", "stop_loss", to_optimize)
  df_decision = df_decision[order(df_decision[to_optimize], decreasing = T),]
  j_opti = df_decision$j[1]            
  limit_opti = df_decision$limit[1]  
  stop_loss_opti = df_decision$stop_loss[1]
  return(liste = list("j_opti" = j_opti, "limit_opti" = limit_opti, "stop_loss_opti" = stop_loss_opti,"df_decision" = df_decision))
}

FINAL_PAIR_TRADING = function(A, B, to_optimize, vec_j, vec_limit, vec_stop_loss, length_in_sample, length_out_sample, initial_capital){
  lookback_window_years = length_in_sample + length_out_sample 
  cat("\n DOWNLOADING S&P500")
  SandP500 = import_SandP500(lookback_window_years)
  df = SandP500$df                             
  first_date = SandP500$first_date
  last_date = SandP500$last_date
  in_out_window_date = df$ref.date[which.closest(df$ref.date, first_date + 365*length_in_sample)]     # separation date in and out sample for optimization and back-testing
  data = pairs_data_formatting(A, B, df)        
  start_date_trading = data$ref.date[31]           
  end_date_trading = as.Date(in_out_window_date)   # best parameters : on in_sample data    
  cat("\n TRAINING : ", as.character(start_date_trading), " - ", as.character(in_out_window_date))
  cat("\n TESTING : ", as.character(in_out_window_date), " - ", as.character(last_date))
  transaction_cost = 10      
  is_test_cointegration = FALSE    
  title = "pair_trading_object for parameters optimization"
  display_graphs = FALSE
  pair_trading_object =  PAIR_TRADING_AUTOMATION(data, A, B, start_date_trading,
                                                 end_date_trading, initial_capital,
                                                 vec_j[1], vec_limit[1], vec_stop_loss[1], transaction_cost, 
                                                 is_test_cointegration, title, display_graphs)
  cat("\n OPTIMIZATION : ", to_optimize)
  param_opti = parameters_optimization(pair_trading_object, vec_j, vec_limit, vec_stop_loss, to_optimize, display_graphs)
  cat("\n OPTIMAL PROPOSAL")
  j_opti = param_opti$j_opti
  limit_opti = param_opti$limit_opti
  stop_loss_opti = param_opti$stop_loss_opti
  title = paste(" PAIR TRADING IN SAMPLE | Optimal : ", to_optimize," | trading from ", start_date_trading, " to ", end_date_trading, " | (j,limit,stop.loss) = (", j_opti, ",", limit_opti, ",", stop_loss_opti, ")", sep = "")
  display_graphs = TRUE
  pta_opti_in_sample = PAIR_TRADING_AUTOMATION(data, A, B, start_date_trading, end_date_trading, initial_capital, limit_opti, j_opti, stop_loss_opti, transaction_cost, is_test_cointegration, title, display_graphs)
  
  # propagation out sample with the best parameters found on the in-sample : 
  start_date_trading = as.Date(in_out_window_date)
  end_date_trading = SandP500$last_date
  is_test_cointegration = TRUE
  title = paste("PAIR TRADING OUT SAMPLE | Optimal : ", to_optimize, " | trading from ", start_date_trading, " to ", end_date_trading, " | (j,limit,stop.loss) = (", j_opti, ",", limit_opti, ",", stop_loss_opti, ")", sep = "")
  display_graphs = TRUE
  pta_opti_out_sample = PAIR_TRADING_AUTOMATION(data, A, B, start_date_trading, end_date_trading, initial_capital, limit_opti, j_opti, stop_loss_opti, transaction_cost, is_test_cointegration, title, display_graphs)
  
  return(liste = list("in_sample" = pta_opti_in_sample, "out_sample" = pta_opti_out_sample))
}

################################################################################
########################  COINTEGRATION CALCULUS   #############################
################################################################################

length_in_sample = 2                                
length_out_sample = 1                               
lookback_window_years = length_in_sample + length_out_sample 
SandP500 = import_SandP500(lookback_window_years)
df = SandP500$df
first_date = SandP500$first_date
last_date = SandP500$last_date
in_out_window_date = df$ref.date[which.closest(df$ref.date, first_date + 365*length_in_sample)]

# few minutes to compute
stocks_retained = pairs_identification(first_date, in_out_window_date, df)
head(stocks_retained$cointegrated_stocks) 
A = "CTLT"  
B = "ABT"
stocks_retained_visualization(A, B, df)


################################################################################
########################## Example from scratch  ###############################
################################################################################

length_in_sample = 2                                 # number of year of in sample
length_out_sample = 1                                # number of year out sample
lookback_window_years = length_in_sample + length_out_sample # number of years of study
SandP500 = import_SandP500(lookback_window_years)    # import all S&P500 stocks

df = SandP500$df                             
first_date = SandP500$first_date                # first date available in the data
last_date = SandP500$last_date                  # last date available in the data
A = "CTLT"                                      # stock A : determined by the function pairs_identification
B = "ABT"                                       # stock B : determined by the function pairs_identification
data = pairs_data_formatting(A, B, df)          # data frame of stock A and B in the right format
start_date_trading = data$ref.date[31]          # begining date of trading - arbitrary : 50 days after the first record
end_date_trading = last_date                    # end date of trading - arbitrary
initial_capital = 100000                        # 100k€ of initial capital invested in the strategy
j = 15                                          # parameter to optimize : window for returns calculation in signal function
limit = 3                                       # trigger limit of the strategy on the signal : parameter of the trading strategy
stop_loss = 200                                 # value of the stop loss = max value of loss acceptable by the investor
transaction_cost = 10                           # 10 bips : cost of transaction
is_test_cointegration = FALSE                   # whether or not a philisps cointegration test must be operated iteratively (TRUE on out sample, FALSE on in sample)
title = paste("trading from ", start_date_trading, " to ", end_date_trading, " | (j,limit,stop.loss) = (", j, ",", limit, ",", stop_loss, ")", sep = "")
display_graphs = TRUE

pta = PAIR_TRADING_AUTOMATION(data, A, B, start_date_trading, end_date_trading,
                              initial_capital, limit, j, stop_loss, transaction_cost,
                              is_test_cointegration, title, display_graphs)  
pta$statistics_strategy

################################################################################
############## BEST PARAMETERS & OUT SAMPLE PROPAGATION ########################
################################################################################

# best parameters = parameters for which a KPI is maximized (sharpe ratio ? end profit ? etc ?)
#                   on the in-sample data
to_optimize = "sharpe_ratio"     

# calculation on the in-sample database
length_in_sample = 2                               
length_out_sample = 1                                
lookback_window_years = length_in_sample + length_out_sample 
SandP500 = import_SandP500(lookback_window_years)    

df = SandP500$df                             
first_date = SandP500$first_date
last_date = SandP500$last_date
in_out_window_date = df$ref.date[which.closest(df$ref.date, first_date + 365*length_in_sample)]     # separation date in and out sample for optimization and back-testing
A = "CTLT"                                      
B = "ABT"                                       
data = pairs_data_formatting(A, B, df)        
start_date_trading = data$ref.date[31]     
end_date_trading = as.Date(in_out_window_date)   # best parameters : on in_sample data    
transaction_cost = 10      
initial_capital = 100000
is_test_cointegration = FALSE    
title = "pair_trading_object for parameters optimization"
display_graphs = FALSE

vec_j = c(1,seq(2,20, by = 2))
vec_limit = seq(0.1, 3.1, by = 0.2)
vec_stop_loss = seq(500, 2000, by = 500)
pair_trading_object =  PAIR_TRADING_AUTOMATION(data, A, B, start_date_trading,
                                               end_date_trading, initial_capital,
                                               vec_j[1], vec_limit[1], vec_stop_loss[1], transaction_cost, 
                                               is_test_cointegration, title, display_graphs)
param_opti = parameters_optimization(pair_trading_object, vec_j, vec_limit, vec_stop_loss, to_optimize, display_graphs)
j_opti = param_opti$j_opti
limit_opti = param_opti$limit_opti
stop_loss_opti = param_opti$stop_loss_opti
title = paste("IN SAMPLE | Optimal : ", to_optimize," | trading from ", start_date_trading, " to ", end_date_trading, " | (j,limit,stop.loss) = (", j_opti, ",", limit_opti, ",", stop_loss_opti, ")", sep = "")
display_graphs = TRUE
pta_opti_in_sample = PAIR_TRADING_AUTOMATION(data, A, B, start_date_trading, end_date_trading, initial_capital, limit_opti, j_opti, stop_loss_opti, transaction_cost, is_test_cointegration, title, display_graphs)

# propagation out sample with the best parameters found on the in-sample : 
start_date_trading = as.Date(in_out_window_date)
end_date_trading = SandP500$last_date
is_test_cointegration = TRUE
title = paste("OUT SAMPLE | Optimal : ", to_optimize, " | trading from ", start_date_trading, " to ", end_date_trading, " | (j,limit,stop.loss) = (", j_opti, ",", limit_opti, ",", stop_loss_opti, ")", sep = "")
display_graphs = TRUE
pta_opti_out_sample = PAIR_TRADING_AUTOMATION(data, A, B, start_date_trading, end_date_trading, initial_capital, limit_opti, j_opti, stop_loss_opti, transaction_cost, is_test_cointegration, title, display_graphs)


################################################################################
########################## IN ONE FUNCTION #####################################
################################################################################
# to_optimize could be : 
# [1] "end_profit"           "max_drawdown"         "sharpe_ratio"        
# [4] "mean_position_length" "max_position_length " "min_position_length" 
# [7] "sd_position_length"   "percentage_good"      "gain_pain_ratio"     
# [10] "max_gain"             "min_gain"             "sd_gain"             
# [13] "VaR_995"              "VaR_99"               "VaR_95"              
# [16] "CTE_95"               "ES_95"

# example 1 : 3 years
length_in_sample = 2                                
length_out_sample = 1                               
lookback_window_years = length_in_sample + length_out_sample 
SandP500 = import_SandP500(lookback_window_years)
df = SandP500$df
first_date = SandP500$first_date
last_date = SandP500$last_date
initial_capital = 100000
in_out_window_date = df$ref.date[which.closest(df$ref.date, first_date + 365*length_in_sample)]
stocks_retained = pairs_identification(first_date, in_out_window_date, df)
head(stocks_retained$cointegrated_stocks)   

# for example : "CTLT" and "ABT"
PT = FINAL_PAIR_TRADING (A = "CTLT", B = "ABT", to_optimize = "gain_pain_ratio", 
                         vec_j = seq(1,30,by=1), vec_limit =  c(0.2, 0.5, 1, 1.5, 2, 2.5, 3),
                         vec_stop_loss = c(500, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000),
                         length_in_sample, length_out_sample, initial_capital)
PT$in_sample$statistics_strategy
PT$out_sample$statistics_strategy

PT = FINAL_PAIR_TRADING (A = "CTLT", B = "ABT", to_optimize = "percentage_good", 
                         vec_j = seq(1,30,by=1), vec_limit =  c(0.2, 0.5, 1, 1.5, 2, 2.5, 3),
                         vec_stop_loss = c(500, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000),
                         length_in_sample, length_out_sample, initial_capital)
PT$in_sample$statistics_strategy
PT$out_sample$statistics_strategy

PT = FINAL_PAIR_TRADING (A = "CTLT", B = "ABT", to_optimize = "VaR_95", 
                         vec_j = seq(1,30,by=1), vec_limit =  c(0.2, 0.5, 1, 1.5, 2, 2.5, 3),
                         vec_stop_loss = c(500, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000),
                         length_in_sample, length_out_sample, initial_capital)
PT$in_sample$statistics_strategy
PT$out_sample$statistics_strategy

n_couples_cointegrated = nrow(stocks_retained$cointegrated_stocks)
set.seed(19021997)
ech = sample(1:n_couples_cointegrated, size = 20, replace = FALSE, prob = rep(1/n_couples_cointegrated,n_couples_cointegrated))
best_couple_in = c()
best_couple_out = c()
stocks_retained$cointegrated_stocks[ech,]
#           A    B pvalue
# 89250   IFF  WEC   0.01
# 44169   CME  HIG   0.01
# 58641   DLR  IBM   0.01
# 48125   CPB  HUM   0.01
# 17904  ANTM  FDX   0.01
# 2574    ABC  CMG   0.01
# 58732   DLR NCLH   0.01
# 53928     D  MLM   0.01
# 103146 MDLZ MNST   0.01
# 7039    AEE EXPE   0.01
# 7103    AEE INTC   0.01
# 16364   AMT  XOM   0.01
# 53368   CVX  XOM   0.01
# 64168   ECL TTWO   0.01
# 26105   BAX    K   0.01
# 10519  AKAM  MCO   0.01
# 25099    BA  DLR   0.01
# 16348   AMT  WBA   0.01
# 45919   CNC  PSA   0.01
# 34376   CAG MKTX   0.01

for(i in 1:20){
  A = stocks_retained$cointegrated_stocks$A[ech[i]]
  B = stocks_retained$cointegrated_stocks$B[ech[i]]
  PT = FINAL_PAIR_TRADING (A = A, B = B, to_optimize = "gain_pain_ratio", 
                           vec_j = seq(2,30,by=2), vec_limit =  c(0.2, 0.5, 1, 1.5, 2, 2.5, 3),
                           vec_stop_loss = c(1000, 2000, 3000, 4000, 5000, 6000, 7000),
                           length_in_sample, length_out_sample, initial_capital)
  bc_in = PT$in_sample$statistics_strategy
  bc_out = PT$out_sample$statistics_strategy
  best_couple_in[[length(best_couple_in)+1]] = bc_in
  best_couple_out[[length(best_couple_out)+1]] = bc_out
}
best_couple_in = as.data.frame(do.call(rbind, best_couple_in))
best_couple_in$couple = paste(stocks_retained$cointegrated_stocks$A[ech], stocks_retained$cointegrated_stocks$B[ech])
best_couple_out = as.data.frame(do.call(rbind, best_couple_out))
best_couple_out$couple = paste(stocks_retained$cointegrated_stocks$A[ech], stocks_retained$cointegrated_stocks$B[ech])
merge(best_couple_in,best_couple_out, by="couple")
# BA DLR, BAX K, ECL TTWO, AEE INTC, AEE EXPE, CPB HUM, DLR IBM, 


length_in_sample = 2                                
length_out_sample = 1 


PT = FINAL_PAIR_TRADING (A = "BAX", B = "K", to_optimize = "gain_pain_ratio", 
                         vec_j = c(seq(1,29,by=2),30), vec_limit =  c(0.5, 1, 1.5, 2, 2.5, 3),
                         vec_stop_loss = c(1000, 2000, 3000, 4000, 5000, 6000),
                         length_in_sample, length_out_sample, initial_capital = 100000)

PT = FINAL_PAIR_TRADING (A = "AEE", B = "INTC", to_optimize = "gain_pain_ratio", 
                         vec_j = c(seq(1,29,by=2),30), vec_limit =  c(0.5, 1, 1.5, 2, 2.5, 3),
                         vec_stop_loss = c(1000, 2000, 3000, 4000, 5000, 6000),
                         length_in_sample, length_out_sample, initial_capital = 100000)

PT = FINAL_PAIR_TRADING (A = "AEE", B = "EXPE", to_optimize = "gain_pain_ratio", 
                         vec_j = c(seq(1,29,by=2),30), vec_limit =  c(0.5, 1, 1.5, 2, 2.5, 3),
                         vec_stop_loss = c(1000, 2000, 3000, 4000, 5000, 6000),
                         length_in_sample, length_out_sample, initial_capital = 100000)

PT = FINAL_PAIR_TRADING (A = "CPB", B = "HUM", to_optimize = "gain_pain_ratio", 
                         vec_j = c(seq(1,29,by=2),30), vec_limit =  c(0.5, 1, 1.5, 2, 2.5, 3),
                         vec_stop_loss = c(1000, 2000, 3000, 4000, 5000, 6000),
                         length_in_sample, length_out_sample, initial_capital = 100000)

PT = FINAL_PAIR_TRADING (A = "DLR", B = "IBM", to_optimize = "gain_pain_ratio", 
                         vec_j = c(seq(1,29,by=2),30), vec_limit =  c(0.5, 1, 1.5, 2, 2.5, 3),
                         vec_stop_loss = c(1000, 2000, 3000, 4000, 5000, 6000),
                         length_in_sample, length_out_sample, initial_capital = 100000)

PT$in_sample$statistics_strategy
PT$out_sample$statistics_strategy
###############################################################################
# à ajouter : 

# en fonction du capital attribuer une taille max (capital init * taille position max)
# machine learning sur les position perdantes : var explicative = signal
# stop gain --> en cas de depassement --> liquider la position 
# egalement tout mettre en data.table pour plus de rapidité
# cout de transaction : fonction de la liquidité --> voir les modèles

setwd("/Users/robin/Documents/Pair_Trading/archive")






