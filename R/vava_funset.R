


# ----Model Evaluate-------------------------------------------------------------------------------

#' Model Evaluate
#' 
#' Evaluate models by calculate error of fitting.
#' 
#' @param y original series
#' @param y_hat fitted or forecasted series by certain model
#' @param method how to calculate the error of \emph{y} and \emph{y_hat}, default is \emph{all}, otherwise,
#' \emph{MRE}: mean relative error, \emph{MAE}: mean absolute error, \emph{RMSE}: Root Mean Square Error,
#' \emph{Theil}: Theil coefficient.
#' @return err a vector store the error of model calculated by \emph{method}.
#' @examples
#' x<-c(1,3,NA,3,56,2,6)
#' y<-c(1.1,2.3,NA,2.7,53,2.5,6.8)
#' x;y
#' modeval(x,y)
#' modeval(x,y,"MRE")
#' modeval(x,y,"MAE")
#' modeval(x,y,method="RMSE")
#' modeval(x,y,method="Theil")
#' 
#' modeval(x,y,method="Theilxxx") # fail
#' modeval(x, y , method = "R")
#' modeval(x, y , 9) # fail
#' @export modeval

modeval<-function(y,y_hat,method=c("all", "MRE","MAE","RMSE","Theil"))  {


  #
  # MRE: Mean Relative Error,=1/n*sum(|(y-y_hat)/y|)
  # MAE: Mean Absolute Error,
  # RMSE: Root Mean Square Error
  # Theil: ==RMSE=sqrt(sum(y^2)/n)+sqrt(sum(y_hat^2)/n)
  #-----------------

  
  if (class(method) == "character") {
    method_n = switch(match.arg(method, c("all", "MRE","MAE","RMSE","Theil")),
           all = 1L,
           MRE = 2L, MAE = 3L, RMSE = 4L, Theil = 5L)
  }
  else {
    method_n = round(method)
    if (method_n<=0L | method_n > 5L) stop("'method' must at range of 1~5, when 'method' is a numeric vector.")
    }
   
  
  

  y_hat<-as.numeric(y_hat)
  y<-as.numeric(y)


  mre = function(y,y_hat) {
    #
    if (any(is.infinite(1/y))) warning("'y' has 0, has been replaced by 1E-10, to avoid #/y has 'inf'.")
    inf<-which(is.infinite(1/y))
    #
    y[inf]<-1E-10
    
    
    diff<-abs((y-y_hat)/y)
    # 
    n<-length(na.omit(diff))
    error<-sum(diff,na.rm = T)/n  
    
    error
  }
  
  mae = function(y,y_hat)  {
    diff<-abs(y-y_hat)
    n<-length(na.omit(diff))
    error<-sum(diff,na.rm=T)/n
    error
  }
  
  rmse = function(y,y_hat)  {
    diff2<-(y-y_hat)^2
    n<-length(na.omit(diff2))
    error<-sum(diff2,na.rm = T)/n
    error<-sqrt(error)
    error
    
  }
  
  theil = function(y,y_hat)  {
    
    # mem<-ModEval(y=y,y_hat=y_hat,method="RMSE")
    mem = rmse(y=y, y_hat = y_hat)
    
    n_y<-length(na.omit(y))
    n_y_hat<-length(na.omit(y_hat))  #
    #
    s_y<-sum(y^2,na.rm = T)/n_y
    sq_y<-sqrt(s_y)
    s_y_hat<-sum(y^2,na.rm = T)/n_y_hat
    sq_y_hat<-sqrt(s_y_hat)
    
    deno<-sq_y+sq_y_hat
    #
    error<-mem/deno
    error
    
    
  }
  
  

 if (method_n == 1L) {
   all = rep(NA,4)
   names(all) = c("MRE","MAE","RMSE","Theil")
   
   all[1] = mre(y=y , y_hat = y_hat)
   all[2] = mae(y=y , y_hat = y_hat)
   all[3] = rmse(y=y , y_hat = y_hat)
   all[4] = theil(y=y , y_hat = y_hat)
   
   err = all
  
  }
  if (method_n == 2L)  err = mre(y=y , y_hat = y_hat)
  if (method_n == 3L)  err = mae(y=y , y_hat = y_hat)
  if (method_n == 4L)  err = rmse(y=y , y_hat = y_hat)
  if (method_n == 5L)  err = theil(y=y , y_hat = y_hat)
  
  err
  
}  
  
  

# ----lag--------------------------------------------------------------

#' return lag series
#' 
#' a simple method to get lag series
#' @param x a numeric vector 
#' @param k how many times to lag
#' @details returned series will have \code{k} \code{NA} in the head 
#' @export lag
#' @example 
#' x = 1:10
#' lag(x, 2)

lag=function(x,k=1) {
  n=length(x)
  if (k>n) stop("'k' must less then 'n'")
  lag=as.numeric(rep(NA,n))
  lag[(k+1):n]=x[1:(n-k)]

  lag
}



 

# ----reglist-------------------------------------------------------------------
#' return a pretty answer of regression 
#' 
#' @param ... objects
#' @param type default is \code{text}

reglist<-function(...,type="text")  {
  
  stargazer(...,type=type)

}



 

# ----imputed by regress--------------------------------------------------------------
#' imputed by regress
#' 
#' substitute \code{NA} by regression values
#' 
#' @param data numeric \code{data.frame}, that first column of data frame is dependent variable. 
#' @export impute_lm
#' @details Make sure that first column of data frame is dependent variable: Y, because progress will 
#' regards first column as dependent variable anyway.



impute_lm<-function(data)  {



  # warning("Make sure that first column of data frame is dependent variable: Y.")
  
  lm<-lm(data)
  y.pre<-predict(lm,data)
  
  y.imp<-data[,1]  ## 将数据框中第一列，即y取出
  y.imp[is.na(y.imp)]<-y.pre[is.na(y.imp)]  ## na替换为y.pre中得值
  
  data$y.impute<-y.imp  ## 填补后的y放入数据框最后一列，名为y.impute
  data$y.pre <- y.pre  # 预测值列
  
  data
 
}





 

# ----imputed by HoltWinters method --------------------------------------------------

#' imputed by HoltWinters method 
#' 
#' imputed by HoltWinters method, forward or afterward

#' @param x a vector, should converted to \code{ts} if not a annual time series.
#' @param extent specify how long to predict forward or afterward.
#' @param forward if \code{TRUE}, will predicts \code{extent} values forward, default is \code{FALSE}.
#' @param methods method for impute, now only \code{HoltWinters}.
#' @param alpha reference to \link{HoltWinters}.
#' @param beta ditto.
#' @param gamma ditto.
#' @param file where to store answers.
#' @param out export answers if \code{TRUE}.
#' @export impute_es
#' @examples 
#' x = c(12575.57 ,13965.77 ,14410.16 ,15531.18 ,17099.35 ,18231.83 ,19078.41 ,20062.66 ,21442.13 ,22879.18 ,23913.76 ,24365.62 ,25466.83 ,26441.65 ,27755.76 ,28762.68 ,30047.31 ,31553.62 ,32929.04 ,34601.72 ,36432.51 ,37241.35 ,38113.89 ,39591.86 ,41838.46 ,44218.31 ,46351.67 ,47954.53 ,48302.28 ,46909.42 ,48309.53 ,49725.5 ,51385.49 ,52615.34 ,54360.5)
#' x.ts = ts(x)
#' impute_es(x.ts, 6, gamma = F, forward = T)
#' impute_es(x.ts, 6, gamma = F, forward = T, out = F)

impute_es = function(x, extent = numeric(), forward = FALSE, methods = "HoltWinters", alpha = NULL, beta = NULL, gamma = NULL, 
                     file = paste(getwd(), "/HoltWintersAns.csv", sep = ""), out = TRUE)   {

  
  # if  (!is.vector(x))  stop("'x' is not a vector")
  
  
  if (forward)  {
    
    index = c(1 : length(x))
    
    x = x[order(index, decreasing = T)]
    
    fit = HoltWinters(x, alpha = alpha, beta = beta, gamma = gamma)
    library(forecast)
    fst = forecast(fit, extent)$mean
    index = c(1 : length(fst))
    fst = fst[order(index, decreasing = T)]  # 还原顺序
    
    
    
  }
  
  
  else {
    
    fit = HoltWinters(x, alpha = alpha, beta = beta, gamma = gamma)
    library(forecast)    
    fst = forecast(fit, extent)$mean
    
    
  }
  
  if (out) {
    write.csv(fst, file)
    file.show(file)
  }
  else {
    fst
  }
  
}




  

# ----growth------------------------------------------------------------------
#' calculate growth of a series 
#' 
#' calculate some type of growth such as rate of increase, symmetrical rate, in addition get chain relative ratio or 
#' other type ratios by specifying gap.
#' 
#' @param x a numeric vector
#' @param diff_gap the gap of calculating rate, you can get year on year rate of a annual series by specifing \code{diff_gap} for 12.
#' @param method \code{division}: \eqn{(x_t-x_{t-1})/x_{t-1}}; \code{difference}: \eqn{x_t-x_{t-1}}; 
#' \code{auto}: select method automatically.
#' @param symrate symmetry rate if \code{TRUE}.
#' @param standard whether to standard the ratio. \code{FALSE}:no, \code{TRUE}:yes.
#' @export growth
#' @examples 
#' x = 1:30
#' growth(x, method = "division" )
#' growth(x, method = 2)
#' growth(x, method = 3, symrate = T)
#' x[2] = -2
#' growth(x, method = 3, symrate = T)

growth = function(x, diff_gap = 1, method = c("division","difference","auto"),  symrate = FALSE, standard = FALSE )  {
  
  
  #--exception control------
  if (!is.vector(x)) stop("'x' must be a vector")
  if (!is.numeric(x)) stop("'x' must be a numeric vector")
  #--numerist options--
  if (class(method) == "character") {
    method_n = switch(match.arg(method, c("division","difference","auto")),
                      division = 1L,
                      difference = 2L, 
                      auto = 3L)
  }
  else {
    method_n = round(method)
    if (method_n<=0L | method_n > 3L) stop("'method' must at range of 1~3, when 'method' is a numeric vector.")
  }
  
  #--symmetry change rate
  if (symrate)  {  
    x_t<-x[-(1 : diff_gap)]
    lx = length(x)
    x_t1<-x[-((lx - diff_gap + 1) : lx)]
    
    #--difference
    if (method_n == 2L) {
      sym_r <- x_t-x_t1
    }
    #--auto i.e.there are some none-positive value
    if (method_n == 3L) {
      if (any(x<=0)) {
        sym_r <- x_t-x_t1
        message("'x' has some none-positive value, automatically used 'difference' method to calculate rate.")
      }
      else {
        sym_r <- 200*(x_t-x_t1)/(x_t+x_t1)
      }
    }
    #--division
    if (method_n == 1L) {
      sym_r <- 200*(x_t-x_t1)/(x_t+x_t1)
      
    }
    
    
    #--standard, mean of abs is equal to 1
    
    if (standard) {
      
      std_fct <- sum(abs(sym_r)) / (length(sym_r))   ##标准化因子
      sym_r <- sym_r/std_fct
      
    }
    
    
  }
  
  #--asymmery change rate, i.e. normal growth rate.
  if (! symrate) {
    
    
    x_t<-x[-(1 : diff_gap)]
    lx = length(x)
    x_t1<-x[-((lx - diff_gap + 1) : lx)]  # 上期的值
    
    #--difference
    if (method_n == 2L) {
      sym_r <- x_t-x_t1
    }
    #--auto i.e.there are some none-positive value
    if (method_n == 3L) {
      if (any(x<=0)) {
        sym_r <- x_t-x_t1
        message("'x' has some none-positive value, automatically used 'difference' method to calculate rate.")
      }
      else {
        sym_r <- (x_t-x_t1) / x_t1
      }
    }
    #--division
    if (method_n == 1L) {
      sym_r <- (x_t-x_t1) / x_t1
      
    }
    
    
    
    #--standard
    
    if (standard) {
      
      std_fct<-sum(abs(sym_r))/(length(sym_r))   ##标准化因子
      sym_r<-sym_r/std_fct
      
      
    }
    
  }
  
  sym_r = c(NA, sym_r)
  sym_r 
  
}






# ----return time series with seasonal character randomly ---------------------------------

#' return time series with seasonal character randomly
#' 
#' return time series with seasonal character randomly, including \code{TC},\code{S},\code{I}.
#' @param n number of observation 
#' @param frequency monthly: 12; quarter: 4; annual: 1.
#' @param cyl cycle, mean long of cycle.
#' @param intercept intercept of trend line.
#' @param slope slope of trend line.
#' @param s a vector specifing seasonal factor, containing weights for every month.
#' @param irr_adj adjust irregular factor.
#' @export tcsi
#' @examples  
#' plot(tcsi,type = "l")
#' 
#' ts.tcsi<-ts(tcsi,frequency = 12)
#' 
#' plot(decompose(ts.tcsi))
#' 


tcsi <- function(n, frequency = 12, cyl = 60, intercept = 100, slope = 1, s = c(1:12), irr_adj = 0.1)  {
  
  c<-cyl  ##假定平均景气循环为5年，即60个月
  a<-intercept  ## 趋势截距
  b<-slope   ##趋势斜率
  
  
  
  
  # set.seed(1324)
  # 
  theta<-runif(n,0,0.00167*c)  ## 随机相位
  t <- c(1:n)
  s = s / sum(s)
  
  
  trend<-a+b*t
  cyl <- 0.1*a*cos(2*pi*(t/c+theta))   #周期为c的随机余弦波
  seas = c(rep(s,n/length(s)))  # 各月占比组成季节因素
  irr <- rnorm(n,mean=irr_adj * a, sd = 0.1 * irr_adj * a)  # 随机因素大小波动受趋势截距和给定调节参数影响
  
  tcsi = seas*(trend+cyl)+irr
  
  tcsi 
  
}






# ----similar coefficient of industry structure ------------------------------------

#' similar coefficient of industry structure
#' 
#' @param x1,x2 industry's proportion of region 1 and region 2.
#' @export similarcoef
#' @examples 
#' x1 = c(1:10)
#' x2 = c(1:10)
#' similarcoef(x1, x2)
#' 
#' x1 = rep(0,10)
#' x2 = rep(1,10)
#' similarcoef(x1,x2)


similarcoef <- function(x1, x2)  {


  
  #--exception control 
  if (any(x1 < 0)) stop("'x1' must be non-negative vector")
  if (any(x2 < 0)) stop("'x2' must be non-negative vector")
  
  #--
  x1 = as.numeric(x1)
  x2 = as.numeric(x2)   # 解决：Warning message:
      # In x1 * x2 : NAs produced by integer overflow
  
  mem = sum(x1 * x2)
  den = sqrt(sum(x1^2) * sum(x2^2))
  sicoef = mem / den
  sicoef  
  
  
  
}




# ----order columns based on row's weighing means---------------------------------------------------------

#' order columns based on row's weighing means
#' 
#' @param x data.frame.
#' @param sortcols columns for comparing.
#' @param weight 
#' @param decreasing \code{TRUE} decreasing order, default.
#' @param mean_row whether to append a row including mean values to last row.
#' @export sortcol
#' @examples 
#' year = c(2001:2005)
#' A = c(8,9,10,12,10)
#' B = c(9,10,11,10,9)
#' C = c(13,14,16,17,12)
#' weight = rep(1,5)
#' dt = data.frame(year,A,B,C,weight)
#' 
#' sortcol(dt)
#' sortcol(dt,mean_row = F)
#' sortcol(dt,sortcols = c(5,4,3,2,1) , weight = "B" , decreasing = F)
#' sortcol(dt,sortcols = c(2,3,4), weight = "weight")
# 
# 不同个体同一指标在不同时间上加权（等权或不等权）平均后，
# 根据平均值大小排序，如：A、B、C三个地区在2001-2005年GDP增长率：
#  year  A    B    C  weight
#  2001  8    9   13   1
#  2002  9    10  14   1
#  2003  10   11  16   1
#  2004  12   10  17   1
#  2005  10   9   12   1
# sortcol(dt,sortcols = c(2,3,4), weight = "weight") -- >
#       C    A    B
# 1    13.0  8.0  9.0
# 2    14.0  9.0 10.0
# 3    16.0 10.0 11.0
# 4    17.0 12.0 10.0
# 5    12.0 10.0  9.0
# mean 14.4  9.8  9.8




sortcol <- function(x, sortcols = c(1:length(x)), weight = NULL, decreasing = TRUE, mean_row = TRUE)  {

  
  
  x_len = length(x)
  orcols_len = length(sortcols)
  #--异常控制--
  if (!class(x) == "data.frame") stop("'x' should be a data frame")
  if (x_len < orcols_len) stop("Compare to length of 'x', 'sortcols' 's length is too long")
  # if (!(class(sortcols) == "character" | class(sortcols) == "numeric")) {
  #   stop("vector 'sortcols' should be a numeric or character")
  # }
  
  if (class(sortcols) == "character") {
    
    sortcols = match(sortcols, names(x))  # 统一为数值型
  }
  if (class(weight) == "character")  weight_col = match(weight, names(x))
  
  order_vec = NULL
  #--
  for (i in 1:orcols_len) {   # 遍历 orcols_len里的数值
    if (is.null(weight))  {
      col = sortcols[i] # 指定计算x中第几列
      col_mean = mean(x[,col])  # 等权==简单平均
      order_vec = c(order_vec, col_mean)  # 将平均的结果存入order_vec中
    }
    else {
      col = sortcols[i] # 指定计算x中第几列
      col_mean = sum(x[,col]*x[,weight_col]) / sum(x[,weight_col])
      order_vec = c(order_vec, col_mean)  # 将加权的结果存入order_vec中
    }
    
    # order_inx = as.data.frame(sortcols, order_vec)
    
    
  }
  
  sortcols = sortcols[order(order_vec, decreasing = decreasing)]  # 根据行均值排序的列索引
  order_vec = order_vec[order(order_vec, decreasing = decreasing)]  # 排序后的存放均值的向量
  
  
  df = x[sortcols]   # 按sortcols指定的列顺序得到新的数据框
  
  if (mean_row)  {
    mean = matrix(order_vec, 1)
    mean = as.data.frame(mean, row.names = "mean")
    names(mean) = names(df)  #  统一名称
    df = rbind(df,mean) 
  }
  
  
  df
}






#====================================================================

# ----日期索引：指定起止日期，返回年月或年季度信息---------

#' return date index
#' 
#' return date index: can return year and monthly or seasonal information when 
#' assign start and end date.
#' @param start a numeric vector like \code{c(year, month)}.
#' @param end like \code{start}.
#' @param frequency 1: annual; 4: seasonal; monthly: 12.
#' @details reference \link{ts}.
#' @export DateIndex
#' @examples 
#' (lst = DateIndex(start = c(2001,9), end = c(2011,7), frequency = 12) )



DateIndex <- function (start = numeric(), end = numeric(), frequency = 1, 
                       ts.eps = getOption("ts.eps") )
{

  if (length(start) !=2 | length(end) != 2) stop("'start' and 'end' should like : 'start = c(2016,10)'")
  if (frequency > 1 && abs(frequency - round(frequency)) < 
      ts.eps) 
    frequency <- round(frequency)
  if (length(start) > 1L) {
    start <- start[1L] + (start[2L] - 1)/frequency
  }
  if (length(end) > 1L) {
    end <- end[1L] + (end[2L] - 1)/frequency
  }
  
  

  if (start > end) 
    stop("'start' cannot be after 'end'")
  nobs <- floor((end - start) * frequency + 1.01)
  
  dateseries = seq(start , end , length.out = nobs)
  
  Year = floor(dateseries)  # 存放年度
  
  MonQua = floor( (dateseries-Year) * frequency + 1.01)  # 存放月度或季度
  
  # date = data.frame(Year, MonQua)

  lst = list(Year = Year, MonthQuarter = MonQua)
  # lst = list(date = date, start = start , end = end , frequency= frequency, dateseries = dateseries)
  lst
  
}





# ----open folder---------------------------------------------------------------------------------------

#' open folder
#' 
#' open folder like \code{stata}'s command \code{cdout}, it is a convenient command.
#' 
#' @param wd directory that to open, default is current directory.
#' @export cdout
#' @examples 
#' cdout()
#' cdout("C:/Windows")


cdout = function(wd = getwd()) {
 
  shell(paste("start", wd)) 
} 







# ----export answers -------------------------------------

#' export answers
#' 
#' @param x object will be exported
#' @param file what to store answers
#' @param open whether to open \code{file} containing answers, \code{TRUE}:yes;\code{no}.


ans.out = function(x, file = ".ans.csv", open = TRUE) {

  write.csv(x = x, file = file)
  file.show(file)
  
  
}






