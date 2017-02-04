#' @title list frequently-used functions
#' \code{vava} lists the list of frequently-used functions in \code{vava} package (function's content).

#' @examples 
#' vava()
#' @export vava
#' @author Hejun Liu, Beijing \email{liuhejunlj@@163.com}  


#             source("D:\\Work\\R\\__MyR\\NisusR.R", encoding = 'UTF-8')


vava <- function()  {
  
  FunNames<-c("modeval", "lag" ,
              "reglist",
              "impute_lm",
              "impute_es",
              "growth",
              "tcsi",
              "similarcoef",
              "sortcol",
              "cdout"              
              
  )
  Descriptions<-c("--model evaluate",
                  "--get lag series" ,
                  "--展示回归结果" ,                 
                  "--回归填补",
                  "--指数平滑法向前or向后填补" ,
                  "--求增长率（环比、同比，对称变化率...）",
                  "--随机生成季节性明显的时间序列",
                  "--计算两地区产业结构相似系数",
                  "--对列排序（依据行加权均值大小）",
                  "--打开文件夹"
                  
  )
  data.frame(FunNames,Descriptions)
}
