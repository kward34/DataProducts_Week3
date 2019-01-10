
library(zoo)
library(tseries)
library(quantmod)
library(ggplot2)
library(plotly)

# import 
env1 = new.env()
getSymbols("^GSPC", env = env1, src ="yahoo", from = as.Date("1988-06-01"),to = Sys.Date())
GSPC = env1$GSPC
gspc.df = data.frame(date=time(GSPC), coredata(GSPC))

# create Bollinger Bands
bbands <- BBands(GSPC[,c("GSPC.High","GSPC.Low","GSPC.Close")])

# join and subset data
df <- subset(cbind(gspc.df, data.frame(bbands[,1:3])))

# plot candlestick chart
p <- df %>%
  plot_ly(x = ~date, type="candlestick",
          open = ~GSPC.Open, close = ~GSPC.Close,
          high = ~GSPC.High, low = ~GSPC.Low, name = "S&P 500") %>%
  add_lines(x = ~date, y = ~up , name = "B Bands",
            line = list(color = '#ccc', width = 0.5),
            legendgroup = "Bollinger Bands",
            hoverinfo = "none", inherit = F) %>%
  add_lines(x = ~date, y = ~dn, name = "B Bands",
            line = list(color = '#ccc', width = 0.5),
            legendgroup = "Bollinger Bands", inherit = F,
            showlegend = FALSE, hoverinfo = "none") %>%
  add_lines(x = ~date, y = ~mavg, name = "Mv Avg",
            line = list(color = '#E377C2', width = 0.5),
            hoverinfo = "none", inherit = F) %>%
  layout(yaxis = list(title = "S&P 500 Price"))
p



#ploting line graph in plotly
plot_ly(df, x = ~Date, y = ~GSPC) %>%
  add_lines(alpha=.8) %>%
  layout( title = "S&P 500 Index Performance", yaxis= list(title="S&P 500 (^GSPC)")) %>%
  add_BBands(n = 20, sd = 2, ma
             = "SMA", draw = "bands", on = -1)


p <- df %>%
  plot_ly(x = ~Date, type="candlestick", open = ~Open, close = ~Close, 
          high = ~High, low = ~Low ) %>%
  add_lines(x = ~Date, y = ~Open, line = list(color = 'Blue', width = 0.75), inherit = F) %>%
  layout(title = "Basic Candlestick Chart", showlegend=FALSE)
p

p1 <- df %>%
  plot_ly(x = ~date, y = ~GSPC.Adjusted, type = "scatter", mode = "lines" ) %>%
  layout(yaxis=list(title="S&P 500"), xaxis=list(title="Date"))
p1