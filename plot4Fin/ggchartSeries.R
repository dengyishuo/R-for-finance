library(ggplot2)
library(gridExtra)
library(TTR)
library(quantmod)

ggchartSeries <- function(data, theme = "white", type = "candlesticks", TA = NULL, show.volume = TRUE, ...) {
  # 转换数据为数据框并处理日期
  df <- fortify(data)
  df$Date <- index(data)
  colnames(df) <- c("Open","High","Low","Close","Volume","Date")

  # 初始化主图和面板列表
  p_main <- ggplot()
  p_volume <- NULL
  panels <- list()

  # 定义颜色方案
  if (theme == "white") {
    up_col <- "green"
    down_col <- "red"
    background <- "white"
  } else {
    up_col <- "darkgreen"
    down_col <- "darkred"
    background <- "black"
  }

  # 绘制主图类型
  if (type == "candlesticks") {
    # 计算蜡烛宽度
    width <- 0.9 * as.numeric(median(diff(df$Date)))

    p_main <- ggplot(df, aes(x = Date)) +
      geom_rect(aes(xmin = Date - width/2, xmax = Date + width/2,
                    ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                    fill = Close >= Open), color = "black") +
      geom_segment(aes(x = Date, xend = Date, y = Low, yend = High)) +
      scale_fill_manual(values = c(down_col, up_col)) +
      theme(legend.position = "none") +
      labs(y = "Price", x = "")
  } else if (type == "line") {
    p_main <- ggplot(df, aes(x = Date, y = Close)) +
      geom_line(color = "blue") +
      labs(y = "Price", x = "")
  }

  # 处理成交量
  if (show.volume) {
    p_volume <- ggplot(df, aes(x = Date, y = Volume)) +
      geom_bar(stat = "identity", fill = "gray50") +
      labs(y = "Volume", x = "Date")
  }

  # 定义TA函数环境
  env <- new.env()
  env$df <- df
  env$p_main <- p_main
  env$panels <- panels
  env$theme <- theme

  # 定义添加TA的函数
  add_SMA <- function(n = 20, col = "blue", ...) {
    sma <- SMA(env$df$Close, n)
    env$df[[paste0("SMA", n)]] <- sma
    env$p_main <- env$p_main +
      geom_line(data = env$df, aes_string(y = paste0("SMA", n)), color = col, ...)
  }

  add_RSI <- function(n = 14, col = "purple", ...) {
    rsi <- RSI(env$df$Close, n = n)
    env$df$RSI <- rsi
    p_rsi <- ggplot(env$df, aes(x = Date, y = RSI)) +
      geom_line(color = col, ...) +
      geom_hline(yintercept = c(30, 70), linetype = "dashed", color = "red") +
      labs(y = "RSI", x = "")
    env$panels <- c(env$panels, list(p_rsi))
  }

  add_BBands <- function(n = 20, sd = 2, ...) {
    bb <- BBands(env$df[,c("High","Low","Close")], n = n, sd = sd)
    env$df$BB_upper <- bb$up
    env$df$BB_middle <- bb$mavg
    env$df$BB_lower <- bb$dn
    env$p_main <- env$p_main +
      geom_ribbon(data = env$df, aes(ymin = BB_lower, ymax = BB_upper),
                  fill = "grey70", alpha = 0.5, ...) +
      geom_line(data = env$df, aes(y = BB_middle), color = "blue", ...)
  }

  # 映射TA字符串到函数
  ta_mapping <- list(
    addSMA = add_SMA,
    addRSI = add_RSI,
    addBBands = add_BBands
  )

  # 处理TA参数
  if (!is.null(TA)) {
    for (ta_expr in TA) {
      # 解析表达式
      parsed <- parse(text = ta_expr)
      func_name <- gsub("\\(.*", "", ta_expr)

      # 提取参数
      args <- eval(parse(text = sub(paste0(func_name, "\\("), "list(", ta_expr)))

      # 调用对应的TA函数
      if (func_name %in% names(ta_mapping)) {
        do.call(ta_mapping[[func_name]], args)
      } else {
        warning(paste("TA function", func_name, "not supported"))
      }
    }
  }

  # 组合所有图形
  plots <- list(env$p_main)
  if (show.volume) plots <- c(plots, list(p_volume))
  plots <- c(plots, env$panels)

  # 设置面板高度比例
  heights <- c(rep(3, length(plots) - length(env$panels)), rep(1, length(env$panels)))

  # 应用主题
  for (i in seq_along(plots)) {
    plots[[i]] <- plots[[i]] +
      theme(
        plot.background = element_rect(fill = background),
        panel.background = element_rect(fill = background),
        panel.grid.major = element_line(color = "gray50"),
        panel.grid.minor = element_line(color = "gray30"),
        text = element_text(color = ifelse(theme == "white", "black", "white"))
      )
  }

  # 输出图形
  grid.arrange(grobs = plots, ncol = 1, heights = heights)
}
