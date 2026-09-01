library(tidyr)
library(reactable)
library(htmltools)
library(tidyquant)
library(dplyr)
library(lubridate)

# ===================== 品种映射（行名直接显示中文） =====================
code_map <- c(
  "513100.SS" = "纳指ETF",
  "518880.SS" = "黄金ETF",
  "510300.SS" = "沪深300ETF"
)

codes <- names(code_map)
end_date <- Sys.Date()
start_date <- end_date - years(2)

# 下载数据
price_data <- tq_get(codes, from = start_date, to = end_date)
colnames(price_data)[1] <- "symbol"

# 计算日收益率
ret_data <- price_data %>%
  group_by(symbol) %>%
  mutate(daily_ret = round((adjusted / lag(adjusted) - 1) * 100, 2)) %>%
  ungroup() %>%
  drop_na()

# 统一气泡样式
cell_style <- function(val) {
  if (is.na(val)) {
    return("")
  }
  bg_col <- ifelse(val < 0, "#27ae60", ifelse(val > 0, "#e74c3c", "#fff"))
  abs_v <- abs(val)
  max_abs <- 4
  ratio <- sqrt(abs_v / max_abs)
  size <- 24 + (46 - 24) * ratio

  div(
    style = "display: flex; align-items: center; justify-content: center; height: 100%",
    div(
      style = paste0("width:", size, "px; height:", size, "px; border-radius:50%; background:", bg_col, "; display:flex; align-items:center; justify-content:center;"),
      span(sprintf("%.2f", val), style = "font-size: 11px; color:#222")
    )
  )
}

# ===================== 图1：最近10个交易日 =====================
dates <- sort(unique(ret_data$date), decreasing = T)
d10 <- head(dates, 10)

tbl1 <- ret_data %>%
  filter(date %in% d10) %>%
  pivot_wider(id_cols = symbol, names_from = date, values_from = daily_ret) %>%
  mutate(品种 = code_map[symbol]) %>%
  relocate(品种, as.character(d10)) %>%
  select(-symbol) # 删除代码列

cols1 <- lapply(as.character(d10), function(d) colDef(name = substr(d, 6, 10), align = "center", cell = cell_style))
names(cols1) <- as.character(d10)

reactable(tbl1, columns = c(list(品种 = colDef(align = "center")), cols1), bordered = T, highlight = T, defaultColDef = colDef(minWidth = 88))

# ===================== 图2：最近12个月 =====================
month_data <- ret_data %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(symbol, month) %>%
  summarise(m_ret = mean(daily_ret, na.rm = T) %>% round(2), .groups = "drop")

months <- sort(unique(month_data$month), decreasing = T)
m12 <- head(months, 12)

tbl2 <- month_data %>%
  filter(month %in% m12) %>%
  pivot_wider(id_cols = symbol, names_from = month, values_from = m_ret) %>%
  mutate(品种 = code_map[symbol]) %>%
  relocate(品种, as.character(m12)) %>%
  select(-symbol) # 删除代码列

cols2 <- lapply(as.character(m12), function(m) colDef(name = substr(m, 3, 7), align = "center", cell = cell_style))
names(cols2) <- as.character(m12)

reactable(tbl2, columns = c(list(品种 = colDef(align = "center")), cols2), bordered = T, highlight = T, defaultColDef = colDef(minWidth = 88))

# ===================== 图3：阶段收益率（1/5/10/20/60/120日、1年） =====================
period_ret <- ret_data %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(
    p1 = (adjusted / lag(adjusted) - 1) * 100,
    p5 = (adjusted / lag(adjusted, 5) - 1) * 100,
    p10 = (adjusted / lag(adjusted, 10) - 1) * 100,
    p20 = (adjusted / lag(adjusted, 20) - 1) * 100,
    p60 = (adjusted / lag(adjusted, 60) - 1) * 100,
    p120 = (adjusted / lag(adjusted, 120) - 1) * 100,
    p250 = (adjusted / lag(adjusted, 250) - 1) * 100
  ) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  select(symbol, p1, p5, p10, p20, p60, p120, p250) %>%
  mutate(across(-symbol, ~ round(., 2)))

colnames(period_ret) <- c("symbol", "最新1日", "近5日", "近10日", "近20日", "近60日", "近120日", "近1年")

tbl3 <- period_ret %>%
  mutate(品种 = code_map[symbol]) %>%
  relocate(品种) %>%
  select(-symbol) # 删除代码列

period_cols <- list(
  "最新1日" = colDef(align = "center", cell = cell_style),
  "近5日" = colDef(align = "center", cell = cell_style),
  "近10日" = colDef(align = "center", cell = cell_style),
  "近20日" = colDef(align = "center", cell = cell_style),
  "近60日" = colDef(align = "center", cell = cell_style),
  "近120日" = colDef(align = "center", cell = cell_style),
  "近1年" = colDef(align = "center", cell = cell_style)
)

reactable(
  tbl3,
  columns = c(list(品种 = colDef(align = "center")), period_cols),
  bordered = T, highlight = T, defaultColDef = colDef(minWidth = 100)
)
