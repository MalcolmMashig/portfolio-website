# clv-analysis.R

library(tidyverse)
library(lubridate)

sales_raw <- read_csv("sample-sales-data.csv") %>% 
  mutate(
    discount_given = (Sales / (1 - Discount)) - Sales,
    ordered_at = mdy(`Order Date`),
    location_name = Region,
    price_charged = Sales,
    customer_id = `Customer ID`
  )

prepare <- function(
  data, 
  ordered_at, location_name, customer_id, price_charged, discount_given
) {
  data %>% 
    mutate(
      month = {{ordered_at}} %>% floor_date(unit = "month") %>% ymd(),
      quarter = {{ordered_at}} %>% floor_date(unit = "quarter") %>% ymd(),
      year = {{ordered_at}} %>% floor_date(unit = "year") %>% ymd()
    ) %>% 
    select(
      {{ordered_at}}, 
      {{location_name}}, 
      {{customer_id}}, 
      {{price_charged}}, 
      {{discount_given}},
      month, quarter, year
    ) %>% 
    arrange(ordered_at)
}

transaction <- sales_raw %>% 
  prepare(
    ordered_at, location_name, customer_id, price_charged, discount_given
  )

transaction_east <- transaction %>% 
  filter(location_name == "East")

# ---- Key Performance Indicators (KPIs) ----
# Monthly sales over time
# Total customers acquired 
# Customer acquisition cost (CAC)
# Distribution of spend per purchase
# Initial versus repeat sales volume
# Initial versus repeat average order value (AOV)
# Sales and AOV by source
# First-purchase profitability
# Cohorted sales (the “C3”)
# Revenue retention curves
# Cumulative spend per customer
# Distribution of total spend by customer
# Customer concentration (“Pareto”) chart 

# That summarize...
# Growth
# Unit costs
# Unit profitability
# Retention
# Heterogeneity (customers, time)

# ---- Monthly sales over time ----

monthly_sales <- transaction_east %>% 
  group_by(month) %>% 
  summarize(sales = sum(price_charged))

monthly_sales %>% 
  ggplot(aes(month, sales, label = scales::dollar(sales))) +
  geom_bar(stat = "identity") +
  geom_text(
    size = 3,
    angle = 90,
    nudge_y = 10000
  ) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels =  "%b %Y",
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(),
    limits = c(0, 60000),
    breaks = seq(0, 60000, 10000)
  ) +
  labs(
    y = "Sales", 
    title = "Monthly spend over time (East-region customers)"
  ) +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title.x = element_blank()
  )

# ---- Total Customers Acquired ----

monthly_customers_acquired <- transaction_east %>% 
  distinct(customer_id, .keep_all = TRUE) %>% 
  group_by(month) %>% 
  summarize(customers_acquired = n())

monthly_customers_acquired %>% 
  ggplot(
    aes(month, customers_acquired, 
        label = customers_acquired)
  ) +
  geom_bar(stat = "identity") +
  geom_text(
    size = 3,
    angle = 90,
    nudge_y = 10
  ) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels =  "%b %Y",
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    labels = scales::comma_format(),
    limits = c(0, 60)
  ) +
  labs(
    y = "Customers Acquired", 
    title = "Total customers acquired over time (Corner Location)"
  ) +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title.x = element_blank()
  )

# ---- Customer Acquisition Cost (CAC) ----

# NOTE: Use `discount_given` as "acquisition cost", probably not appropriate

monthly_CAC <- transaction_east %>% 
  group_by(month) %>%
  summarize(total_discounts = sum(discount_given)) %>% 
  inner_join(monthly_customers_acquired) %>% 
  mutate(
    CAC = round(total_discounts / customers_acquired, 2)
  )

monthly_CAC %>% 
  ggplot(
    aes(month, CAC / 100)
  ) +
  geom_bar(aes(y = customers_acquired, size = "Customers Acquired"), 
           stat = "identity") +
  geom_line(aes(color = "CAC"), size = 2) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels =  "%b %Y",
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    labels = scales::comma_format(),
    limits = c(0, 60),
    sec.axis = sec_axis( trans = ~.*100, name = "CAC")
  ) +
  labs(
    y = "Customers Acquired", 
    title = "CAC vs total customers acquired over time (Corner Location)"
  ) +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

# ---- Distribution of Spend Per Purchase ----

# raw distribution -- not meaningful
transaction_east %>% 
  ggplot(aes(price_charged)) +
  geom_density()

# zoom in from extreme outliers and exclude refunds
transaction_east %>% 
  ggplot(aes(price_charged)) +
  geom_density() +
  xlim(0, 300)

transaction_east %>% 
  ggplot(aes(price_charged)) +
  geom_histogram(aes(y = ..density..), bins = 25) +
  xlim(0, 1000) +
  labs(
    title = "Distribution of spend given purchase",
    subtitle = "With chi-sq distribution (10 degrees of freedom) overlayed",
    x = "Purchase Amount",
    y = "Proportion of All Transactions"
  )

transaction_east %>% 
  filter(price_charged < 1000) %>% 
  mutate(purchase = cut_interval(price_charged, length = 100)) %>% 
  count(purchase, wt = price_charged) %>% 
  ggplot(aes(purchase, n)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribution of total revenue across different purchase amounts",
    x = "Purchase Amount ($)", 
    y = "Revenue ($)"
  ) +
  theme(
    axis.text.x = element_text(angle = 90),
  )

# ---- Initial Versus Repeat Sales Volume ----

# NOTE: Customer Type is defined in two ways...
# 1) A customer is only "new" in first transaction
# 2) A customer is "new" in month of first transaction (cohort)

# 1)
monthly_sales_new_repeat <- transaction_east %>% 
  group_by(customer_id) %>% 
  mutate(
    visit_num = row_number(),
    customer_type = if_else(
      visit_num == 1, "new", "repeat"
    )
  ) %>% 
  group_by(month, customer_type) %>% 
  summarize(sales = sum(price_charged),
            n_transactions = n()) %>% 
  group_by(month) %>% 
  mutate(
    pct_of_sales = sales / sum(sales) * 100
  )

# Sales
monthly_sales_new_repeat %>% 
  ggplot(aes(month, sales, fill = customer_type)) +
  geom_bar(stat = "identity")

# Number of Transactions
monthly_sales_new_repeat %>% 
  ggplot(aes(month, n_transactions, fill = customer_type)) +
  geom_bar(stat = "identity")

# Percent of Sales
monthly_sales_new_repeat %>% 
  ggplot(aes(month, pct_of_sales, fill = customer_type)) +
  geom_bar(stat = "identity") +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels =  "%b %Y",
    expand = c(0.01, 0.01)
  ) +
  labs(
    y = "Percent of Total Sales", 
    title = "New vs Repeat Sales, % of Total"
  ) +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

# 2)
monthly_sales_new_repeat_cohort <- transaction_east %>% 
  group_by(customer_id) %>% 
  mutate(
    month_acquired = first(month),
    customer_type = if_else(
      month == month_acquired, "new", "repeat"
    )
  ) %>% 
  group_by(month, customer_type) %>% 
  summarize(sales = sum(price_charged),
            n_transactions = n()) %>% 
  group_by(month) %>% 
  mutate(
    pct_of_sales = sales / sum(sales) * 100
  )

# Sales
monthly_sales_new_repeat_cohort %>% 
  ggplot(aes(month, sales, fill = customer_type)) +
  geom_bar(stat = "identity")

# Transactions
monthly_sales_new_repeat_cohort %>% 
  ggplot(aes(month, n_transactions, fill = customer_type)) +
  geom_bar(stat = "identity")

# Percent of Sales
monthly_sales_new_repeat_cohort %>% 
  ggplot(aes(month, pct_of_sales, fill = customer_type)) +
  geom_bar(stat = "identity") +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels =  "%b %Y",
    expand = c(0.01, 0.01)
  ) +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  labs(
    title = "Total sales from repeat vs new cohort customers (% of total)",
    y = "Percent of Sales (%)"
  )

# ---- Initial Versus Repeat Average Order Value (AOV) ----

# 1)
monthly_AOV_new_repeat <- transaction_east %>% 
  group_by(customer_id) %>% 
  mutate(
    visit_num = row_number(),
    customer_type = if_else(
      visit_num == 1, "initial", "repeat"
    )
  ) %>% 
  group_by(month, customer_type) %>% 
  summarize(
    AOV = mean(price_charged)
  )

monthly_AOV_new_repeat %>% 
  ggplot(aes(month, AOV, color = customer_type)) +
  geom_line(size = 2) +
  geom_smooth(method = "lm", se = FALSE, linetype = 2, size = 0.5) +
  labs(
    title = "Initial vs repeat AOV over time",
    y = "Average Order Value (AOV) ($)"
  ) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels =  "%b %Y",
    expand = c(0.01, 0.01)
  ) +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

# 2)
monthly_AOV_new_repeat_cohort <- transaction_east %>% 
  filter(!is.na(customer_id)) %>% 
  group_by(customer_id) %>% 
  mutate(
    month_acquired = first(month),
    customer_type = if_else(
      month == month_acquired, "new", "repeat"
    )
  ) %>% 
  group_by(month, customer_type) %>% 
  summarize(
    AOV = mean(price_charged)
  )

monthly_AOV_new_repeat_cohort %>% 
  ggplot(aes(month, AOV, color = customer_type)) +
  geom_line(size = 2) +
  geom_smooth(method = "lm", se = FALSE, linetype = 2, size = 0.5) +
  labs(
    title = "Initial vs repeat cohort AOV over time",
    y = "Average Order Value (AOV) ($)"
  ) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels =  "%b %Y",
    expand = c(0.01, 0.01)
  ) +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

# ---- Sales and AOV by Source ----

# NOTE: Look at AOV across different locations

monthly_AOV_locations <- transaction %>% 
  mutate(month = ordered_at %>% floor_date(unit = "month") %>% ymd()) %>% 
  group_by(month, location_name) %>% 
  summarize(
    sales = sum(price_charged),
    AOV = mean(price_charged)
  ) %>% 
  group_by(month) %>% 
  mutate(
    pct_of_sales = sales / sum(sales) * 100
  )

# Sales
monthly_AOV_locations %>% 
  ggplot(aes(month, sales, color = location_name)) +
  geom_line(size = 1) +
  geom_smooth(se = FALSE, size = 2) +
  ylim(0000, 50000) +
  labs(
    title = "Sales by source",
    y = "Sales ($)"
  ) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels =  "%b %Y",
    expand = c(0.01, 0.01)
  ) +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

# Sales Pct
monthly_AOV_locations %>% 
  ggplot(aes(month, pct_of_sales, fill = location_name)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Sales by source, % of total",
    y = "Percent of Total Sales"
  ) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels =  "%b %Y",
    expand = c(0.01, 0.01)
  ) +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

# AOV: compare to number of items
monthly_AOV_locations %>% 
  ggplot(aes(month, AOV, color = location_name)) +
  geom_line(size = 1) +
  geom_smooth(method = "lm", se = FALSE, linetype = 1, size = 2) +
  labs(
    title = "AOV by source",
    y = "Average Order Value (AOV) ($)"
  ) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels =  "%b %Y",
    expand = c(0.01, 0.01)
  ) +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title.x = element_blank(),
    legend.position = "top",
    legend.title = element_blank()
  )

# ---- First-purchase profitability ----

# NOTE: This requires COGS and other transaction-level costs to determine
# profit. Will try with discounts instead which does not work well.

# 1) 
monthly_first_purchase_profit <- transaction_east %>% 
  group_by(customer_id) %>% 
  mutate(
    visit_num = row_number(),
    profit = price_charged - discount_given
  ) %>% 
  ungroup() %>% 
  filter(visit_num == 1) %>% 
  group_by(month) %>% 
  summarize(
    "Avg Profit" = mean(profit)
  ) %>% 
  inner_join(monthly_CAC) %>% 
  mutate(
    "Avg Init Purchase Profit" = `Avg Profit` - CAC
  )

monthly_first_purchase_profit %>% 
  pivot_longer(
    cols = c(`Avg Profit`, CAC),
    names_to = "metric"
  ) %>% 
  ggplot(aes(month, value, color = metric)) +
  geom_line() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 90)
  ) +
  labs(
    title = "Average Initial Purchase Profit vs CAC"
  ) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels =  "%b %Y",
    expand = c(0.01, 0.01)
  )

monthly_first_purchase_profit %>% 
  pivot_longer(
    cols = c(`Avg Init Purchase Profit`, CAC),
    names_to = "metric"
  ) %>% 
  ggplot(aes(month, value, color = metric)) +
  geom_line() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 90)
  ) +
  labs(
    title = "Average Initial Purchase Profit vs CAC"
  ) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels =  "%b %Y",
    expand = c(0.01, 0.01)
  )

# 2)
monthly_first_purchase_profit_cohort <- transaction_east %>% 
  group_by(customer_id) %>% 
  mutate(
    month_acquired = first(month),
    profit = price_charged - discount_given
  ) %>% 
  ungroup() %>% 
  filter(month == month_acquired) %>% 
  group_by(month) %>% 
  summarize(
    "Avg Profit" = mean(profit)
  ) %>% 
  inner_join(monthly_CAC) %>% 
  mutate(
    "Avg Init Purchase Profit" = `Avg Profit` - CAC
  )

monthly_first_purchase_profit_cohort %>% 
  pivot_longer(
    cols = c(`Avg Profit`, CAC),
    names_to = "metric"
  ) %>% 
  ggplot(aes(month, value, color = metric)) +
  geom_line() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 90)
  ) +
  labs(
    title = "Average Initial Purchase Profit vs CAC"
  ) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels =  "%b %Y",
    expand = c(0.01, 0.01)
  )

monthly_first_purchase_profit_cohort %>% 
  pivot_longer(
    cols = c(`Avg Init Purchase Profit`, CAC),
    names_to = "metric"
  ) %>% 
  ggplot(aes(month, value, color = metric)) +
  geom_line() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 90)
  ) +
  labs(
    title = "Average Initial Purchase Profit vs CAC"
  ) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels =  "%b %Y",
    expand = c(0.01, 0.01)
  )

# ---- Cohorted sales (the “C3”) ----

c3 <- function(measure_freq, acquisition_freq = "yearly") {
  cohorts <- transaction_east %>% 
    group_by(customer_id) %>% 
    mutate(
      acquired = floor_date(min(ordered_at), acquisition_freq)
    ) %>% 
    ungroup() %>% 
    group_by(acquired) %>% 
    mutate(
      revenue = cumsum(price_charged)
    ) %>% 
    ungroup() %>% 
    group_by({{measure_freq}}) %>% 
    distinct(acquired, .keep_all = TRUE) %>% 
    arrange({{measure_freq}}, acquired) %>% 
    mutate(
      ARR = cumsum(revenue),
      lag_arr = if_else(is.na(lag(ARR)), 0, lag(ARR))
    ) %>% 
    ungroup()
}

c3 <- c3(measure_freq = year, acquisition_freq = "yearly")

c3 %>% 
  ggplot(aes(month)) + 
  geom_ribbon(
    aes(ymin = lag_arr, ymax = ARR, fill = factor(year(acquired)))
  ) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90)
  ) +
  labs(
    title = "Annual Recurring Revenue",
    subtitle = "By Annual Cohort",
    y = "Cumulative Revenue ($)"
  ) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels =  "%b %Y",
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    labels = scales::dollar_format()
  )

# revenue retention
# quarter-level

# Non-cumulative (for month)
c3 %>% 
  group_by(acquired) %>% 
  mutate(
    months = time_length(
      interval(acquired, month),
      unit = "month"
    ) %>% ceiling(),
    months = months - min(months)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(months, revenue, color = factor(year(acquired)))) + 
  geom_line() +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  ) +
  labs(
    title = "Cohort Revenue Over Time",
    y = "Revenue ($)"
  ) +
  scale_y_continuous(
    labels = scales::dollar_format()
  )

# Stacked (remove extra)
monthly_cohort_sales <- transaction_east %>% 
  group_by(customer_id) %>% 
  mutate(
    acquired = floor_date(min(ordered_at), "yearly")
  ) %>% 
  ungroup() %>% 
  group_by(acquired, month) %>% 
  summarize(
    sales = sum(price_charged)
  )

monthly_cohort_sales %>% 
  ggplot() +
  geom_area(
    aes(month, sales, fill = factor(year(acquired))),
    position = position_stack(reverse = TRUE)
  )  +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90)
  ) +
  labs(
    title = "Monthly Sales by Cohort",
    y = "Revenue ($)"
  ) +
  scale_x_date(
    date_breaks = "1 month", 
    date_labels =  "%b %Y",
    expand = c(0.01, 0.01)
  ) +
  # geom_line(data = monthly_sales, aes(x = month, y = sales),
  #           linetype = 2) +
  scale_y_continuous(
    labels = scales::dollar_format()
  )

annual_cohort_sales <- transaction_east %>% 
  group_by(customer_id) %>% 
  mutate(
    acquired = floor_date(min(ordered_at), "yearly")
  ) %>% 
  ungroup() %>% 
  group_by(acquired, year) %>% 
  summarize(
    sales = sum(price_charged)
  )

annual_cohort_sales %>% 
  ggplot() +
  geom_bar(
    aes(year, sales, fill = factor(year(acquired))),
    stat = "identity"
  )  +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90)
  ) +
  labs(
    title = "Monthly Sales by Cohort",
    y = "Revenue ($)"
  ) +
  scale_y_continuous(
    labels = scales::dollar_format()
  )

