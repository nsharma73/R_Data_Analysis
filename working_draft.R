knitr::opts_chunk$set(collapse = TRUE)

library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(formattable)
library(gt)

#Load Data
ppp_loans <- readr::read_csv("./ppp_loans.csv")
dim(ppp_loans)
str(ppp_loans)

#delete columns that are not needed for analysis
ppp_loans = ppp_loans[-c(3,4,6,7,9,13,17,18,19,20,
               21,22,23,31,36,37,39,40,41,42,43,45,47,48)]

dim(ppp_loans)
str(ppp_loans)

#Convert Date Approved to Date datatype
ppp_loans$DateApproved <- as.Date(ppp_loans$DateApproved, "%m/%d/%Y")
class(ppp_loans$DateApproved)
unique(ppp_loans$DateApproved)
min(ppp_loans$DateApproved)
max(ppp_loans$DateApproved)

#Add Year-Month to trend data
ppp_loans$Month_Yr = strftime(ppp_loans$DateApproved,format="%Y-%m")
unique(ppp_loans$Month_Yr)

#examine na across columns
map(ppp_loans, ~sum(is.na(.)))

sum(is.na(ppp_loans$BorrowerName))
ppp_loans[is.na(ppp_loans$BorrowerName),]

sum(is.na(ppp_loans$BorrowerState))

ppp_loans = ppp_loans %>%
  mutate(BorrowerState = if_else(is.na(BorrowerState) , ProjectState, BorrowerState))

sum(is.na(ppp_loans$BorrowerState))

ppp_loans[is.na(ppp_loans$BorrowerState),]

ppp_loans = ppp_loans %>%
  mutate(BusinessType = if_else(is.na(BusinessType) , "Unknown", BusinessType))

sum(is.na(ppp_loans$BusinessType))

ppp_loans = ppp_loans %>%
  mutate(NonProfit = if_else(is.na(NonProfit) , "N", NonProfit))

sum(is.na(ppp_loans$NonProfit))

ppp_loans[is.na(ppp_loans$BusinessAgeDescription),]

ppp_loans = ppp_loans %>%
  mutate(BusinessAgeDescription = if_else(is.na(BusinessAgeDescription) , 
                                          "Unanswered", BusinessAgeDescription))

sum(is.na(ppp_loans$BusinessAgeDescription))

#Delete columns: ProjectCity, ProjectCountyName, ProjectState CD
ppp_loans = subset(ppp_loans, select = -c(ProjectCity,
                                          ProjectCountyName,ProjectState,CD))
dim(ppp_loans)
tail(ppp_loans)
#examine na across columns
map(ppp_loans, ~sum(is.na(.)))

ppp_loans = ppp_loans %>% 
  mutate(UndisbursedAmount = ifelse(is.na(UndisbursedAmount), 
                                    0, UndisbursedAmount))

ppp_loans = ppp_loans %>% 
  mutate(ForgivenessAmount = ifelse(is.na(ForgivenessAmount), 
                                    0, ForgivenessAmount))

ppp_loans = ppp_loans %>% 
  mutate(JobsReported = ifelse(is.na(JobsReported), 
                               0, JobsReported))


#add 2 digit nacis code and industry mapping
nacis <- readr::read_csv("./two_digit_naics_code.csv")

#extract 2 digit nacis codes from ppp loans data
ppp_loans = ppp_loans%>%
  mutate(NAICSCode2 = as.numeric(gsub("(^\\d{2}).*", "\\1", ppp_loans$NAICSCode)))

sort(unique(ppp_loans$NAICSCode2))

unique(nacis$NAICSCode2)

# using join to add Industry dimension 
ppp_loans = ppp_loans%>%left_join(nacis)

ppp_loans = ppp_loans %>% 
  mutate(Industry = ifelse(is.na(Industry), "Unknown", Industry))

unique(ppp_loans$Industry)
map(ppp_loans, ~sum(is.na(.)))

ppp_loans%>%
  group_by(BusinessType)%>%
  summarise(init_amt = sum(InitialApprovalAmount),
            curr_amt = sum(CurrentApprovalAmount),
            undis_amt = sum(UndisbursedAmount),
            forg_amt = sum(ForgivenessAmount)) 
  
#The UndisbursedAmount is small and we will ignore it for this analysis
ppp_loans = subset(ppp_loans, select = -c(UndisbursedAmount))

# boxplots by Industry type
p <- ggplot(ppp_loans, aes(x=Industry, y=CurrentApprovalAmount)) + 
  geom_boxplot() + coord_flip()
p

balance_summary1 = ppp_loans%>%
  group_by(BusinessType)%>%
  summarise(init_amt = (sum(InitialApprovalAmount)/1000000),
            curr_amt = (sum(CurrentApprovalAmount)/1000000),
            forg_amt = (sum(ForgivenessAmount)/1000000),
            jobs = sum(JobsReported),
            avg_term = mean(Term))%>%
  arrange(desc(curr_amt))


balance_summary1 = ppp_loans%>%
  group_by(Industry)%>%
  summarise(
            total_curr_amt = sum(CurrentApprovalAmount),
            avg_curr_amt = mean(CurrentApprovalAmount),
            med_curr_amt = median(CurrentApprovalAmount),
            sd_curr_amt = sd(CurrentApprovalAmount)
            )%>%
  arrange(desc(total_curr_amt))

gt_theme_538 <- function(data,...) {
  data %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    ) 
}

gt(data=balance_summary1)%>%
  gt_theme_538() %>% 
   tab_header(title = "PPP Loans by Industry")%>%
   fmt_currency(
     columns = c(total_curr_amt, avg_curr_amt, med_curr_amt,sd_curr_amt),
     currency = "USD",
     decimals = 0,
     suffixing = TRUE
   ) %>% cols_label(
     total_curr_amt = md("**Total Loan Amt**"),
     avg_curr_amt = md("**Average Loan Amt**"),
     med_curr_amt = md("**Median Loan Amt**"),
     sd_curr_amt = md("**Std. Dev of Loan Amt**")) %>%
  opt_align_table_header("left")

sum(ppp_loans$InitialApprovalAmount)
sum(ppp_loans$CurrentApprovalAmount)
sum(ppp_loans$ForgivenessAmount)

ggplot(ppp_loans, aes(x=log(CurrentApprovalAmount))) + 
  geom_histogram()

# boxplots by business type
p <- ggplot(ppp_loans, aes(x=BusinessType, y=log(CurrentApprovalAmount))) + 
  geom_boxplot() + coord_flip()
p

#We will use current approval amount, as it reflects the latest loan balances
ppp_loans%>%
  group_by(Month_Yr)%>%
  summarise(Total_Loans = sum(CurrentApprovalAmount))%>%
  ggplot() + geom_col(aes(x=Month_Yr, y=Total_Loans)) + 
  scale_y_log10()



ppp_loans%>%
  select(JobsReported, CurrentApprovalAmount)%>%
  ggplot() + geom_point(aes(x=JobsReported, y=CurrentApprovalAmount)) + 
  scale_y_log10()


va_loans = ppp_loans%>%
  filter(BorrowerState=='VA')%>%
  select(BorrowerName, 
         JobsReported,
         DateApproved, 
         InitialApprovalAmount, 
         CurrentApprovalAmount,
         PAYROLL_PROCEED,
         ForgivenessAmount
         )%>%
  arrange(InitialApprovalAmount)

tail(va_loans)

l1 <- ggplot(data = va_loans,
            aes(x = log(PAYROLL_PROCEED),
                y = log(CurrentApprovalAmount)) )

l1+geom_point()+geom_smooth(method="lm")

g1 <- ggplot(data = ppp_loans, aes(x = InitialApprovalAmount))
g1 + geom_histogram(binwidth = 250000)

summary(va_loans$InitialApprovalAmount)

