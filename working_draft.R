ppp_loans <- readr::read_csv("./ppp_loans_2022.csv")

sum(ppp_loans$CurrentApprovalAmount)

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
            aes(x = PAYROLL_PROCEED,
                y = InitialApprovalAmount) )

l1+geom_point()+geom_smooth(method="lm")

g1 <- ggplot(data = ppp_loans, aes(x = InitialApprovalAmount))
g1 + geom_histogram(binwidth = 250000)

summary(va_loans$InitialApprovalAmount)