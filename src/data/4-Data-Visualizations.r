
if(!require(dplyr)) {install.packages("dplyr");require(dplyr)}
if(!require(ggplot2)) {install.packages("ggplot2");require(ggplot2)}
if(!require(gridExtra)) {install.packages("gridExtra");require(gridExtra)}
if(!require(choroplethr)) {install.packages("choroplethr");require(choroplethr)}
if(!require(choroplethrMaps)) {install.packages("choroplethrMaps");require(choroplethrMaps)}
if(!require(DBI)){install.packages("DBI"); require(DBI)}
if(!require(RSQLite)){install.packages("RSQLite"); require(RSQLite)}
if(!require(funModeling)){install.packages("funModeling"); require(funModeling)}
if(!require(tidyr)){install.packages("tidyr"); require(tidyr)}






readSQL <- function(query, db=DB_FILE) {
    require(DBI)
    require(RSQLite)
    con <- dbConnect(SQLite(), DB_FILE)
    df <- dbGetQuery(con, query)
    return(df)
}



### load data
DB_FILE = paste0(getwd(),"/Data/loans.db") 
loans = readSQL("SELECT * FROM loans_dataset")

poverty_rate_ds <- readSQL("select * from povertyRateByStates")

copy1<-loans
split(names(loans),sapply(loans, function(x) paste(class(x), collapse=" ")))

loans<-copy1

chr_to_date_vars <- 
  c("issue_d", "earliest_cr_line","last_credit_pull_d")
convert_date <- function(x){
  as.Date(x)
  } 
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
loans <-
  loans %>%
  mutate_at(.funs = funs(convert_date), .vars = chr_to_date_vars)
Sys.setlocale("LC_TIME", lct)

options(scipen = 999)


amnt_df <- loans %>% 
  select(issue_d, loan_amnt) %>% 
  group_by(issue_d) %>% 
  summarise(Amount = sum(loan_amnt),Count = n())

ts_amnt <- ggplot(amnt_df, 
                  aes(x = issue_d, y = Amount))
ts_amnt + geom_line() + xlab("Date issued")
ts_amnt <- ggplot(amnt_df, 
                  aes(x = issue_d, y = Count))
ts_amnt + geom_line() + xlab("Date issued")

time_df <- loans %>% 
  select(issue_d, loan_amnt)
head(time_df)

time_df$issue_month<-substr(time_df$issue_d,6,7)
time_df$issue_year<-substr(time_df$issue_d,1,4)

options(repr.plot.width=7, repr.plot.height=4)
time_df %>% group_by(issue_year,issue_month)%>%
summarise(lcnt=n())%>%
ggplot(aes(x=issue_month,y=lcnt,group=1,col=issue_month))+
geom_line()+
facet_wrap(~issue_year)+
geom_point()+
theme(legend.position="")+
labs(x="Loan Processed Month",y="Count",title="Loans processed in each month/Year")


options(repr.plot.width=5, repr.plot.height=4,scipen=10000)
time_df %>% group_by(issue_year)%>%
summarise(lycnt=n())%>%
ggplot(aes(x=issue_year,y=lycnt,group=1,col=issue_year))+
geom_line(size=1.5)+
geom_point(size=2)+
theme(legend.position="")+
labs(x="Year",y="Count",title="Loans processed in each Year")

options(repr.plot.width=5, repr.plot.height=4,scipen=999)
time_df %>% group_by(issue_year)%>%
summarise(lycnt=sum(loan_amnt))%>%
ggplot(aes(x=issue_year,y=lycnt,group=1,col=issue_year))+
geom_line(size=1.5)+
geom_point(size=2)+
theme(legend.position="")+
scale_y_log10()+
labs(x="Year",y="Amount",title="Loans processed in each Year")

state_by_value <-
loans %>%rename(region = full_state) %>%mutate(region=tolower(region))%>% group_by(region) %>%
  summarise(value = sum(loan_amnt, na.rm=TRUE))

state_choropleth(state_by_value, title = "Value by State")

state_by_volume <-
loans %>%rename(region = full_state) %>%mutate(region=tolower(region))%>% group_by(region) %>%
  summarise(value = n())

state_choropleth(state_by_volume, title = "Count by State")

head(state_by_volume%>% arrange(desc(value)),10)

head(state_by_volume%>% arrange(value),10)

amnt_df <- loans %>% 
  select(issue_d, loan_amnt,full_state)%>% mutate(full_state=tolower(full_state))%>% mutate(issue_d=format(as.Date(issue_d, format="%d/%m/%Y"),"%Y")) %>%
  group_by(issue_d,full_state) %>% 
  summarise(Count = n())

poverty_rateByYears<- poverty_rate_ds%>%mutate(state_name=tolower(state_name))%>% select(year,poverty_rate_p,state_name)

amnt_df <- merge(amnt_df, poverty_rateByYears, by.x = c("issue_d","full_state"), by.y = c("year","state_name"))

plotPovertyLoansByCountry <- function(data,states){
   i <- 1
   glist <- list();
   for (state in states){
      df <- data %>%filter(full_state==state)
      g2 <- ggplot(df, aes(issue_d, Count)) +
               geom_point(data=df, aes(x=issue_d, y=Count,  size=poverty_rate_p))
      g2 <-g2 +labs(title=paste0("Loans Count Over PovertyRate in ",state))
      glist[[i]] <- g2
      i <- i + 1
    }
  return(glist)
} 

par(mfrow = c(1,2))
statesv <- c("california","new york","texas","north dakota","idaho","iowa")
plist<- plotPovertyLoansByCountry(amnt_df,statesv)
for (i in 1:length(plist))
    print(plist[[i]])

purposes_df <- loans %>% 
  select(purpose, loan_amnt) %>% 
  group_by(purpose) %>% 
  summarise(Amount = sum(loan_amnt),Count = n())

g <- ggplot(purposes_df, 
                  aes(x = purpose, y =Amount ))
g + geom_col() + xlab("Loan Purposes") + coord_flip()
g <- ggplot(purposes_df, 
                  aes(x = purpose, y = Count))
g + geom_col() + xlab("Loan Purposes")+ coord_flip()

loans %>% group_by(home_ownership) %>% dplyr::summarise(count=n()) %>% 
mutate(pct=count/sum(count))%>% 
  ggplot(aes(x = reorder(home_ownership, -pct), y = pct)) + geom_bar(stat = "identity", fill = "purple", aes(color = I('black')), size = 0.1) + 
xlab("Home Ownership") + ylab("Percent")

home_df <- loans %>% 
  select(home_ownership, loan_amnt,default) %>% 
  group_by(home_ownership,default) %>% 
  summarise(MeanL = mean(loan_amnt),Count = n())%>% 
select(home_ownership, MeanL,Count,default) 

home_df

home_df %>%
  ggplot(aes(home_ownership,MeanL)) +
  geom_col() +
  facet_wrap(~ default) +
  labs(title="Loan Amount by Home Ownership", x = "Home Ownership", y = "Loans Mean \n")

home_df %>%
  ggplot(aes(home_ownership,Count)) +
  geom_col() +
  facet_wrap(~ default) +
  labs(title="Loan Count by Home Ownership", x = "Home Ownership", y = "Loan Count \n")

emp_df <- loans %>% 
  select(emp_length, loan_amnt,default) %>% 
  group_by(emp_length,default) %>% 
  summarise(MeanL = mean(loan_amnt),Count = n())%>% 
select(emp_length, MeanL,Count,default) 

emp_df %>%
  ggplot(aes(emp_length,MeanL)) +
  geom_col() +
  facet_wrap(~ default) +
  labs(title="Loan Amount by emp_length", x = "emp_length", y = "Loans Mean \n") + coord_flip()

emp_df %>%
  ggplot(aes(emp_length,Count)) +
  geom_col() +
  facet_wrap(~ default) +
  labs(title="Loan Count by emp_length", x = "emp_length", y = "Loan Count \n")+ coord_flip()

emp_df <- loans %>%select(annual_inc,emp_length,loan_amnt)

emp_df <- emp_df%>%group_by(emp_length)%>%
summarise(avg_inc=mean(annual_inc),avg_loan=mean(loan_amnt))%>%
arrange(desc(avg_loan))%>%
gather(key=avg,measure,avg_inc,avg_loan)

emp_df%>%ggplot(aes(x=emp_length,y=measure,group=avg,fill=avg))+
geom_bar(stat="identity",position="dodge")+
scale_fill_manual(values=c("#732ded","#33c14b"))+
labs(title="Avg Income vs Avg Loan")


ver_df <- loans %>% 
  select(verification_status, loan_amnt,default) %>% 
  group_by(verification_status,default) %>% 
  summarise(MeanL = mean(loan_amnt),Count = n())%>% 
select(verification_status, MeanL,Count,default) 

ver_df

ver_df %>%
  ggplot(aes(verification_status,MeanL)) +
  geom_col() +
  facet_wrap(~ default) +
  labs(title="Loan Amount by verification_status", x = "verification_status", y = "Loans Mean \n")

ver_df %>%
  ggplot(aes(verification_status,Count)) +
  geom_col() +
  facet_wrap(~ default) +
  labs(title="Loan Count by verification_status", x = "verification_status", y = "Loan Count \n")

loans$term_fac<- factor(loans$term,labels=c("36 Months", "60 Months"))

options(repr.plot.width=5, repr.plot.height=3)
#i. Term
loans %>% group_by(term_fac) %>% dplyr::summarise(count=n()) %>% mutate(pct=count/sum(count))%>% 
  ggplot(aes(x = term_fac, y = pct)) + geom_bar(stat = "identity", fill = "purple", aes(color = I('black')), size = 0.1)+xlab("Term") + 
ylab("Percent")


term_df <- loans %>% 
  select(term_fac, loan_amnt,default) %>% 
  group_by(term_fac,default) %>% 
  summarise(MeanL = mean(loan_amnt),Count = n())%>% 
select(term_fac, MeanL,Count,default) 

term_df %>%
  ggplot(aes(term_fac,MeanL)) +
  geom_col() +
  facet_wrap(~ default) +
  labs(title="Loan Amount by Term", x = "Term", y = "Loans Mean \n")

term_df %>%
  ggplot(aes(term_fac,Count)) +
  geom_col() +
  facet_wrap(~ default) +
  labs(title="Loan Count by Term", x = "Term", y = "Loan Count \n")

histogram( ~ loan_amnt | term_fac, data=loans, xlab="Loan Amount", ylab="Percentage of total data points",layout=c(1,2))

loans$term_fac<-NULL

numeric_cols <- sapply(loans, is.numeric)

loansWitnNumeric <- loans[,numeric_cols]
split(names(loansWitnNumeric),sapply(loansWitnNumeric, function(x) paste(class(x), collapse=" ")))
length(loansWitnNumeric)

splitAndPlotVars <- function(data){
   if(!require(reshape2)) {install.packages("reshape2");require(reshape2)}
   glist <- list();
   g<-1
   i<-1
   while (i <ncol(data)){
      if ((i+5)>ncol(data))
       upper <-ncol(data)
      else
       upper <-i+5  
      df <- cbind(data[,c(i:upper)],default=loans[,'default'])
      df_f <- melt(df, id="default")
      p <- ggplot(aes(x = value, group = default, colour = factor(default)), 
            data = df_f) +
        # create the plot to check if there are any good variables that can be used in predictive models
        geom_density() + facet_wrap(~variable, scales="free")
      print(p)
     # glist[[g]] <- p
      i <- i + 6
      g <- g + 1
    }
  return(glist)
} 

splitAndPlotVars(loansWitnNumeric)

num_vars <- loans %>% sapply(is.numeric) %>% which() %>% names()
num_vars


loans %>%
  ggplot(aes(x = annual_inc, y = loan_amnt)) +
  geom_point()

options(repr.plot.width=6, repr.plot.height=4)
ggplot(filter(loans,annual_inc < 2500000), aes(x = annual_inc, y = dti, color = default)) + 
geom_point(alpha=0.5)

num_vars <- 
  loans %>% 
  sapply(is.numeric) %>% 
  which() %>% 
  names()



if(!require(corrplot)) {install.packages("corrplot");require(corrplot)}
corrplot::corrplot(cor(loans[, num_vars], use = "complete.obs"), 
                   method = "pie", type = "upper",tl.cex = 0.3)
