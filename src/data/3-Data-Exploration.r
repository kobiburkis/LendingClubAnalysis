
##Importing required packages 
if(!require(DBI)){install.packages("DBI"); require(DBI)}
if(!require(RSQLite)){install.packages("RSQLite"); require(RSQLite)}
if(!require(dplyr)){install.packages("dplyr"); require(dplyr)}
if(!require(readxl)){install.packages("readxl"); require(readxl)}
if(!require(tableone)){install.packages("tableone"); require(tableone)}
if(!require(funModeling)){install.packages("funModeling"); require(funModeling)}
if(!require(openintro)){install.packages("openintro"); require(openintro)}

##############################################################################
##########                DATABASE FUNCTIONS                     #############
##############################################################################
            

#### Read function to import data from the SQL to a pandas dataframe.
readSQL <- function(query, db=DB_FILE) {
    require(DBI)
    require(RSQLite)
    con <- dbConnect(SQLite(), DB_FILE)
    df <- dbGetQuery(con, query)
    return(df)
}

#### Write a pandas dataframe into an SQL table. Use overwrite=True if you want to delete 
#### first a pre-existent table with the same name. Use append=True if you want to append
#### the data in the dataframe to a pre-existent table.
writeSQL <- function(df,tablename,overwrite=FALSE, append=FALSE,db=DB_FILE) {
    require(DBI)
    require(RSQLite)
    con <- dbConnect(SQLite(), DB_FILE)
    dbWriteTable(con,tablename,df,overwrite,append)
}
####

DB_FILE = paste0(getwd(),"/Data/loans.db") 

loans = readSQL("SELECT * FROM v_loans")

#make a copy - for restore if necessary 
loansdata <- loans

dim(loans)

# Load the Excel workbook
path <- getwd()
excel_file <- paste0("/lcdatadictionary.xlsx")
# see available tabs
excel_sheets(paste0(path, excel_file))
meta_loan_stats <- read_excel(paste0(path, excel_file), sheet = "LoanStats")

meta_loan_stats[,1:2]

summary(loans)

split(names(loans),sapply(loans, function(x) paste(class(x), collapse=" ")))

## numeric variables
num_vars <- 
  loans %>% 
  sapply(is.numeric) %>% 
  which() %>% 
  names()
num_vars
length(num_vars)

chr_to_date_vars <- 
  c("issue_d", "last_pymnt_d", "last_credit_pull_d",
    "next_pymnt_d", "earliest_cr_line")

loans %>%
  select_(.dots = chr_to_date_vars) %>%
  str()

##convert-date as string to Date Variable
convert_date <- function(x){
  as.Date(paste0("01-", x), format = "%d-%b-%Y")
  } 
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
loans <-
  loans %>%
  mutate_at(.funs = funs(convert_date), .vars = chr_to_date_vars)
Sys.setlocale("LC_TIME", lct)

##check if the conversion was successfuly done
loans %>%
  select_(.dots = chr_to_date_vars) %>%
  str()

## categorical variables
cv <- setdiff(names(loans),c(chr_to_date_vars,num_vars))
cv

loans %>%
  select_(.dots = cv) %>%
  str()

meta_loans <- funModeling::df_status(loans, print_results = FALSE)

meta_loans%>%
  knitr::kable()

meta_loans <-
  meta_loans %>%
  mutate(uniq_rat = unique / nrow(loans))

meta_loans %>%
  select(variable, unique, uniq_rat) %>%
  mutate(unique = unique, uniq_rat = scales::percent(uniq_rat))

meta_loans %>%
  select(variable, p_zeros, p_na, unique, uniq_rat) %>%
  mutate(unique = unique, uniq_rat = scales::percent(uniq_rat))%>% filter_(~ variable %in% num_vars)%>% arrange(desc(unique))

excludeColumns <- function(data,colv) {
    for(n in colv) {
        data[[n]] <- NULL
    }
    return(data)
}
exclude <- c("member_id","url","emp_title","policy_code")
ncol(loans)
loans <- excludeColumns(loans,exclude)
ncol(loans)

excludeUniqueValue <- function(data) {
    nm <- names(data)
    res <- NULL
    for(n in nm) {
        if(length(unique(data[[n]]))==1) {
            data[[n]] <- NULL
            res <- c(res, n)
            print(n)
        }
    }
    if(length(res) > 0) {
        message("The following variables had only one unique values and were removed")
        message(res)
    }
    return(data)
}

meta_loans %>%
  select(variable, p_zeros, p_na, unique) %>% arrange(desc(p_na))

applications loans %>% group_by(application_type) %>% summarise(N = n())%>%select(application_type,N)%>%mutate(app_rat = N / nrow(loans))

loans <- loans %>% filter(application_type == 0)
exclude <- c("verification_status_joint","annual_inc_joint","dti_joint","application_type")
ncol(loans)
loans <- excludeColumns(loans,exclude)
ncol(loans)
nrow(loans)


exclude <- c("grade","sub_grade","int_rate")
ncol(loans)
loans <- excludeColumns(loans,exclude)
ncol(loans)

#loans<-copy1

loans <-
  loans %>% mutate(loan_installment = ifelse(term == 0,round(loan_amnt/36,2) , round(loan_amnt/60,2)))

loans <- loans %>% select(-one_of("installment"))

summary(loans %>% select(one_of("loan_installment")))

loans %>%
  group_by(loan_status) %>%
  summarise(count = n(), rel_count = count/nrow(loans))

loan_status <- ggplot(loans, aes(loan_status, loan_amnt))
loan_status + geom_boxplot(aes(fill = loan_status)) +
  theme(axis.text.x = element_blank()) +
  labs(list(
    title = "Loan amount by status",
    x = "Status",
    y = "Amount"))  

nrow(loans)
status<-c("Current", "Default","In Grace Period", "Issued", "Late (16-30 days)",
          "Late (16-30 days)", "Late (31-120 days)"  ) 


loans <-
loans %>% filter(!(loan_status %in% status))
nrow(loans)

Charged_Off <- 
  c( "Charged Off",
    "Does not meet the credit policy. Status:Charged Off")


loans <-
  loans %>%
  mutate(default = ifelse(!(loan_status %in% Charged_Off), FALSE, TRUE))


loans <- loans %>% select(-one_of("loan_status"))

exclude <- c("last_pymnt_d","next_pymnt_d","collection_recovery_fee","last_pymnt_amnt",
             "out_prncp","out_prncp_inv","recoveries","total_pymnt",
             "total_pymnt_inv","total_rec_int","total_rec_late_fee","total_rec_prncp"           
            )
meta_loan_stats[,1:2] %>% filter(LoanStatNew %in% exclude)


ncol(loans)
loans <- excludeColumns(loans,exclude)
ncol(loans)

split(names(loans),sapply(loans, function(x) paste(class(x), collapse=" ")))

#Recreate Meta Loans
meta_loans <- funModeling::df_status(loans, print_results = FALSE)
meta_loans <-
  meta_loans %>%
  mutate(uniq_rat = unique / nrow(loans)) %>%
  mutate(unique = unique, uniq_rat = scales::percent(uniq_rat))

cv <- c('verification_status','loanDesc', 'purpose' ,'title','emp_length','zip_code', 'addr_state', 'home_ownership')
meta_loans %>%
  select(variable, p_zeros, p_na, unique,uniq_rat) %>% arrange(desc(p_na))  %>% filter_(~ variable %in% cv)

exclude <- c("loanDesc","title")
ncol(loans)
loans <- excludeColumns(loans,exclude)
ncol(loans)

meta_loans %>%
  select(variable, p_zeros, p_na, unique,uniq_rat) %>% arrange(unique)

cv <- c('verification_status', 'purpose' ,'emp_length', 'home_ownership')
tab <- CreateTableOne(vars = cv, data = loans)

summary(tab)

loans$verification_status <-as.factor(loans$verification_status)
loans$purpose <-as.factor(loans$purpose)
loans$emp_length <-as.factor(loans$emp_length)
loans$home_ownership <-as.factor(loans$home_ownership)
loans$term <-as.factor(loans$term)

head(loans %>% select(zip_code,addr_state))

copy1 <- loans

loans <- loans %>% select(-one_of("zip_code"))
loans <- loans %>%select(everything())%>%mutate(full_state= abbr2state(loans$addr_state)) 
head(loans %>% select(addr_state,full_state))

##LoansBeforeEncoding
loansBE <- loans
writeSQL(loansBE, "loans_beforeHotEncoding")
##loansBE = readSQL("SELECT * FROM loans_beforeDataSet")

## one-hot-encoding...
one_hot_encoding <- function(data,id,category,prefix) {
    require(dplyr)
    cat <- levels(factor(data[[category]]))
    res <- data %>% group_by_(id) %>% select_(id) %>% distinct_()
    for (l in 1:length(cat)) {
        #cname <- paste(prefix,l,sep="_")
        #data[[cname]] <- ifelse(data[[category]]==cat[l],1,0)
        #cc1 <- data %>% group_by_(id) %>% summarise_(cname=max(cname)) %>% select_(id,cname)
        #res <- cbind(res, cc1[,2])
        cname <- paste(prefix,l,sep="_")
        ncat <- "newcat"
        data[[ncat]] <- ifelse(data[[category]]==cat[l],1,0)
        cc1 <- data %>% select_(id,ncat) %>% group_by_(id) %>% 
               summarise(newcat=max(newcat))
        res[[cname]] <- cc1$newcat
    }
    return(res)
}

loans <- loansBE
nrow(loans)
head(loans)

head(one_hot_encoding(data=loans,id="loan_id",category="verification_status",prefix="verif"))


loans <- inner_join(loans,one_hot_encoding(data=loans,id="loan_id",category="verification_status",prefix="verif"))

nrow(loans)
head(loans)

head(one_hot_encoding(data=loans,id="loan_id",category="purpose",prefix="purpose"))


loans <- inner_join(loans,one_hot_encoding(data=loans,id="loan_id",category="purpose",prefix="purpose"))

nrow(loans)
head(loans)

head(one_hot_encoding(data=loans,id="loan_id",category="emp_length",prefix="seniority"))


loans <- inner_join(loans,one_hot_encoding(data=loans,id="loan_id",category="emp_length",prefix="seniority"))

nrow(loans)
head(loans)

head(one_hot_encoding(data=loans,id="loan_id",category="home_ownership",prefix="owner_status"))

loans <- inner_join(loans,one_hot_encoding(data=loans,id="loan_id",category="home_ownership",prefix="owner_status"))

nrow(loans)
head(loans)


cv <- c('verification_status', 'purpose' ,'emp_length', 'home_ownership')
loans <- excludeColumns(loans,cv)

funModeling::df_status(loans, print_results = TRUE)

num_vars <- 
  loans %>% 
  sapply(is.numeric) %>% 
  which() %>% 
  names()

num_vars

tab <- CreateTableOne(vars = num_vars, data = loans)
summary(tab)

##SQLLite Converts the date rto numeric so imlilictly apply as.character on date columns
loans$issue_d <- as.character(loans$issue_d)
loans$earliest_cr_line <- as.character(loans$earliest_cr_line)
loans$last_credit_pull_d <- as.character(loans$last_credit_pull_d)


##saving the file ,for future work!

write.csv(loans, file = "loans_db.csv",row.names=FALSE)
writeSQL(loans, "loans_dataset")

loans = readSQL("SELECT * FROM loans_dataset")

###Get Tomas Library
if(!require(devtools)){install.packages("devtools"); require(devtools)}
if(!require(mechkar)){install_github("karpatit/mechkar"); require(mechkar)}

Table1(data = loans,x = num_vars,y = "default")
