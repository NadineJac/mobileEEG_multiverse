# data analysis summary at beginning of results section
#details on autor and computing environment here

# ADAPT: directory of your Database.xlsx file
PATH <- "C:\\Users\\alfi7048\\Documents\\Psychologie\\otto_projects\\naj_meteor\\mobileEEG\\PreprocessingMobileP3Gait\\data\\"
setwd(PATH)
# libraries
library(readxl)
library(stringr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Load data ---------------------------------------------------------------
df_steps <- read_excel("Database.xlsx", sheet = "Coding_steps")
df_opts <- read_excel("Database.xlsx", sheet = "Coding_options")

# Number of processing steps ----------------------------------------------
# the articles ID and each step are a nonzero element
# count all non-zero elements and subtract one
num_steps <- (rowSums(df_steps != 0)-1) # 
summary(num_steps)      # summary of number of steps per paper (range, mean)
sd(num_steps)           # SD


# Number of manual processing steps in articles ---------------------------
# counting the occurrences of "manual"

df_manual <- df_steps
df_manual = subset(df_manual, select = -c(Key)) # delete "Key" column
df_opts = subset(df_opts, select = -c(Key)) # delete "Key" column

for (i in colnames(df_manual)){
  #df_manual[[i]]<- str_count(df_manual[[i]], "Visual inspection")
  df_manual[[i]]<- grepl("Visual inspection", df_manual[[i]], fixed = T)
}
for (i in colnames(df_opts)){
  df_opts[[i]]<- grepl("manual", df_opts[[i]], fixed = T)
}
num_manual <- rowSums(df_manual) + rowSums(df_opts) # add number of manual steps per article
sum(num_manual!= 0)     # number of articles using any manual processing step
(sum(num_manual!= 0)/length(num_manual))*100# percentage of articles using any manual processing step
summary(num_manual)     # summary of number of manual steps per paper (range, mean)
sd(num_manual)          # SD

# Number of not reported details per article ------------------------------
# load and prepare data
dat_op_or <- read_excel("Database.xlsx", sheet = "Coding_options_or", na = "NA") # load data
dat_op_or[dat_op_or == ""] <- NA
dat <- read_excel("Database.xlsx", sheet = "Coding_steps")

for (opc in 2:ncol(dat_op_or)){ # loop through all coding steps to identify for which no option was reported
  for (opr in 1:nrow(dat_op_or)){
    opt <- dat_op_or[opr,opc]
    if (is.na(opt)){
      pap <- dat[opr,]
      if (colnames(dat_op_or)[opc] %in% pap){
        dat_op_or[opr,opc] <- "Not_reported"
      }
      else{
        dat_op_or[opr,opc] <- "Not_used"
      }
    }
  }
} 

# count instances of "Not_reported"
df_NA <- dat_op_or
df_NA = subset(df_NA, select = -c(Key)) # delete "Key" column
for (i in colnames(df_NA)){
  df_NA[[i]]<- str_count(df_NA[[i]], "Not_reported")
}
num_NA <- rowSums(df_NA) # add number of NA per article
sum(num_NA!= 0)     # number of articles w/ any NA
(sum(num_NA != 0)/length(num_NA))*100# percentage of articles  w/ any NA
summary(num_NA)     # summary of number of NA per paper (range, mean)
sd(num_NA)          # SD

# OS practice: plot static ------------------------------------------------
dat <- read_excel("Database.xlsx", sheet = "OpenScholarship", na = "NA")
# add "Not reported" to all empty fields
dat <- dat %>%
  mutate_all(~ifelse(is.na(.), "Not reported", .))

#offer dropdown for dynamic table
OS_practice <- c(rep("open code" , 4) , rep("open data" , 4) , rep("open materials" , 4) ) #get from all options?
condition <- rep(c("yes" , "yes, upon request" , "no, but intended", "no") , 3)
value <-rep(0,12)
df_OS <- data.frame(OS_practice,condition,value)

# count
shared <- list("yes" , "upon request" , "intended, but no", "no")
counter=0
for (si in shared) {
  counter <- counter+1
  df_OS$value[counter] <- (sum(dat$open_code == si))
  df_OS$value[4+counter] <- (sum(dat$open_data == si))
  df_OS$value[8+counter] <- (sum(dat$open_materials == si))
}

# my_col <- brewer.pal(4,"RdYlGn") # switch to flat colors
my_col <- brewer.pal(4,"Greys") # switch to greyscale

ggplot(df_OS, aes(fill=condition, y=value, x=OS_practice)) + 
  geom_bar(position="stack", stat="identity", color = "black") +
  scale_fill_manual(values = my_col) +
  labs(x = "Open scholarship practice",
       y = "Number of articles",
       fill = "Use") +
  theme(text = element_text(size = 14),
        legend.position = "bottom")
ggsave(file.path("results","OS_practices2.jpg"), dpi = 300, height = 6, width = 6)

