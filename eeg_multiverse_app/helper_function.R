# Developed in R version 4.2.1 (2022-06-23 ucrt) -- "Funny-Looking Kid"
# Daniel Kristanto (University of Oldenburg)
# adapted by Nadine Nadine Jacobsen (University of Oldenburg) and Cosku Inceler (University of Oldenburg)
# January 2024, last revision: 23-January-2024
library(shiny)        # v1.7.5
library(networkD3)    # v0.4
library(dplyr)        # v1.1.3
library(igraph)       # v1.5.1
library(visNetwork)   # v2.1.2
library(geomnet)      # v0.3.1, not on CRAN anymore, download from https://cran.r-project.org/src/contrib/Archive/geomnet/?C=M;O=A
library(stringr)      # v1.5.0
library(png)          # v0.1-8
library(shinyjs)      # v2.1.0
library(DT)           # v0.29
library(rintrojs)     # v0.3.2
library(ggplot2)      # v3.4.3
library(qdapTools)    # v2.4.6
library(RColorBrewer) # v1.1-3
library(forcats)      # v1.0.0
library(readxl)       # v1.4.3

rm(list = ls())
Sys.setlocale('LC_CTYPE','C')

#############################
#########ReadData############
#############################
dat <- read_excel("Database.xlsx", sheet = "Coding_steps")
dat_prep1 <- select(dat, contains("Step"))
dat_prep1[dat_prep1 == ""] <- NA 
steps <- read_excel("Database.xlsx", sheet = "All_steps", na = "NA")
dat_op_or <- read_excel("Database.xlsx", sheet = "Coding_options_or", na = "NA")
dat_op_or[dat_op_or == ""] <- NA
dat_op <- read_excel("Database.xlsx", sheet = "Coding_options", na = "NA")
dat_prep_op1 <- select(dat_op, contains("Choice"))
dat_prep_op1[dat_prep_op1 == ""] <- NA 
steps_op <- read_excel("Database.xlsx", sheet = "All_options")
p_inf <- read_excel("Database.xlsx", sheet = "Paper_info")
p_inf$DOI <- sprintf('<a href="https://doi.org/%s" target="_blank">%s</a>', p_inf$DOI, p_inf$DOI)
p_inf2 <- read_excel("Database.xlsx", sheet = "Paper_info2")
p_inf2$DOI <- sprintf('<a href="https://doi.org/%s" target="_blank">%s</a>', p_inf2$DOI, p_inf2$DOI)
as.integer(p_inf$Year)
cn_p_inf <- colnames(p_inf)
MetaData <- read_excel("Database.xlsx", sheet = "metadata")
MetaData <- MetaData %>%
  mutate_all(~ ifelse(. == "NA", "Not_reported", .))
ExperimentModels <- read_excel("Database.xlsx", sheet = "EEGAcquisition")
ExperimentModels <- ExperimentModels[c(1:32), 2]
ExperimentModels <- unique(ExperimentModels)
EEGAcq <- read_excel("Database.xlsx", sheet = "EEGAcquisition")
EEGAcq <- EEGAcq %>%
  mutate_all(~ ifelse(. == "NA", "Not_reported", .))
EEGAcq <- EEGAcq[-c(1:32),]
EEGAcq <- unique(EEGAcq)
SoftwareMeta <- read_excel("Database.xlsx", sheet = "EEGSoftware")
SoftwareMeta <- SoftwareMeta %>%
  mutate_all(~ ifelse(. == "NA", "Not_reported", .))
SoftwareMeta <- unique(SoftwareMeta)
OpenScienceMeta <- read_excel("Database.xlsx", sheet = "EEGOpenScience")
OpenScienceMeta <- OpenScienceMeta %>%
  mutate_all(~ ifelse(. == "NA", "Not_reported", .))
OpenScienceMeta <- unique(OpenScienceMeta)
ListSteps <- steps[, c(2,3,4)]
colnames(ListSteps) <- c("Step", "Group", "Definiton")
ListOptions <- steps_op[, c(1, 3, 4)]
colnames(ListOptions) <- c("Option", "Step", "Definition")
#dat with name visual
replace_names <- function(x) {
  mapping <- setNames(steps$Names_vis, steps$Names)
  return(mapping[x])
}
dat_vis <- data.frame(lapply(dat, replace_names))
dat_vis$Key <- dat$Key

replace_column_names <- function(x) {
  mapping <- setNames(steps$Names_vis, steps$Names)
  new_names <- sapply(names(x), function(col_name) {
    if (col_name %in% names(mapping)) {
      return(mapping[col_name])
    } else {
      return(col_name)
    }
  })
  names(x) <- new_names
  return(x)
}

dat_op_or_vis <- replace_column_names(dat_op_or)


#############################
####params visualization#####
#############################
all_clrs <- c(
  "#009E73", # Green 
  "#0072B2", # Blue 
  "#D55E00", # Orange 
  "#CC79A7", # Pink 
  "#56B4E9", # Light blue 
  "#FFD700", # Gold
  "#32CD32", # Lime Green
  "#1E90FF", # Dodger Blue
  "#FF69B4", # Hot Pink
  "#008B8B", # Dark Cyan
  "#FF4500", # Red-Orange
  "#9370DB", # Medium Purple
  "#A0522D", # Sienna
  "#006400", # Dark Green
  "#FF8C00", # Dark Orange
  "#9400D3", # Dark Violet
  "#00BFFF", # Deep Sky Blue
  "#FF6347", # Tomato
  "#8A2BE2", # Blue Violet
  "#3CB371", # Medium Sea Green
  "#800080", # Purple
  "#20B2AA", # Light Sea Green
  "#6A5ACD", # Slate Blue
  "#FFA07A", # Light Salmon
  "#48D1CC", # Medium Turquoise
  "#9932CC", # Dark Orchid
  "#ADFF2F", # Green Yellow
  "#B22222", # Fire Brick
  "#FF1493", # Deep Pink
  "#7FFF00" # Chartreuse
)
#############################
#########WholePipe###########
#############################
###Defining nodes
nodes <- steps
nodes$Groups.type <- as.factor(nodes$Groups)
nodes[sapply(nodes, is.factor)] <- data.matrix(nodes[sapply(nodes, is.factor)])
nodes$size <- 0
for (nd in 1:nrow(nodes)){
  nd_p <- nodes$Names[nd]
  row_nd <- which(apply(dat, 1, function(x2) all(nd_p %in% x2)))
  nodes$size[[nd]] <- length(row_nd)
}


###Defining links
links <- expand.grid(source = nodes$Names, target = nodes$Names)
links$value <- 0

for (pp in 1:nrow(links)){
  st_l <- as.character(c(links$source[pp], links$target[pp]))
  row_l <- which(apply(dat, 1, function(x1) {
    idx_l <- match(st_l, x1)
    all(!is.na(idx_l)) && all(diff(idx_l) == 1)
  }))
  links$value[[pp]] <- length(row_l)
}

###Graph visualization
nodes2 <- nodes
links2 <- links

#########################
#########STEPS###########
#########################
###Listing paper
list_df <- list()
for (p in 1:length(dat$Key)){
  ppr <- dat_prep1[p, ]
  dat_prep <- ppr
  source <- c(dat_prep$Step1, dat_prep$Step2, dat_prep$Step3, dat_prep$Step4, dat_prep$Step5,
              dat_prep$Step6, dat_prep$Step7, dat_prep$Step8, dat_prep$Step9, dat_prep$Step10,
              dat_prep$Step11, dat_prep$Step12, dat_prep$Step13, dat_prep$Step14, dat_prep$Step15)
  target <- c(dat_prep$Step2, dat_prep$Step3, dat_prep$Step4, dat_prep$Step5,
              dat_prep$Step6, dat_prep$Step7, dat_prep$Step8, dat_prep$Step9, dat_prep$Step10,
              dat_prep$Step11, dat_prep$Step12, dat_prep$Step13, dat_prep$Step14, dat_prep$Step15,
              dat_prep$Step16)
  dat2 <- data.frame(table(source, target))
  value <- c()
  for (i in 1:length(source)){
    s <- source[i]
    if (is.na(s)){
      value[i] <- 0
    }
    else {
      t <- target[i]
      if (is.na(t)){
        value[i] <- 0
      }
      else {
        ind <- which(dat2$source %in% s & dat2$target %in% t)
        value[i] <- dat2$Freq[ind]
      }
    }
  }
  
  links <- data.frame(source = source,
                      target = target,
                      value  = value)
  links <- na.omit(links)
  links <- unique(links)
  
  nodes <- steps
  nodes$Groups.type <- as.factor(nodes$Groups)
  nodes[sapply(nodes, is.factor)] <- data.matrix(nodes[sapply(nodes, is.factor)])
  
  links$IDsource <- match(links$source, nodes$Names) - 1
  links$IDtarget <- match(links$target, nodes$Names) - 1
  links <- na.omit(links)
  list_df[[p]] <- links
}



nodes <- steps
nodes$ID <- c(1:nrow(steps))
nodes$Groups.type <- as.factor(nodes$Groups)
nodes[sapply(nodes, is.factor)] <- data.matrix(nodes[sapply(nodes, is.factor)])
clrs <- all_clrs[1:length(unique(nodes$Groups))]
nodes$col <- clrs[nodes$Groups.type]
nodes$ID <- c(1:nrow(steps))

###########################
#########CHOICES###########
###########################
for (opc in 2:ncol(dat_op_or)){
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

cn_dat_op <- colnames(dat_op_or)


###Listing paper
list_df_op <- list()
for (p in 1:length(dat_op$Key)){
  ppr <- dat_prep_op1[p, ]
  dat_prep_op <- ppr
  source_op <- c(dat_prep_op$Choice1, dat_prep_op$Choice2, dat_prep_op$Choice3, dat_prep_op$Choice4, dat_prep_op$Choice5,
                 dat_prep_op$Choice6, dat_prep_op$Choice7, dat_prep_op$Choice8, dat_prep_op$Choice9, dat_prep_op$Choice10,
                 dat_prep_op$Choice11, dat_prep_op$Choice12, dat_prep_op$Choice13, dat_prep_op$Choice14, dat_prep_op$Choice15) #num choices -1
  target_op <- c(dat_prep_op$Choice2, dat_prep_op$Choice3, dat_prep_op$Choice4, dat_prep_op$Choice5,
                 dat_prep_op$Choice6, dat_prep_op$Choice7, dat_prep_op$Choice8, dat_prep_op$Choice9, dat_prep_op$Choice10, dat_prep_op$Choice11,
                 dat_prep_op$Choice12, dat_prep_op$Choice13, dat_prep_op$Choice14, dat_prep_op$Choice15, dat_prep_op$Choice16)#num choices
    dat2_op <- data.frame(table(source_op, target_op))
  value_op <- c()
  for (i in 1:length(source_op)){
    s <- source_op[i]
    if (is.na(s)){
      value_op[i] <- 0
    }
    else {
      t <- target_op[i]
      if (is.na(t)){
        value_op[i] <- 0
      }
      else {
        ind <- which(dat2_op$source_op %in% s & dat2_op$target_op %in% t)
        value_op[i] <- dat2_op$Freq[ind]
      }
    }
  }
  
  links_op <- data.frame(source = source_op,
                         target = target_op,
                         value  = value_op)
  links_op <- na.omit(links_op)
  links_op <- unique(links_op)
  
  nodes_op <- steps_op
  nodes_op$Groups.type <- as.factor(nodes_op$Groups)
  nodes_op[sapply(nodes_op, is.factor)] <- data.matrix(nodes_op[sapply(nodes_op, is.factor)])
  
  links_op$IDsource <- match(links_op$source, nodes_op$Names) - 1
  links_op$IDtarget <- match(links_op$target, nodes_op$Names) - 1
  links_op <- na.omit(links_op)
  list_df_op[[p]] <- links_op
}

nodes_op <- steps_op
nodes_op$Groups.type <- as.factor(nodes_op$Groups)
nodes_op[sapply(nodes_op, is.factor)] <- data.matrix(nodes_op[sapply(nodes_op, is.factor)])
clrs_op <- all_clrs[1:length(unique(nodes_op$Groups))]
####################################
#########Further Analyses###########
####################################
###Yes or no
st <- nodes[ ,"Names"]
st <- data.frame(st)
colnames(st) <- "Names"
# mat_yn <- matrix(0, nrow = length(st$Names), ncol = length(st$Names))
# for (i in 1:length(st$Names)){
#   sti <- st$Names[i]
#   id_i <- which(dat == sti, arr.ind = T)
#   for (j in 1:length(st$Names)){
#     stj <- st$Names[j]
#     id_j <- which(dat == stj, arr.ind = T)
#     use_both <- merge(id_i, id_j, by = "row")
#     n_p <- length(use_both$row)
#     mat_yn[i,j] <- n_p
#   }
# }
# colnames(mat_yn) <- nodes$Names_vis
# rownames(mat_yn) <- nodes$Names_vis

mat_yn <- readRDS("mat_yn.RDS")

###Steps
# mat_or <- matrix(0, nrow = length(st$Names), ncol = length(st$Names))
# for (i in 1:length(st$Names)){
#   sti <- st$Names[i]
#   id_i <- which(dat == sti, arr.ind = T)
#   for (j in 1:length(st$Names)){
#     stj <- st$Names[j]
#     id_j <- which(dat == stj, arr.ind = T)
#     use_both <- merge(id_i, id_j, by = "row")
#     ord <- use_both$col.x - use_both$col.y
#     i_first <- length(which(ord<0))
#     mat_or[i,j] <- i_first
#   }
# }
# colnames(mat_or) <- nodes$Names_vis
# rownames(mat_or) <- nodes$Names_vis
# saveRDS(mat_or, file = "mat_or.RDS")
mat_or <- readRDS("mat_or.RDS")

#######################
#########DIY###########
#######################
nodes_DIY <- data.frame(Names = "") 
