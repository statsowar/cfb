library(rvest)
library(tidyverse)

dat <- read_html("https://247sports.com/Season/2021-Football/TransferPortal/")%>% 
  html_nodes(css = "#page-content > div > section > section > div > ul > li:nth-child(1)") %>%
  html_text() %>% as.list()

names <- str_split(dat, pattern = " ")

dat2 <-
  read_html("https://247sports.com/Season/2021-Football/TransferPortal/") %>% 
  html_nodes(css = "#page-content > div > section > section > div > ul > li") %>%
  html_text()

a <- dat2 %>% as.data.frame()
colnames(a) <- "Variable"
transfer <- a %>% 
  separate(Variable, sep = "   ", into=c("pos", "size", "player", "eligible", 
                                         "transfer", "stuff1", "stuff2", "stuff4")) %>%
  select(player, transfer, hs = stuff1, position = stuff2, eligible = stuff4) %>%
  filter(!is.na(player))%>%
  separate(player, sep = " ", into = c("first", "last", "blank", "height", "junk", "weight")) %>%
  select(-blank, -junk, eligible) %>%
  mutate(
    position = ifelse(!grepl("(HS)", hs), hs, position), 
    hs = ifelse(!grepl("(HS)", hs), transfer, hs), 
    transfer = ifelse(eligible == "" & grepl("(HS)", transfer), "", transfer)) %>%
  mutate(position = trimws(position))

# QUARTERBACKS
######################################################################################
qb.transfers <- transfer %>% filter(position %in% c("DUAL", "PRO", "QB"))
qbs <- data.frame()
len <- nrow(qb.transfers) + 1
for(i in 2:len){

css_string <- paste0("#page-content > div > section > section > div > ul > li:nth-child(", i, ") > div.transfer-institution > a")
tmp <-
  read_html("https://247sports.com/Season/2021-Football/TransferPortal/") %>% 
  html_nodes(css =css_string)


from <- str_match(tmp[1], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021')
from[1,2]

possibleError <- tryCatch(
  to <- str_match(tmp[2], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021'), 
  error=function(e) e
)

if(!inherits(possibleError, "error")) {
  to <- str_match(tmp[2], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021')
  to[1,2]
} else {
(to <- as.data.frame(matrix(ncol = 1, nrow = 2)))
to[1,2] = "?"
} 


tmp2 <- data.frame(matrix(ncol = 2, nrow = 1))
names(tmp2) <- c("from", "to")
tmp2$from <- from[1,2]
tmp2$to <- to[1,2]

qbs<-rbind(qbs, tmp2)
}



# RUNNING BACKS
######################################################################################
rbs <- data.frame()
rb.transfers <- transfer %>% filter(position %in% c("APB", "RB"))
len <- i + 2
end <- len + nrow(rb.transfers) - 1
for(i in len:end){
  css_string <- paste0("#page-content > div > section > section > div > ul > li:nth-child(", i, ") > div.transfer-institution > a")
  tmp <-
    read_html("https://247sports.com/Season/2021-Football/TransferPortal/") %>% 
    html_nodes(css =css_string)
  
  
  from <- str_match(tmp[1], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021')
  from[1,2]
  
  possibleError <- tryCatch(
    to <- str_match(tmp[2], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021'), 
    error=function(e) e
  )
  
  if(!inherits(possibleError, "error")) {
    to <- str_match(tmp[2], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021')
    to[1,2]
  } else {
    (to <- as.data.frame(matrix(ncol = 1, nrow = 2)))
    to[1,2] = "?"
  } 
  
  
  tmp2 <- data.frame(matrix(ncol = 2, nrow = 1))
  names(tmp2) <- c("from", "to")
  tmp2$from <- from[1,2]
  tmp2$to <- to[1,2]
  
  rbs<-rbind(rbs, tmp2)
}






# WRs
######################################################################################
wrs <- data.frame()
wr.transfers <- transfer %>% filter(position %in% c("TE", "WR"))
len <- i + 1
end <- nrow(wr.transfers) + len - 1
for(i in len:end){
  
  css_string <- paste0("#page-content > div > section > section > div > ul > li:nth-child(", i, ") > div.transfer-institution > a")
  tmp <-
    read_html("https://247sports.com/Season/2021-Football/TransferPortal/") %>% 
    html_nodes(css =css_string)
  
  
  from <- str_match(tmp[1], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021')
  from[1,2]
  
  possibleError <- tryCatch(
    to <- str_match(tmp[2], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021'), 
    error=function(e) e
  )
  
  if(!inherits(possibleError, "error")) {
    to <- str_match(tmp[2], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021')
    to[1,2]
  } else {
    (to <- as.data.frame(matrix(ncol = 1, nrow = 2)))
    to[1,2] = "?"
  } 
  
  
  tmp2 <- data.frame(matrix(ncol = 2, nrow = 1))
  names(tmp2) <- c("from", "to")
  tmp2$from <- from[1,2]
  tmp2$to <- to[1,2]
  
  wrs<-rbind(wrs, tmp2)
}






# OLINE
######################################################################################
ol <- data.frame()
ol.transfers <- transfer %>% filter(position %in% c("OC", "OG", "OT", "IOL"))
len <- i + 2
end <- len + nrow(ol.transfers) - 1
for(i in len:end){
  
  css_string <- paste0("#page-content > div > section > section > div > ul > li:nth-child(", i, ") > div.transfer-institution > a")
  tmp <-
    read_html("https://247sports.com/Season/2021-Football/TransferPortal/") %>% 
    html_nodes(css =css_string)
  
  
  from <- str_match(tmp[1], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021')
  from[1,2]
  
  possibleError <- tryCatch(
    to <- str_match(tmp[2], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021'), 
    error=function(e) e
  )
  
  if(!inherits(possibleError, "error")) {
    to <- str_match(tmp[2], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021')
    to[1,2]
  } else {
    (to <- as.data.frame(matrix(ncol = 1, nrow = 2)))
    to[1,2] = "?"
  } 
  
  
  tmp2 <- data.frame(matrix(ncol = 2, nrow = 1))
  names(tmp2) <- c("from", "to")
  tmp2$from <- from[1,2]
  tmp2$to <- to[1,2]
  
  ol<-rbind(ol, tmp2)
}

# DLINE
######################################################################################
dl <- data.frame()
dl.transfer <- transfer %>% filter(position %in% c("DL", "DT", "SDE", "WDE", "Edge"))
len <- i + 2
end <- len + nrow(dl.transfer) - 1
for(i in len:end){
  
  css_string <- paste0("#page-content > div > section > section > div > ul > li:nth-child(", i, ") > div.transfer-institution > a")
  tmp <-
    read_html("https://247sports.com/Season/2021-Football/TransferPortal/") %>% 
    html_nodes(css =css_string)
  
  
  from <- str_match(tmp[1], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021')
  from[1,2]
  
  possibleError <- tryCatch(
    to <- str_match(tmp[2], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021'), 
    error=function(e) e
  )
  
  if(!inherits(possibleError, "error")) {
    to <- str_match(tmp[2], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021')
    to[1,2]
  } else {
    (to <- as.data.frame(matrix(ncol = 1, nrow = 2)))
    to[1,2] = "?"
  } 
  
  
  tmp2 <- data.frame(matrix(ncol = 2, nrow = 1))
  names(tmp2) <- c("from", "to")
  tmp2$from <- from[1,2]
  tmp2$to <- to[1,2]
  
  dl<-rbind(dl, tmp2)
}

# LINEBACKER
######################################################################################
lbs <- data.frame()
lb.transfer <- transfer %>% filter(position %in% c("LB", "OLB", "ILB"))
len <- i + 2
end <- len + nrow(lb.transfer) - 1
for(i in len:end){
  
  css_string <- paste0("#page-content > div > section > section > div > ul > li:nth-child(", i, ") > div.transfer-institution > a")
  tmp <-
    read_html("https://247sports.com/Season/2021-Football/TransferPortal/") %>% 
    html_nodes(css =css_string)
  
  
  from <- str_match(tmp[1], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021')
  from[1,2]
  
  possibleError <- tryCatch(
    to <- str_match(tmp[2], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021'), 
    error=function(e) e
  )
  
  if(!inherits(possibleError, "error")) {
    to <- str_match(tmp[2], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021')
    to[1,2]
  } else {
    (to <- as.data.frame(matrix(ncol = 1, nrow = 2)))
    to[1,2] = "?"
  } 
  
  
  tmp2 <- data.frame(matrix(ncol = 2, nrow = 1))
  names(tmp2) <- c("from", "to")
  tmp2$from <- from[1,2]
  tmp2$to <- to[1,2]
  
  lbs<-rbind(lbs, tmp2)
}

dbs <- data.frame()
db.transfer <- transfer %>% filter(position %in% c("ATH", "CB", ""))
len <- i + 2
end <- len + nrow(db.transfer) - 1
for(i in len:end){
    
  css_string <- paste0("#page-content > div > section > section > div > ul > li:nth-child(", i, ") > div.transfer-institution > a")
 
  tmp <-
    read_html("https://247sports.com/Season/2021-Football/TransferPortal/") %>% 
    html_nodes(css =css_string)

    

    from <- str_match(tmp[1], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021')
 
 possibleError <- tryCatch(
    to <- str_match(tmp[2], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021'), 
    error=function(e) e
  )
  
  if(!inherits(possibleError, "error")) {
    to <- str_match(tmp[2], '<a href="https://247sports.com/college/\\s*(.*?)\\s*/Season/2021')
    to[1,2]
  } else {
    (to <- as.data.frame(matrix(ncol = 1, nrow = 2)))
    to[1,2] = "?"
  } 
  
  
  tmp2 <- data.frame(matrix(ncol = 2, nrow = 1))
  names(tmp2) <- c("from", "to")
  tmp2$from <- from[1,2]
  tmp2$to <- to[1,2]
  }
  dbs<-rbind(dbs, tmp2)
}



destination <- rbind(qbs, rbs, wrs, ol, dl, lbs, dbs)

transfer.no.st <- transfer %>% filter(row_number() < i)

transfers <- cbind(transfer.no.st, destination) %>%
  mutate(from = str_replace(from, "-", " "),
         to = str_replace(to, "-", " "),
         from = str_to_title(from),
         to = str_to_title(to))

write.csv(transfers, "transfers.csv")
