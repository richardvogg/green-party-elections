library(rvest)
library(dplyr)
library(ggplot2)

http <- "https://de.wikipedia.org/wiki/Liste_der_Wahlergebnisse_und_Regierungsbeteiligungen_von_B%C3%BCndnis_90/Die_Gr%C3%BCnen"
green <- read_html(http)

table1 <- green %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table(fill=T) %>% {.[[1]]} %>% 
  mutate(Datum = Datum %>% 
            strsplit(split="♠") %>%
            lapply(function(x) x <- x[2]) %>%
            unlist(),
          day = substr(Datum,1,2),
          month = substr(Datum,5,7),
          month = case_when(month=="Jan" ~ 1,
                            month=="Feb" ~ 2,
                            month=="Mär" ~ 3,
                            month=="Apr" ~ 4,
                            month=="Mai" ~ 5,
                            month=="Jun" ~ 6,
                            month=="Jul" ~ 7,
                            month=="Aug" ~ 8,
                            month=="Sep" ~ 9,
                            month=="Okt" ~ 10,
                            month=="Nov" ~ 11,
                            month=="Dez" ~ 12),
          year = substr(Datum,nchar(Datum)-3,nchar(Datum)),
         date=as.Date(paste0(year,"-",month,"-",day))) %>% #clear the date
  mutate_all(function(x) gsub("\\[[[:digit:]]*\\]","",x)) %>% #remove quotations
  mutate_at(3:5,function(x) {strsplit(x,split="!") %>% #remove weird exclamation mark duplications
               lapply(function(y) y <- y[length(y)]) %>%
               unlist()}) %>%
  filter(year>=1993) %>%
  mutate(diff_proz_pkt=gsub("[\\+|±]","",`Differenzin Prozent-punkten`),
         diff_proz_pkt=gsub(",",".",diff_proz_pkt),
         zweitstimmen_proz=gsub(",",".",ZweitstimmeninProzent),
         region=factor(Wahl)) %>%
  mutate_at(vars(c("day","year","zweitstimmen_proz")),as.numeric)
  
table1 %>% ggplot(aes(x=year,y=zweitstimmen_proz,group=1,col=year>=2018))+
  geom_line(aes(group=1),size=2)+
  geom_point(size=4)+
  facet_wrap(~region)+
  ggtitle("Green party election results in different German regions (blue=2018/19)")+
  ylab("Percentage of votes")+
  xlab("")+
  theme_light()+
  theme(legend.position = "none") 
