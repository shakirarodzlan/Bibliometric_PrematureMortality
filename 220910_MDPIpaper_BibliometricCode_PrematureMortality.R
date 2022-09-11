# Analysis bibliometric by Shakira Rodzlan
# Supplement file for manuscript published in MDPI (Healthcare) journal
# Manuscript title: Bibliometric Analysis of Global Research Activity on Premature Mortality


# Packages
install.packages("bibliometrix", dependencies = TRUE)
install.packages("tidyverse")

library(bibliometrix)
library(tidyverse)


# Read data from BibTex (WOS)----------------------------------------------

pm_wo1 <- convert2df(file = "wos1.bib", dbsource = "wos", format = "bibtex") 
pm_wo2 <- convert2df(file = "wos2.bib", dbsource = "wos", format = "bibtex")
pm_wo3 <- convert2df(file = "wos3.bib", dbsource = "wos", format = "bibtex")

pm <- 
  pm_wo1 %>% 
  bind_rows(pm_wo2) %>% 
  bind_rows(pm_wo3)


# Check Missing values ----------------------------------------------------------

pm %>% 
  select(TI, AB) %>% 
  summarise(TI = sum(is.na(TI)), AB = sum(is.na(AB))) #90 missing abstract

pm %>% 
  filter(is.na(AB)) %>% 
  select(TI, AB) %>% 
  remove_rownames() %>% 
  slice(1:5) 


# Check Duplicates --------------------------------------------------------------

pm %>% 
  select(TI, AB) %>%
  summarise(TI = sum(duplicated(TI)), AB = sum(duplicated(AB)))

# Extract all duplicates - TI
pm[duplicated(pm$TI) | duplicated(pm$TI, fromLast = T), "TI"] %>% 
  head()

# Extract 5 duplicate only - AB
pm[duplicated(pm$AB), "AB"] %>% 
  head(5)

#check duplicate ignore NA (missing) - Because R detect NA as duplicate

dup_TI <- duplicated(pm$TI, incomparables=NA) # 1 duplicate for Title
sum(dup_TI)

dup_ab <- duplicated(pm$AB, incomparables=NA) # no duplicate for AB
sum(dup_ab)

# Remove duplicates
pm2 <- pm[!duplicated(pm$TI, incomparables = NA),] #remove 1 duplicate from Title


# Descriptive -------------------------------------------------------------
S <- biblioAnalysis(pm2)
summary(S, 10) #we have book chapter (15)

#remove book chapter
pm3 <- 
  pm2 %>% 
  filter(!DT %in% c("ARTICLE; BOOK CHAPTER", "ARTICLE; PROCEEDINGS PAPER"))

S2 <- biblioAnalysis(pm3)
summary(S2, 10) 

plot(S2, k = 10)

# Article per year (used this for paper)
pm3 %>% 
  group_by(PY) %>% 
  summarise(no_article = n()) %>% 
  ggplot(aes(PY, no_article)) +
  geom_col() +
  scale_x_continuous(breaks = seq(1971, 2022, by = 2)) +
  ylab("Number of publications") +
  xlab("Publication year") +
  theme_bw()

# Total citation  
pm3$TC %>% as.numeric() %>% sum() 


# Collaboration plot ------------------------------------------------------

pm4 <- metaTagExtraction(pm3, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(pm4, 
                           analysis = "collaboration",
                           network = "countries", 
                           sep = ";")
summary(networkStat(NetMatrix))

# Plot the network
collab_countries <- 
  networkPlot(NetMatrix, n = 30, 
              Title = "", 
              type = "kamada", 
              size = TRUE, 
              remove.multiple = TRUE,
              remove.isolates = FALSE, 
              labelsize = 1.1,
              cluster = "none")

# Capitalised country name
country_name <- 
  rownames(collab_countries$nodeDegree %>% 
             as.data.frame()) %>% 
  stringr::str_to_sentence()

country_name
country_name[1] <- "USA"
country_name[2] <- "United Kingdom"

library(igraph)
vertex_attr(collab_countries$graph, "label", index = V(collab_countries$graph)) <- country_name
plot(collab_countries$graph)  


# Bradford's law ----------------------------------------------------------

blaw <- bradford(pm3)
blaw$graph +
  theme_bw() +
  labs(title = "") +
  theme(axis.text.x = element_text(angle = 60,hjust = 1))

blaw$table %>% 
  group_by(Zone) %>% 
  summarise(n = n())

blaw$table %>% 
  remove_rownames() %>% 
  filter(Zone == "Zone 1")  



# Top author --------------------------------------------------------------

#Total citation & T h-index of the first 10 most productive authors 

authors=gsub(","," ",names(S2$Authors)[1:10])
indices <- Hindex(pm3, field = "author", elements=authors, sep = ";", years = 50)
indices$H

#Plot The top author's Production over time

topAU <- authorProdOverTime(pm3, k = 10, graph = TRUE)

edited_topAU <- 
  topAU$graph +
  theme_bw() +
  labs(title = "") +
  xlab("")
edited_topAU


# Lotka's law for scientific productivity --------------------------------------------------------

L <- lotka(S2) # Author Productivity. Empirical Distribution
L$AuthorProd # Beta coefficient estimate
L$Beta # Constant
L$C # Goodness of fit
L$R2 # P-value of K-S two sample test
L$p.value

#Scientific productivity plot

Observed=L$AuthorProd[,3]# Observed distribution

Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))# Theoretical distribution with Beta = 2

#plot
plot(L$AuthorProd[,1],Theoretical,type="l",col="red",ylim=c(0, 1), 
     xlab="Articles",ylab="Freq. of Authors",main="Scientific Productivity") 
lines(L$AuthorProd[,1],Observed,col="blue") 
legend(x="topright",c("Theoretical (B=2)","Observed"),col=c("red","blue"),lty = c(1,1,1),cex=0.6,bty="n")


# Fundings ----------------------------------------------------------------

pm3$FU %>% 
  is.na() %>% 
  as.numeric() %>% 
  table() %>% 
  prop.table() #58% were funded


#Thematic map ----------------------------------------------------------------

Map <- thematicMap(pm3, field = "DE", #"ID","DE", "TI", "AB"
                   minfreq = 3, stemming = FALSE, n.labels = 4, repel = T)
plot(Map$map)


# Further customisation
th_map <- plot(Map$map + 
                 theme_bw() + 
                 theme(axis.line = element_blank(),
                       axis.text.x = element_blank(),
                       axis.text.y = element_blank(), 
                       axis.ticks = element_blank(),
                       legend.position = "none"))

th_map$layers[[6]] <- NULL #remove logo, specific to this plot
th_map


#trending topic ----------------------------------------------------------------

trend_kw <- fieldByYear(pm3, field = "ID", timespan = c(2010,2019),
                        min.freq = 1, n.items = 5, graph = TRUE) 
trend_kw$graph +
  labs(title = "") +
  theme_bw()



