# Loading Packages ------
library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(car)
library(moments)
library(ggplot2)
library(tikzDevice)
library(showtext)
library(gridExtra)
library(ggpattern)
library(Hmisc)
library(plyr)
library(psych)
library(dplyr)
library(rstatix)
library(scales)
library(xtable)
# 1 Import  CSV SA -----------------------------------------------------------------------------------------------
##  1.1 Auswertung Gesamt -----------------------------------------------------------------------------------------
df <- read_csv2('data/Auswertunggesamt.csv') %>%
  clean_names() %>%
  mutate(gruppe = as.factor(gruppe),
         tag = as.factor(tag),
         sa_gesamt = as.numeric(sa_gesamt),
         qualitat = as.factor(qualitat),
         gruppe2 = plyr::revalue(gruppe, c("M Flat" = "M 2D", 'M Sphere' = 'M 3D')),
         szene = str_remove(szene, "SA"),
         szene= as.factor(szene),
         verkehrsaufkommen = as.factor(verkehrsaufkommen),
         verkehrsaufkommen2 = plyr::revalue(verkehrsaufkommen, c("1" = "L", "2"='M', "3"="H")),
         proband = as.factor(proband),
         altergruppe = as.factor(altersgruppe),
         km_y = as.factor(km_y)) %>% 
         na.omit() %>%
  select(-gruppe) %>% 
  select(-verkehrsaufkommen) %>% 
  dplyr::rename(gruppe = gruppe2) %>%
  dplyr::rename(verkehrsaufkommen = verkehrsaufkommen2) %>%
  dplyr::rename(termin = tag) %>%
  set_names(capitalize(names(.)))


# Set order for this factor
df$Szene <- factor(df$Szene, levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29))
df$Gruppe <- factor(df$Gruppe, levels=c('M 2D','M 3D','HMD'))
print(levels(df$Gruppe))

## 1.2 Abschlussfragebogen ------------------------------------------------------------------------------------------
dfA <- read_csv2('data/Abschlussfragebogen Putz.csv')  %>%
  na.omit() %>%
  mutate(Usab2 =as.numeric(gsub(",", ".", Usab2)),
         TLX = as.numeric(gsub(",", ".", TLX)),
         Gruppe = as.factor(Gruppe),
         Termin = as.factor(Termin),
         Qualitat = as.factor(Qualitat))
skim(dfA)
str(dfA)
hist(dfA$Pe1)
hist(dfA$Immersion)
str(dfA)
hist(dfA$Immersion)
## 1.3 Create Additional Dataframes ------------------------------------------------------------------------------------

str(df)
problems(df)
skim(df)

df_hmd <- df %>% filter(Gruppe=='HMD')
df_flat <- df %>% filter(Gruppe=='M 2D')
df_sphere <- df %>% filter(Gruppe=='M 3D')
p1<-hist(df_flat$Sa_gesamt)
p2<-hist(df_hmd$Sa_gesamt)
p3<-hist(df_sphere$Sa_gesamt)
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,1))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,1), add=T)  # second
plot( p3, col=rgb(1,0,0,1/4), xlim=c(0,1), add=T)  # second


# 2 Statistische Auswertung ----------------------------------------------------------------------------------------
## 2.1 Kruskal Wallis - all days - sa_gesamt ~ gruppe ---- 
df_hmd <- df %>% filter(Gruppe=='HMD')
# Deskriptive Statistik
describeBy(df_hmd$Sa_gesamt,df_hmd$Termin) #

# Kruskal Test
k_res <- kruskal.test(df$Sa_gesamt~df$Gruppe)
k_res
# Paarweiser Wilcox Test
pairwise.wilcox.test(df_hmd$Sa_gesamt,df_hmd$Termin, p.adjust="bonferroni")

# Effektstärke Kruskal Wallis Test
eta_squared <- (k_res[["statistic"]][["Kruskal-Wallis chi-squared"]]-4+1)/(60*4-4)
sqrt(eta_squared/(1-eta_squared))

# Effektstärke Wilox paarweiser Vergleich 

df_hmd %>%
  wilcox_effsize(Sa_gesamt~Termin) %>%
  as.data.frame()

sink(file = "tables/kw.tex")
foo <- as.matrix(k_res)
print(xtable(foo, caption = "Kruskal Test", label = "tab:one",
             digits = c(0, 0, 2, 0, 2, 3, 3)), table.placement = "tbp",
      caption.placement = "top")
sink(file = NULL)





#########################################
## 2.2 Kruskal Wallis - all days - sa_gesamt ~ gruppe ---- 

# Deskriptive Statistik
describeBy(df$Sa_gesamt,df$Gruppe) #

# Kruskal Test
k_res <- kruskal.test(df$Sa_gesamt~df$Gruppe)
k_res
# Paarweiser Wilcox Test
pairwise.wilcox.test(df$Sa_gesamt,df$Gruppe, p.adjust="bonferroni")

# Effektstärke Kruskal Wallis Test
eta_squared <- (k_res[["statistic"]][["Kruskal-Wallis chi-squared"]]-3+1)/(240+240+228-3)
sqrt(eta_squared/(1-eta_squared))
# -> Schwacher Effekt 

# Effektstärke Wilox paarweiser Vergleich 

df %>%
  wilcox_effsize(Sa_gesamt~Gruppe) %>%
  as.data.frame()

#########################################
# 3 Plotting ----
## 3.1 Defines ----
### 3.2 Boxplottheme ----
boxplotTheme_light <- theme_light()+theme(axis.line = element_line(linewidth = 0.25),
  panel.grid.major = element_line(linewidth = 0.25),
  panel.grid.minor = element_line(linewidth = 0.25))+  
  theme(legend.key.width = unit(0.4, "cm"),
        legend.key.height = unit(0.6, "cm"),
        axis.text.x= element_text( colour="#000000"),
        axis.text.y= element_text( colour="#000000"),
        legend.text = element_text(size=10)) # adjust legend key size

boxplotTheme_standard <- theme(axis.line = element_line(linewidth = 0.0),
                      panel.grid.major = element_line(linewidth = 0.25),
                      panel.grid.minor = element_line(linewidth = 0.25))+  
                      theme(legend.key.width = unit(0.6, "cm"),
        legend.key.height = unit(0.8, "cm"),
        axis.text.x= element_text( colour="#000000"),
        axis.text.y= element_text( colour="#000000"),
        legend.text = element_text(size=10)) # adjust legend key size

boxplotTheme <- boxplotTheme_standard

boxplot_colorPalette <- c("#3070B3", "#F7B11E", "#9FBA36", "#075507")
boxplot_colorPalette <- c("#D7E4F4", "#FAD080", "#C7D97D", "#075507")
boxplot_boxWidth = 0.5
boxplot_patternSpacing = 0.03
boxplot_patternDensity = 0.01

#boxplot_patternSpacing = 0.02
#boxplot_patternDensity = 0.06

legendAtBottom <- theme(legend.position = "bottom", legend.direction = "horizontal")
legendNotTitle <- theme(legend.title = element_blank())
#### 3.2.1  Trendline ----
trendline <- geom_smooth(method=lm,se=FALSE, color="black", aes(group=1)) 

patternColor='#000000'
patternFill='#000000'
patternAlpha=0.2

### 3.3 Shapes and Sizes ----
geomshape=4
geomsize=3
geomcolor='#000000'
geomfill='#000000'
plotHeight = 3.2
plotWidthWide = 5.66
plotWidthNarrow = 3.66
### 3.4 Utilites ---- 
#### 3.4.1 Percent Formatting y Axis ---- 
percent_format <- function(x) {
  paste0(format(x * 100, nsmall = 0), "\\%")
}

#### 3.4.2 TIKZ Start Function ----
stikz <- function(name,wi,hi){
  name<-paste('plots/',name)
  new_name <- gsub(" ","",paste(name,'.tex'))
  tikz(new_name, width = wi,height = hi,pointsize = 12) #define plot name size and font size
  par(mar=c(2,2,2,2)) # The syntax is mar=c(bottom, left, top, right).
}

## 3.2 Plots ----
### 3.2.1 Situationsbewusstsein ----
#### BP_SA~TERcG ----
stikz("BP_SA~TERcG",plotWidthNarrow,plotHeight)
ggplot(df, aes(x = Termin, y = Sa_gesamt, fill = Gruppe))+
  stat_boxplot(geom = 'errorbar',linewidth=0.2) +
  geom_boxplot(size=0.2)+
  scale_y_continuous(labels = percent_format) +
  scale_fill_manual(name = 'Gruppe', values = boxplot_colorPalette) +
  geom_boxplot_pattern(aes( pattern = Gruppe),pattern_color=patternColor, pattern_fill=patternFill,pattern_spacing = boxplot_patternSpacing,pattern_alpha=patternAlpha,pattern_density = boxplot_patternDensity)+
  stat_summary(fun.y=mean, color=geomcolor, position = position_dodge(0.75), geom="point", shape=geomshape, size=geomsize, show.legend = FALSE) +
  labs(x = "Termin", y = "SA Gesamt" ) +
  boxplotTheme+
  trendline+
  legendAtBottom+theme(legend.key = element_rect(fill = NA)) 
dev.off() # export file and exit tikzDevice function


#### BP_SA~SZcVER ----
stikz("BP_SA~SZcVER",plotWidthWide,plotHeight)
ggplot(df, aes(x = Szene, y = Sa_gesamt, fill = Verkehrsaufkommen))+
    stat_boxplot(geom = 'errorbar',linewidth=0.2)+
    geom_boxplot(size=0.2)+
    scale_y_continuous(labels = percent_format) +
    scale_fill_manual(name = 'Verkehrsaufkommen', values = boxplot_colorPalette) +
    geom_boxplot_pattern(aes( pattern = Verkehrsaufkommen),pattern_color=patternColor, pattern_fill=patternFill,pattern_spacing = boxplot_patternSpacing,pattern_alpha=patternAlpha,pattern_density = boxplot_patternDensity)+
    stat_summary(fun.y=mean, color=geomcolor, position = position_dodge(0.75), geom="point", shape=geomshape, size=geomsize, show.legend = FALSE) +
    labs(x = "Szene", y = "SA Gesamt") +
    boxplotTheme+
    legendAtBottom+theme(legend.key = element_rect(fill = NA))
dev.off()

### 3.2.2 Verkehrsteilnehmer ----
#### BP_VT~TERcG ----
stikz("BP_VT~TERcGruppe",plotWidthNarrow,plotHeight)
ggplot(df, aes(x = Termin, y = Vt, fill = Gruppe))+
 stat_boxplot(geom = 'errorbar',linewidth=0.2) +
  geom_boxplot(size=0.2)+
  scale_fill_manual(name = 'Gruppe', values = boxplot_colorPalette) +
  geom_boxplot_pattern(aes( pattern = Gruppe),pattern_color=patternColor, pattern_fill=patternFill,pattern_spacing = boxplot_patternSpacing,pattern_alpha=patternAlpha,pattern_density = boxplot_patternDensity)+
  labs(x = "Termin", y = "Verkehrsteilnehmer") +
  stat_summary(fun.y=mean, color="black", position = position_dodge(0.75), geom="point", shape=4, size=5, show.legend = FALSE) +
  boxplotTheme+
  trendline+
  legendAtBottom+theme(legend.key = element_rect(fill = NA))
dev.off() # export file and exit tikzDevice function

#### BP_VT~SZcQ ----
stikz("BP_VT~SZcQ ",plotWidthWide,plotHeight)
ggplot(df, aes(x = Szene, y = Vt, fill=Qualitat))+
  stat_boxplot(geom = 'errorbar',linewidth=0.2)+
  geom_boxplot(size=0.2)+
  scale_y_continuous(labels = percent_format) +
  scale_fill_manual(name = 'Qualitat', values = boxplot_colorPalette) +
  geom_boxplot_pattern(aes( pattern = Qualitat),pattern_color=patternColor, pattern_fill=patternFill,pattern_spacing = boxplot_patternSpacing,pattern_alpha=patternAlpha,pattern_density = boxplot_patternDensity)+
  stat_summary(fun.y=mean, color=geomcolor, position = position_dodge(0.75), geom="point", shape=geomshape, size=geomsize, show.legend = FALSE) +
  labs(x = "Szene", y = "Verkehrsteilnehmer") +
  boxplotTheme+
  legendAtBottom+theme(legend.key = element_rect(fill = NA))
dev.off()

### 3.2.3 Verkehrsschilder ----
#### BP_VS~TERcG ----
stikz("BP_VS~TERcG",plotWidthWide,plotHeight)
all<- ggplot(df, aes(x = Termin, y = Vs, fill = Gruppe))+
  stat_boxplot(geom = 'errorbar',linewidth=0.2) +
  geom_boxplot(size=0.2)+
    scale_y_continuous(labels = percent_format) +
  scale_fill_manual(name = 'Gruppe', values = boxplot_colorPalette) +
  geom_boxplot_pattern(aes( pattern = Gruppe),pattern_color=patternColor, pattern_fill=patternFill,pattern_spacing = boxplot_patternSpacing,pattern_alpha=patternAlpha,pattern_density = boxplot_patternDensity)+
  labs(x = "Termin", y = "Verkehrsschilder") +
  boxplotTheme+
  trendline+
  legendAtBottom+theme(legend.key = element_rect(fill = NA))

hmd <-ggplot(df_hmd, aes(x = Termin, y = Vs, fill = Gruppe))+
  stat_boxplot(geom = 'errorbar',linewidth=0.2) +
  geom_boxplot(size=0.2)+
  scale_y_continuous(labels = percent_format) +
  scale_fill_manual(name = 'Gruppe', values = boxplot_colorPalette) +
  labs(x = "Termin", y = "Verkehrsschilder") +
  stat_summary(fun=mean, geom="point", shape=3, size=7, color="black", fill="red",position = position_dodge(0.75)) +
  boxplotTheme+
  trendline+
  legendAtBottom+theme(legend.key = element_rect(fill = NA))
grid.arrange(all,hmd, ncol=2)
dev.off() # export file and exit tikzDevice function

### 3.2.4 Immersion ----
#### BP_IM~TERcG ----
stikz("BP_IM~TERcG",plotWidthWide,plotHeight)
ggplot(dfA, aes(x = Termin, y = Immersion, fill=Gruppe))+
  stat_boxplot(geom = 'errorbar',linewidth=0.2)+
  geom_boxplot(size=0.2)+
  scale_fill_manual(name = 'Gruppe', values = boxplot_colorPalette) +
  geom_boxplot_pattern(aes( pattern = Gruppe),pattern_color=patternColor, pattern_fill=patternFill,pattern_spacing = boxplot_patternSpacing,pattern_alpha=patternAlpha,pattern_density = boxplot_patternDensity)+
  stat_summary(fun.y=mean, color=geomcolor, position = position_dodge(0.75), geom="point", shape=geomshape, size=geomsize, show.legend = FALSE) +
  labs(x = "Termin", y = "Immersion") +
  boxplotTheme+
  legendAtBottom+theme(legend.key = element_rect(fill = NA))
dev.off()

#### BP_IM~TERcG ----
stikz("BP_IM~TERcG",plotWidthWide,plotHeight)
ggplot(dfA, aes(x = Gruppe, y = Immersion, fill=Qualitat))+
  stat_boxplot(geom = 'errorbar',linewidth=0.2)+
  geom_boxplot(size=0.2)+
  scale_fill_manual(name = 'Qualitat', values = boxplot_colorPalette) +
  geom_boxplot_pattern(aes( pattern = Qualitat),pattern_color=patternColor, pattern_fill=patternFill,pattern_spacing = boxplot_patternSpacing,pattern_alpha=patternAlpha,pattern_density = boxplot_patternDensity)+
  stat_summary(fun.y=mean, color=geomcolor, position = position_dodge(0.75), geom="point", shape=geomshape, size=geomsize, show.legend = FALSE) +
  labs(x = "Termin", y = "Immersion") +
  boxplotTheme+
  legendAtBottom+theme(legend.key = element_rect(fill = NA))
dev.off()






# Testing Random ----

df_H <- df %>% filter(Qualitat == 'H' )
df_N <- df %>% filter(Qualitat == 'N' )
mean(df_H$Vt)
mean(df_N$Vt)
describe(df_H$Vt)
describe(df_N$Vt)

wilcox.test(df_H$Vt,df_N$Vt, paired = TRUE, exact = TRUE, correct = TRUE, conf.int = TRUE)

describe(df_hmd$Vt)
wil<-wilcox.test(Vt~Qualitat, data = df_hmd, exact = FALSE, correct = FALSE, conf.int = FALSE)
z <- qnorm(wil[["p.value"]]/2)
r <- abs(z/sqrt(239))
r



## Testing ---- 
#pdf("test.pdf", height=2, width=6)
#hist(df$sa_gesamt)
#plot(1:10, type='l', main='boring')
#dev.off()

r2spss::wilcoxon_test(df_hmd,c('Vt','Qualitat'), exact = FALSE)


hist(dfA$Immersion)

