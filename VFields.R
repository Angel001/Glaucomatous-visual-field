### http://www.orgids.com/longitudinal-glaucomatous-vf-data---download.html

library(dplyr)
library(reshape2)
library(ggplot2)
library(grid)
library(RColorBrewer)

t1 <- read.csv("Patients.csv")
t2 <- read.csv("VFPoints.csv")
t3 <- read.csv("VisualFields.csv", na.strings="-1")
data <- merge(t1,t3)
vfst <- select(data, contains("ID"), SITE)

## Get first and last field id per patient

df <- data %>% group_by(STUDY_ID, SITE) %>% summarise(first = min(FIELD_ID), last = max(FIELD_ID))
df <- melt(df, id = c("STUDY_ID", "SITE"), variable.name = "time", value.name = "FIELD_ID")
vf <- filter(t2, FIELD_ID %in% df$FIELD_ID)
vf <- left_join(vf, vfst)
vf <- left_join(vf, df)

table(vf$time, vf$SITE)

## Not same first as last tests?

x <- vf %>% group_by(FIELD_ID,STUDY_ID, time, SITE) %>% summarise(N=n())
filter(x, N != 54) 
## Found them, delete patient with ID 16, he seems to have 2 tests with 1 field id
vf <- filter(vf, STUDY_ID != 16)

### Preparing for plot

vfd <- vf %>% group_by(SITE, time, X, Y) %>% 
        summarise(meanT = round(mean(THRESHOLD), 1))
f <- filter(vfd, time == "first")
l <- filter(vfd, time == "last")
fvf <- full_join(f, l, by =c("SITE", "X", "Y"))
fvf <- mutate(ungroup(fvf), diffT = round(meanT.y - meanT.x, 2))
fvf$diffCat <- cut(fvf$diffT, seq(-4, 4, 1),
                   labels = c("< -3", "-3, -2", "-2, -1", "-1, 0", 
                              "0, +1", "+1, +2", "+2, +3", "> +3"))

## and plot

mypalette <- paste0("grey", c(seq(10, 40, 10), 80, 90, 100))
mytheme <- theme_minimal() +
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              panel.grid = element_blank(),
              panel.margin = unit(1, "cm"),
              legend.margin = unit(3, "cm"),
              legend.text = element_text(size = 20),
              legend.title = element_blank(),
              legend.position = "bottom",
              strip.text = element_text(face ="bold", size = 20),
              plot.title = element_text(size = 20)
        )

fvf$SITE <- factor(fvf$SITE, levels = c("OS", "OD"))
odd1 <- data.frame(X = c(-15, 15), Y = c(-3, -3), SITE = c("OS", "OD"), diffCat = NA)
odd2 <- data.frame(X = c(-15, 15), Y = c(-3, -3), SITE = c("OD", "OS"), diffCat = NA)

ggplot(fvf, aes(x = X, y = Y, fill = diffCat)) +
        geom_point(pch = 22, size = 20) +
        guides(fill = guide_legend(override.aes = list(size = 10), ncol = 2)) +
        scale_fill_manual(values = mypalette) +
        facet_wrap(~ SITE) +
        geom_text(data = odd1, label = "A", colour = "red") +
        geom_text(data = odd2, label = "B", colour = "red") +
        ggtitle("Mean dB difference\nbetween first and last test") +
        mytheme
dev.off()

## Check for odd points A and B, I will do it only for OD
## point B

odb <- filter(vf, X == -15, Y == -3, SITE == "OD")
bf <- filter(odb, time == "first")
bl <- filter(odb, time == "last")
ticks <- seq(-80, 80, by = 10)

ggplot(bf, aes(x = THRESHOLD)) +
        geom_histogram(aes(y = -..count..), colour = "darkolivegreen4", fill = "darkolivegreen3") +
        geom_histogram(data = bl, aes(y = ..count..), colour = "skyblue3", fill = "skyblue") +
        scale_y_continuous(breaks = ticks, labels = abs(ticks)) +
        annotate("text", x = c(31, 31), y = c(-25, 25),
                 label = c("First test", "Last test")) +
        ggtitle("Histograms for the first and last test at point B for OD") +
        coord_flip() +
        theme_minimal() +
        theme(axis.title.x = element_blank())

## point A

oda <- filter(vf, X == 15, Y == -3, SITE == "OD")
af <- filter(oda, time == "first")
al <- filter(oda, time == "last")

ggplot(af, aes(x = THRESHOLD)) +
        geom_histogram(aes(y = -..count..), colour = "darkolivegreen4", fill = "darkolivegreen3") +
        geom_histogram(data = al, aes(y = ..count..), colour = "skyblue3", fill = "skyblue") +
        scale_y_continuous(breaks = ticks, labels = abs(ticks)) +
        annotate("text", x = c(31, 31), y = c(-25, 25),
                 label = c("First test", "Last test")) +
        ggtitle("Histograms for the first and last test at point A for OD") +
        coord_flip() +
        theme_minimal() +
        theme(axis.title.x = element_blank())
