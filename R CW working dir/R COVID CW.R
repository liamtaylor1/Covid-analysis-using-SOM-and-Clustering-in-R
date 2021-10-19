#library
library("dplyr")
library(kohonen)
library(corrplot)
library(RColorBrewer)
library("PerformanceAnalytics")
library(factoextra)
library(cluster)
library(fpc)

#cleans global environment before re-running
remove(Cluster_DF,List_CityDF, Master_DF,KM, x,som.grid,som_model,i,Outliers,BlueToRedFade,colors ,bgcols,som_cluster,var,wss,mydata,M)

#original Data sets imported via "import dataset" wizard
#dataset1 imported as Cities 
#dataset2 imported as City_Info

#merge data sets
Master_DF <- merge (x = Cities, y = City_Info, by.x ="City", by.y = "City", all = FALSE )

#cleaning data by removing any incomplete (redundant) columns
Master_DF <- subset( Master_DF, select = -c(4,17,18,20,25,28) )

#create cities as a separate dataset before standardizing
List_CityDF <- subset ( Master_DF, select = c(1) )
# #add row number
# List_CityDF <- List_CityDF %>% mutate(id = row_number())

#standard deviation of data, this removes City so after standardizing
Master_DF <- scale (Master_DF [2:24], center = TRUE, scale = TRUE)

# exporing data using boxplot
boxplot(Master_DF,
        main = "Master_DF box plot",
        xlab = "per 100,000",
        ylab = "Master dataset Box Plot",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

#storing the outliers of the box plot results (note: Lab 7)
Outliers <- boxplot(Master_DF, plot=FALSE)$out
Outliers

#re-visualize boxplot
boxplot(Master_DF,
        main = "Master_DF box plot",
        xlab = "per 100,000",
        ylab = "Master dataset Box Plot",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)

#Correlation plot
M <-cor(Master_DF)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

#chart of a correlation matrix
chart.Correlation(M, histogram=TRUE, pch=19)

#Attributes for SOM
som.grid = somgrid(xdim = 10, ydim=10, topo="hexagonal", toroidal = TRUE)
som_model <- som(Master_DF,grid=som.grid, rlen=700, alpha=c(0.05,0.01), keep.data = TRUE)

#Som training iteration progress over time 
plot(som_model, type="changes")

#colour palet so we can represent red for higher count in head map
colors <- function(n, alpha = 1) {
        rev(heat.colors(n, alpha))
}

#colors function for the charts
BlueToRedFade <- function(n, alpha = 1) {
        rainbow(n, end=4/6, alpha=alpha)[n:1]
}

#Heatmap SOM, darker red represents higher numbers
plot(som_model, type="count", palette.name = colors, heatkey = TRUE)

#Quality plot, Red contains higher numbers
plot(som_model, type="quality", palette.name = colors, shape = "straight")

#mapping plot used to map citys together based on which are simular
plot(som_model, type="mapping", border = "grey")

#plot depending on distance between each node and nearest neighbor
plot(som_model, type = "dist.neighbours", palette.name = BlueToRedFade)

# Viewing WCSS to determine best cluster numbers
mydata <- getCodes(som_model)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 1:23) {
        wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(wss)

# Create color pallet:
bgcols <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

# Visualising cluster results
# use hierarchical clustering
som_cluster <- cutree(hclust(dist(getCodes(som_model))), 3)
plot(som_model, type="mapping", bgcol = bgcols [som_cluster], main = "Hierarchical Clusters")
add.cluster.boundaries(som_model, som_cluster)

#k-means clustering
KM <- kmeans(Master_DF, centers = 3, iter.max = 10, nstart = 1,
       algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                     "MacQueen"), trace=FALSE)
KM$cluster

#cluster plot
clusplot(Master_DF, KM$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0, main = "Cluster plot")

#variables 
par(mfrow = c(5, 6), cex.main = 1.5)
#loop for each column to create comparison plot
for(i in 1:23) {
        var <- i
        plot(
                som_model,
                type = "property",
                property = getCodes(som_model)[, var],
                main = "",
                palette.name = BlueToRedFade
        )
        title(colnames(getCodes(som_model)) [var], line = 2.5)
        mtext(
                side = 2,
                text = "Map-Values",
                line = 0,
                7,
                cex = 0.8
        )
}
