library(factoextra)
library(ggpubr)




df <- read.csv("gender-classifier-DFE-791531.csv")
names(df)
X <- na.omit(data.frame(col1 = df$gender.confidence, col2 = df$profile_yn.confidence))
km.res <- kmeans(X, 2, nstart=25)
km.res$cluster

fviz_cluster(km.res, data = X,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
             )
