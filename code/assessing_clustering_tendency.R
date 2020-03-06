# Assessing Clustering Tendency.R
#
#
# Version:  1.1
#
# Date:     2020  Jan
# Author:   Paraskevi Massara (p.massara@utoronto.ca)
#
# Versions:
#   
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. 
#
# ==============================================================================

#== Objectives ==================================================================

# We will start by describing why clustering tendency is necessary before applying 
# any clustering method on a data. Next, we will apply visual and statistical 
# visual methods for assessing the clustering tendency.

#RESOURCES:http://www.sthda.com/english/

#TOC> ==========================================================================
#TOC>
#TOC>   Section  Title                                            Line
#TOC> -----------------------------------------------------------------
#TOC>   1        Packages                                          39
#TOC>   2        Data preparation                                  52      
#TOC>   3        Visual inspection                                 75
#TOC>   4        Why assessing clustering tendency is important?   91                
#TOC>
#TOC> ===========================================================================

# =    1  Packages  =============================================================

if (! require(factoextra, quietly=TRUE)) {
  install.packages(factoextra)
  library(factoextra)
}

if (! require(clustertend, quietly=TRUE)) {
  install.packages(clustertend)
  library(clustertend)
}


# =    2  Data preparation  ====================================================

# We will use 2 datasets

# --the built-in R data set iris
# --and a random data set derived by iris

# Iris data set

head(iris, 3)

# We will exclude column 5 (categorical variable)
df <- iris[, -5]

# Random data generated from the iris data set
ran_df <- apply(df, 2,
                   function(x){runif(length(x), min(x), (max(x)))})

ran_df <- as.data.frame(ran_df)

# Standardize the data sets
df <- iris.scaled <- scale(df)
ran_df <- scale(ran_df)

# =    3  Visual inspection  ==============================================================

# Now that we have the 2 datasets let's assess whether they contain any meaningful clusters


# Plot Iris data set

fviz_pca_ind(prcomp(df), title = "PCA - Iris data",
             habillage = iris$Species, palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")

# Plot the random df
fviz_pca_ind(prcomp(ran_df), title = "PCA - Random data",
             geom = "point", ggtheme = theme_classic())

# =    4  Why assessing clustering tendency is important?  =================================

set.seed(123)

# K-means on iris dataset
km.res1 <- kmeans(df, 3)
fviz_cluster(list(data = df, cluster = km.res1$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())


# K-means on the random dataset
km.res2 <- kmeans(ran_df, 3)
fviz_cluster(list(data = ran_df, cluster = km.res2$cluster),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())


# Hierarchical clustering on the random dataset
fviz_dend(hclust(dist(ran_df)), k = 3, k_colors = "jco",
          as.ggplot = TRUE, show_labels = FALSE)











