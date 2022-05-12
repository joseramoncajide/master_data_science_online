##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: Unupervised Learning
##########################################################################

library(tidyverse)
library(tidymodels)
library(broom)
library(hrbrthemes)
theme_set(theme_ipsum_rc())

# In this script:
# 1. Clustering: Kmeans
#       - Robust K-means
# 2. Hierarchical Clustering
#       - Exercise: Hierarchical Clustering
# 3. Principal Component Analysis (PCA)
# 4. Hierarchical Clustering over Principal Component Analysis
# 5. PCA applied to liner regression 
# 6. Robust K-means Practical exercise 



# Clustering: Kmeans ------------------------------------------------------

# Let's create some artifical data around three points. Latter we'll try 
# to discover those points using a unsupervised techinque.
set.seed(1973)
centers <- tibble(
  cluster = factor(1:3), 
  num_points = c(100, 150, 50),  # number points in each cluster
  x1 = c(5, 0, -3),              # x1 coordinate of cluster center
  x2 = c(-1, 1, -2)              # x2 coordinate of cluster center
)

labelled_points <- 
  centers %>%
  mutate(
    x1 = map2(num_points, x1, rnorm),
    x2 = map2(num_points, x2, rnorm)
  ) %>% 
  select(-num_points) %>% 
  unnest(cols = c(x1, x2))

ggplot(labelled_points, aes(x1, x2, color = cluster)) +
  geom_point(alpha = 0.3)

points <- 
  labelled_points %>% 
  select(-cluster)

kclust <- kmeans(points, centers = 3)
kclust
glance(kclust)
augment(kclust, points)

tidy(kclust)

# Compare wagainst the real centers
centers

# Trying different vallues of K

kclusts <- tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(points, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, points)
  )

kclusts

clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))


p1 <- ggplot(assignments, aes(x = x1, y = x2)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)
p1

p2 <- p1 + geom_point(data = clusters, size = 10, shape = "x")
p2

# Elbow chart
# As you can see the optimal number matches the artificial data set
ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point() +
  labs(x = 'Number of clusters',
       y = 'Total within-cluster sum of squares',
       title = 'Total within sum of squares, by # clusters')

assignments %>% 
  filter(k==3) %>% 
  ggplot(aes(x = x1, y = x2)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  geom_point(data = clusters %>% filter(k==3), size = 10, shape = "x") +
  labs(title = 'Best result k=3')



# Robust K-means ----------------------------------------------------------

# Not all the observations need to belong to a cluster

library(tclust)

tclust_res <- tclust(
  x = points,
  k = 3, 
  alpha = 0.1,
  restr = "eigen", 
  restr.fact = 20,
  nstart = 50,
  iter.max = 1000
)
tclust_res

trimmed_cluster <- as.factor(tclust_res$cluster)

labelled_points %>% 
  mutate(trimmed_cluster = trimmed_cluster) -> labelled_points

# Cluster 0 are anomalies
table(labelled_points$trimmed_cluster)

labelled_points %>% 
  ggplot(aes(x = x1, y = x2, shape = as.factor(cluster), color = trimmed_cluster)) +
  geom_point() +
  theme_bw() +
  scale_color_manual(
    breaks = c(0, 1, 2, 3),
    values = c("0"="gray", "1"="steelblue", "2"="orange", "3"="darkgreen")
  ) +
  labs(title = "Real cluster vs predicted") +
  theme(legend.position = "none")



# Hierarchical Clustering -------------------------------------------------

# Maximum or complete linkage clustering: It computes all pairwise dissimilarities
# between the elements in cluster 1 and the elements in cluster 2, 
# and considers the largest value (i.e., maximum value) of these dissimilarities 
# as the distance between the two clusters. It tends to produce more compact clusters.
res_hclust_complete <- points %>%
  dist() %>%
  hclust(method = "complete")

# Mean or average linkage clustering 
# It computes all pairwise dissimilarities between the elements in cluster 1 
# and the elements in cluster 2, and considers the average of these dissimilarities 
# as the distance between the two clusters.
res_hclust_average <- points %>%
  dist() %>%
  hclust(method = "average")

# Ward’s minimum variance method: 
# It minimizes the total within-cluster variance. 
# At each step the pair of clusters with minimum between-cluster distance are merged.
res_hclust_ward <- points %>%
  dist() %>%
  hclust(method = "ward.D2")

library(factoextra)
fviz_dend(res_hclust_complete, main = "complete", k = 3)
fviz_dend(res_hclust_average, main = "average", k = 3)
fviz_dend(res_hclust_ward, main = "res_hclust_ward", k = 3)

# Cut tree into 4 groups
sub_grp <- cutree(res_hclust_ward, k = 3)
# Number of members in each cluster
table(sub_grp)

points %>% 
  mutate(cluster = factor(sub_grp)) %>% 
  ggplot(aes(x = x1, y = x2)) +
  geom_point(aes(color = cluster), alpha = 0.8) + 
  labs(title = 'Hierarchical Clustering: ward.D2 ')




# Principal Components Analysis -------------------------------------------

# Dimensionality reduction is the technique of representing multi-dimensional data ...
# (data with multiple features having a correlation with each other) in 2 or 3 dimensions. 

crime_df <- as_tibble(USArrests, rownames = "state")
crime_df


# PC1: this component  corresponds to a measure of overall rates of serious crimes.
# PC2: this component  corresponds to the level of urbanization of the state
# PC1=−0.5358995 Murder−0.5831836 Assault−0.2781909 UrbanPop−0.5434321 Rape
crime_df %>% 
  select(-state) %>%
  prcomp(scale = TRUE, rank =2 ) %>% 
  tidy(matrix = "loadings") %>% 
  ggplot(aes(value, column)) +
  facet_wrap(~ PC) +
  geom_col()


crime_df %>% 
  select(-state) %>%
  prcomp(scale = TRUE) %>% 
  tidy(matrix = "eigenvalues") %>%
  ggplot(aes(PC, percent)) +
  geom_col()

# The below biplot shows that 50 states mapped according to the 2 principal components.
prcomp(x = crime_df %>% 
         select(-state), 
       scale = TRUE, 
       rank =2) %>% 
  biplot(scale = 0, cex = 0.6, col = c("blue4", "brown3"))

library(ggfortify)
autoplot(prcomp(x = crime_df %>% 
                  select(-state), 
                scale. = T),
         loadings = TRUE, label=T, loadings.colour = 'blue', loadings.label = TRUE, 
         loadings.label.size = 4, loading.label.color = 'red',loadings.label.repel=T, label.repel =T) +
  ggtitle(label = "Principal Component Analysis") +
  theme_ipsum_rc(grid = 'XY')


# PCA is a linear algorithm. It will not be able to interpret complex polynomial relationship between features



# Hierarchical Clustering Exercise ----------------------------------------
# Apply a Hierarchical Clustering to crime_df

# Dissimilarity matrix: Calculate de Euclidena distance for all points


# Hierarchical clustering using ward.D2


# Plot the obtained dendrogram


# Cut tree into 4 groups


# Number of members in each cluster


# Estimate de average values for all the variables for each cluster and extract some conclusions


# Extra:
library("igraph")
set.seed(5665)
fviz_dend(x = crime_hc,
          k = 4,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE,
          cex = 0.8,
          type = "phylogenic",
          repel = TRUE)


# Hierarchical clustering over principal component  -----------------------

library(FactoMineR)
crime_df %>% 
  column_to_rownames('state') %>%
  # Compute principal components
  PCA(ncp = 3, graph = FALSE) %>% 
  # Compute hierarchical clustering
  HCPC(graph = FALSE) -> hcpc_res

fviz_dend(hcpc_res, 
          cex = 0.7,  
          palette = "jco",               
          rect = TRUE, rect_fill = TRUE, 
          rect_border = "jco",  
          labels_track_height = 0.8      
)
  
fviz_cluster(hcpc_res,
             repel = TRUE,        
             show.clust.cent = TRUE, 
             palette = "jco",  
             ggtheme = theme_minimal(),
             main = "Factor map"
)

plot(hcpc_res, choice = "3D.map")



# PLS: PCA applied to liner regression ------------------------------------

# Principal Components Regression PCR 
# How to fit a partial least squares regression using principal componentes as variables

library(faraway)

# Read the description of the problem to solve
?meatspec

data(meatspec)

dim(meatspec)

hist(meatspec$fat)

# Train and test set
train <- meatspec %>% 
  as_tibble() %>% 
  slice(1:172)

test <- meatspec %>% 
  as_tibble() %>% 
  slice(173:215)

model.1 <- lm(fat ~ ., data = train)
summary(model.1)

training_mse <- mean((model.1$fitted.values - train$fat)^2)
training_mse

predictions <- predict(model.1, newdata = test)
test_mse <- mean((predictions - test$fat)^2)
test_mse



# Less try with less number of variables
model.2 <- stats::step(object = model.1, trace = FALSE)

# Number of predictos
length(model.2$coefficients)

training_mse <- mean((model.2$fitted.values - train$fat)^2)
training_mse

predictions <- predict(model.2, newdata = test)
test_mse <- mean((predictions - test$fat)^2)
test_mse



# Let's try with PCA
# Note that: PCA is not a method for variable selection.
res_pca <- prcomp(x = train %>% 
                    dplyr::select(-fat), 
                  scale. = TRUE)

# The first comoponent acounts for 98% of the variance in de dataset
summary(res_pca)$importance[, 1:9]



# Let's use the four first principal components
library(pls)
model.pcr <- pcr(formula = fat ~ ., data = train, scale. = TRUE, ncomp = 4)
summary(model.pcr)

predictions <- predict(model.pcr, newdata = test)
test_mse <- mean((predictions - test$fat)^2)
test_mse
# What does it mean?
# Perhaps the number of PC's is not valid



# Using Cross-validationto select the optimal number of principal components
set.seed(123)
model_cv.pcr <- pcr(formula = fat ~ ., data = train, scale. = TRUE,
                    validation = "CV")
model_cv_res.pcr <- MSEP(model_cv.pcr, estimate = "CV")
num_comp <- which.min(model_cv_res.pcr$val)
num_comp

predictions <- predict(model_cv.pcr, newdata = test, ncomp = num_comp)
test_mse <- mean((predictions - test$fat)^2)
test_mse



# Robust K-means: Practical example ---------------------------------------

crime_df %>% 
  column_to_rownames('state') %>% 
  scale(center = T, scale = T) -> crime_mat

crime_tclust_res <- tclust(
  x = crime_mat,
  k = 4, 
  alpha = 0.1,
  restr = "eigen", 
  restr.fact = 20,
  nstart = 50,
  iter.max = 1000
)

library(ggrepel)
crime_df %>% 
  mutate(cluster = factor(crime_tclust_res$cluster)) -> crime_tclust_df

crime_tclust_df %>% 
  ggplot(aes(x = UrbanPop, y = Murder, color=cluster)) +
  geom_point() +
  geom_text_repel(data = crime_tclust_df %>% filter(cluster == 0), aes(label= state)) +
  labs(title = 'UrbanPop & Murder') + 
  scale_color_manual(
    breaks = c(0, 1, 2, 3),
    values = c("0"="tomato", "1"="steelblue", "2"="orange", "3"="darkgreen")
  ) -> p1

crime_tclust_df %>% 
  ggplot(aes(x = UrbanPop, y = Assault, color=cluster)) +
  geom_point() +
  geom_text_repel(data = crime_tclust_df %>% filter(cluster == 0), aes(label= state)) +
  labs(title = 'UrbanPop & Assault') + 
  scale_color_manual(
    breaks = c(0, 1, 2, 3),
    values = c("0"="tomato", "1"="steelblue", "2"="orange", "3"="darkgreen")
  ) -> p2

crime_tclust_df %>% 
  ggplot(aes(x = UrbanPop, y = Rape, color=cluster)) +
  geom_point() +
  geom_text_repel(data = crime_tclust_df %>% filter(cluster == 0), aes(label= state)) +
  labs(title = 'UrbanPop & Rape') + 
  scale_color_manual(
    breaks = c(0, 1, 2, 3),
    values = c("0"="tomato", "1"="steelblue", "2"="orange", "3"="darkgreen")
  ) -> p3

library(ggpubr)
ggarrange(p1, p2, p3, nrow = 1, common.legend = TRUE, legend = "bottom") 



