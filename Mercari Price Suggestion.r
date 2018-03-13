
getwd()

setwd('C:/Users/sugand/Documents')

getwd()

#list of libraries we require
library(data.table)
library(ggplot2)
library(dplyr)
library(xgboost)
library(MLmetrics)
library(treemapify)
library(tm)
library(wordcloud)
library(RColorBrewer)

train <- fread('mercaritrain.tsv', sep = '\t')

summary(train)

str(train)

# Checking for missing data
any(is.na(train))

#Univariate analysis

#First let's check out target variable Price

range(train$price)


pl <- ggplot(train,aes(price)) + geom_histogram(aes(fill=..count..),color='black',bins  = 30,alpha = 0.7) + theme_bw()
pl + ggtitle('Histogram of price') + theme(plot.title = element_text(hjust = 0.5))


# Ok the plot sizes are too big let's convert them to a smaller size
options(repr.plot.width=5, repr.plot.height=4)

pl <- ggplot(train,aes(price)) + geom_histogram(aes(fill=..count..),color='black',bins  = 30,alpha = 0.7) + theme_bw()
pl + ggtitle('Histogram of price') + theme(plot.title = element_text(hjust = 0.7))


pl2 <- ggplot(train,aes(log(price+1))) + geom_histogram(aes(fill=..count..), color = 'black',bins  = 30,alpha = 0.7) + theme_bw()
pl2 + ggtitle('Histogram of log of price') + theme(plot.title = element_text(hjust = 0.5))


#look like some of the items are given free of cost, now lets look how many items are given away like that
sum(train$price == 0)

#now lets look at the item condition id
range(train$item_condition_id)


table(train$item_condition_id)

#item with condition 1 is the highest, now let visualize the same
pl3 <- ggplot(train,aes(factor(item_condition_id)))+ geom_bar(aes(fill=item_condition_id),color='black',alpha = 0.7) + theme_bw()
pl3 + ggtitle('Item condition') + theme(plot.title = element_text(hjust = 0.5))

#now let's look at the median prices for item conditions
train[,list(medianprice = median(price)), by = list(item_condition_id)][order(item_condition_id)]


#now lets look at the relationship between item condition and price
options(repr.plot.width=5, repr.plot.height=4)
pl4 <- ggplot(train,aes(factor(item_condition_id),log(price+1)))+ geom_boxplot(aes(fill=factor(item_condition_id)),alpha = 0.4) + theme_bw()
pl4 + scale_y_continuous(breaks = seq(min(0),max(10),by = 1)) + ggtitle('Item Condition vs Price') + theme(plot.title = element_text(hjust = 0.5))


#now lets look at the shipping data
pl5 <- ggplot(train,aes(factor(shipping))) + geom_bar(aes(fill=factor(shipping)),color='black',alpha = 0.7) + theme_bw()
pl5 + ggtitle('An overview of shipping data') + theme(plot.title = element_text(hjust = 0.5))


# now let's see the relationship between price and shipping data

pl6 <- ggplot(train,aes(factor(shipping),log(price+1))) + geom_boxplot(aes(fill=factor(shipping)),color='black',alpha = 0.4) + theme_bw()
pl6 + ggtitle('shipping data vs price') + theme(plot.title = element_text(hjust = 0.5))


#now let's see the relationship between price, shipping and item condition_id from above obtained median price.

df <- filter(train,item_condition_id == 5, shipping == 0, price > 19)


head(df)

dim(df)

table(df$brand_name == "")

# now lets analyze the category name

length(unique(train$category_name))


#looks like there are lot of categories
# let's take a look at top ten
sort(table(train$category_name), decreasing = TRUE)[1:10]

#now inorder to proceed further with analysis,let's split the categories
split <- strsplit(train$category_name, split = "/")


cat1 <- sapply(split,'[',1) 


cat2 <- sapply(split,'[',2) 


cat3 <- sapply(split,'[',3) 
cat4 <- sapply(split,'[',4) 
cat5 <- sapply(split,'[',5) 
cat6 <- sapply(split,'[',6) 

train$cat1 <- cat1 
train$cat2 <- cat2
train$cat3 <- cat3 
train$cat4 <- cat4 
train$cat5 <- cat5 
train$cat6 <- cat6

table(cat1)


#ok now since we have successfully splitted, let's see if we have any 'na's' in our category
sum(is.na(train$cat1))


#yes there exsists hence we shall assign them to a negative value and keep them aside
train$cat1[is.na(train$cat1)] = -1



sum(is.na(train$cat2))



train$cat2[is.na(train$cat2)] = -1



sum(is.na(train$cat3))




train$cat3[is.na(train$cat3)] = -1


sum(is.na(train$cat4))


sum(is.na(train$cat5))

sum(is.na(train$cat6))

#table cat4,5,6 contains many NA's hence we drop them
colnames(train)

train <- train[,-c(11,12,13)]


colnames(train)

# so now we have 3 categories, now let's start analyzing them


#when it comes to analyzing Hierarchical structures Tree maps stand out to be the best
#setting the size for tree maps
options(repr.plot.width=7, repr.plot.height=7)


#now let's start of with 1st and 2nd categories
train %>% group_by(cat1, cat2) %>% count() %>% ungroup() %>% ggplot(aes(area=n, fill=cat1, label=cat2, subgroup=cat1)) +
geom_treemap() + geom_treemap_subgroup_text(grow = T, alpha = 0.5, colour = "white", fontface = "italic", min.size = 0) +
geom_treemap_text(colour = "black", place = "topleft", reflow = T) + theme(legend.position = "null") +
ggtitle("1st and 2nd Hierarchical Category Levels")

train[cat1 =="Women", .N, by = list(cat2, cat3)] %>% group_by(cat2, cat3) %>% count() %>% ungroup() %>% ggplot(aes(area=n, fill=cat2, label=cat3, subgroup=cat2)) +
geom_treemap() + geom_treemap_subgroup_text(grow = T, alpha = 0.5, colour = "white", fontface = "italic", min.size = 0) +
geom_treemap_text(colour = "black", place = "topleft", reflow = T) + theme(legend.position = "null") +
ggtitle("2nd and 3rd Hierarchical Category Levels under Women")

train[cat1=='Men',.N, by = list(cat2,cat3)] %>% group_by(cat2,cat3) %>% count %>% ungroup() %>% ggplot(aes(area=n, fill=cat2, label=cat3, subgroup=cat2)) +
geom_treemap() + geom_treemap_subgroup_text(grow = T, alpha = 0.5, colour = "white", fontface = "italic", min.size = 0) +
geom_treemap_text(colour = "black", place = "topleft", reflow = T) + theme(legend.position = "null") +
ggtitle("2nd and 3rd Hierarchical Category Levels under Men")

train %>% group_by(cat2, cat3) %>% count() %>% ungroup() %>% ggplot(aes(area=n, fill=cat2, label=cat3, subgroup=cat2)) +
geom_treemap() + geom_treemap_subgroup_text(grow = T, alpha = 0.5, colour = "white", fontface = "italic", min.size = 0) +
geom_treemap_text(colour = "black", place = "topleft", reflow = T) + theme(legend.position = "null") +
ggtitle("2nd and 3rd Hierarchical Category Levels")

str(train)

# so now let's start with brand analysis
train[, has_brand := (brand_name!='')] %>% 
ggplot(aes(x=cat1, fill=has_brand)) +geom_bar(position='fill',alpha = 0.5, color='black') +xlab('1st Categories') + coord_flip() +
ylab('Proportion') + ggtitle('1st level Category Item - with and without brands')




#Now let's take a look at top 20 most sold brands and their Categories

top20 <- train[brand_name !="", .N, by = list(bName = brand_name)][order(N, decreasing = T)][1:20]



train[brand_name %in% top20$bName] %>% ggplot(aes(x=factor(brand_name, levels=rev(top20$bName)), fill=cat1)) +
geom_bar(width=0.5,alpha=0.5, color='black') + coord_flip() + xlab('Brand') +labs(fill='1st Category') +
ggtitle('Top 20 most sold brands and their Categories') + theme(plot.title = element_text(hjust = 0.5))


Before getting into Item-Description, let us analyze names

#top 20 names mostl used
top20name <- train[name !="", .N, by = list(Name = name)][order(N, decreasing = T)][1:20]



train[name %in% top20name$Name] %>% ggplot(aes(x=factor(name, levels=rev(top20name$Name)))) +
  geom_bar(aes(fill=cat1),width=0.5,alpha=0.7, color='black') + coord_flip() + xlab('Name') +labs(fill='1st Category') +
  ggtitle('Top 20 most used names in 1st level Category') + theme(plot.title = element_text(hjust = 0.5))


options(repr.plot.width=14, repr.plot.height=7)
train[, .(mprice = median(price)), by = category_name][order(mprice, decreasing = TRUE)][1:25] %>%
ggplot(aes(x = reorder(category_name, mprice),y = mprice)) + geom_point(aes(color=category_name,size = mprice)) +
labs(x = '', y = 'Median price',title = 'Median price by item category - top 30') + scale_y_continuous(labels = scales::dollar) + coord_flip()



train[, .(mprice = median(price)), by = brand_name][order(mprice, decreasing = TRUE)][1:30] %>%
ggplot(aes(x = reorder(brand_name, mprice),y = mprice)) + geom_point(aes(color=brand_name,size = mprice)) +
labs(x = '', y = 'Median price',title = 'Median price by brand name - top 30') + scale_y_continuous(labels = scales::dollar) + coord_flip()


train[, .(mprice = median(price)), by = brand_name][order(mprice, decreasing = TRUE)][1:30] %>%
ggplot(aes(x = reorder(brand_name, mprice),y = mprice)) + geom_point(aes(color=brand_name,size = mprice)) +
labs(x = '', y = 'Median price',title = 'Median price by brand name - top 30') + scale_y_continuous(labels = scales::dollar) + coord_flip()



#now we load the  test set
test <- fread('mercaritest.tsv', sep = '\t')

#now here comes the challenge, filling in the zero priced items, these items can significantly have an impact on our model
sum(train$price == 0)

#so there are 874 rows with zero priced items which is a small amount when compared to the dataset, now lets drop them
train <- train[train$price > 0]

sum(train$price == 0)

range(train$price)

# now let's create a new variable log(price) which will help in our analysis later
train$log_price <- log(train$price, base=exp(1))

sum(is.na(test))

#system was damn slow hence performing garbage collection
gc()

colnames(train)[1] <- 'id'

colnames(test)[1] <- 'id'

target.train <- train[,c(6,9)]  #These two columns are the target variables,shall add back later for analysis.


mercari.train <- train[,-c(6,9)]

mercari.df <- rbind(mercari.train, test, fill=TRUE)

#Checking if there is any missing data
colSums(sapply(mercari.df, is.na))

#now let's analyze the item_desc coloumn
mercari_desc <- table(mercari.train[,7])
mercari_desc <- data.frame(mercari_desc)

colnames(mercari_desc) <- c("desc", "freq")
mercari_desc <- mercari_desc[order(mercari_desc$freq, decreasing=TRUE),]
head(mercari_desc)

mercari_desc_cloud <- mercari_desc[c(2:150),] 
#As the wordcloud has limitation on no. of character, you would see some error below

wordcloud(words = mercari_desc_cloud$desc, freq = mercari_desc_cloud$freq, min.freq = 1,
          max.words=500, random.order=FALSE, rot.per=0.1, 
          colors=brewer.pal(7, "Dark2"))

# now let's analyze the same for name
train_name <- table(mercari.train[,2])
train_name <- data.frame(train_name)

colnames(train_name) <- c("desc", "freq")
train_name <- train_name[order(train_name$freq, decreasing=TRUE),]
head(train_name)

train_name_cloud <- train_name[c(2:150),] 
#As the wordcloud has limitation on no. of character, you would see some error below

wordcloud(words = train_name_cloud$desc, freq = train_name_cloud$freq, min.freq = 1,
          max.words=300, random.order=FALSE, rot.per=0.1, 
          colors=brewer.pal(7, "Dark2")) 

#now let's convert item_desc,brand name, category and name to upper case 
mercari.df$item_description <- toupper(mercari.df$item_description)


mercari.df$category_name <- toupper(mercari.df$category_name)

mercari.df$brand_name <- toupper(mercari.df$brand_name)

mercari.df$name <- toupper(mercari.df$name)

#now lets factor them
mercari.df$item_description <- as.factor(mercari.df$item_description)


mercari.df$category_name <- as.factor(mercari.df$category_name)


mercari.df$name <- as.factor(mercari.df$name)

mercari.df$brand_name <- as.factor(mercari.df$brand_name)

colnames(mercari.df)[7]

mercari.df2 <- mercari.df[,-7] # we shall seperate this aside for now
#colnames(mercari.train)

mr.fn <- mercari.df[,c(4,5,7)]


colnames(mr.fn)

class(mr.fn)

mr.fn <- as.data.frame(mr.fn)
colnames(mr.fn)

colnames(mr.fn) <- c('cat','brand','desc')

str(mr.fn)

mr.fn$beauty <- ifelse(grepl("BEAUTY", mr.fn$cat)|grepl("BEAUTY", mr.fn$desc),1,0)
mr.fn$electronic <- ifelse(grepl("ELECTRONIC", mr.fn$cat)|grepl("ELECTRONIC", mr.fn$desc),1,0)
mr.fn$handmade <- ifelse(grepl("HANDMADE", mr.fn$cat)|grepl("HANDMADE", mr.fn$desc)|
                         grepl("DIY", mr.fn$cat)|grepl("DIY", mr.fn$desc),1,0)
mr.fn$home <- ifelse(grepl("HOME", mr.fn$cat)|grepl("HOME", mr.fn$desc)|
                      grepl("HOUSE", mr.fn$cat)|grepl("HOUSE", mr.fn$desc),1,0)
mr.fn$kid <- ifelse(grepl("KID", mr.fn$cat)|grepl("KID", mr.fn$desc)|
                     grepl("BOY", mr.fn$cat)|grepl("BOY", mr.fn$desc)|
                     grepl("GIRL", mr.fn$cat)|grepl("GIRL", mr.fn$desc)|
                     grepl("CHILD", mr.fn$cat)|grepl("CHILD", mr.fn$desc),1,0) 
mr.fn$women <- ifelse((grepl("WOMEN", mr.fn$cat)|grepl("WOMEN", mr.fn$desc)),1,0)
mr.fn$men <- ifelse(((!grepl("WOMEN", mr.fn$cat)) & (grepl("MEN", mr.fn$cat)))|
                     ((!grepl("WOMEN", mr.fn$desc)) & (grepl("MEN", mr.fn$desc))),1,0)
mr.fn$athletic <- ifelse(grepl("ATHLETIC", mr.fn$cat)|grepl("ATHLETIC", mr.fn$desc)|
                          grepl("SPORT", mr.fn$cat)|grepl("SPORT", mr.fn$desc),1,0)
mr.fn$wine <- ifelse(grepl("WINE", mr.fn$cat)|grepl("WINE", mr.fn$desc)|
                      grepl("VINTAGE", mr.fn$cat)|grepl("VINTAGE", mr.fn$desc)|
                      grepl("ALCOCHOL", mr.fn$cat)|grepl("ALCOHOL", mr.fn$desc),1,0)



mr.fn$athletic_apparel <- ifelse(grepl("ATHLETIC APPAREL", mr.fn$cat)|grepl("ATHLETIC APPAREL", mr.fn$desc),1,0)
mr.fn$makeup <- ifelse(grepl("MAKEUP", mr.fn$cat)|grepl("MAKEUP", mr.fn$desc),1,0)
mr.fn$blouse <- ifelse(grepl("BLOUSE", mr.fn$cat)|grepl("BLOUSE", mr.fn$desc),1,0)
mr.fn$shoes <- ifelse(grepl("SHOE", mr.fn$cat)|grepl("SHOE", mr.fn$desc),1,0)
mr.fn$toy <- ifelse(grepl("TOY", mr.fn$cat)|grepl("TOY", mr.fn$desc),1,0)
mr.fn$jewelry <- ifelse(grepl("JEWELRY", mr.fn$cat)|grepl("JEWELRY", mr.fn$desc),1,0)
mr.fn$phone <- ifelse(((!grepl("ACCESSORIES", mr.fn$cat)) & (grepl("PHONE", mr.fn$cat)))|
                        ((!grepl("ACCESSORIES", mr.fn$desc)) & (grepl("PHONE", mr.fn$desc))),1,0)
mr.fn$bag <- ifelse(grepl("BAG", mr.fn$cat)|grepl("BAG", mr.fn$desc),1,0)
mr.fn$dress <- ifelse(grepl("DRESS", mr.fn$cat)|grepl("DRESS", mr.fn$desc),1,0)
mr.fn$pant <- ifelse(grepl("PANT", mr.fn$cat)|grepl("PANT", mr.fn$desc)|
                       grepl("JEAN", mr.fn$cat)|grepl("JEAN", mr.fn$desc),1,0)
mr.fn$accessories <- ifelse(((!grepl("PHONE", mr.fn$cat)) & (grepl("ACCESSORIES", mr.fn$cat)))|
                              ((!grepl("PHONE", mr.fn$desc)) & (grepl("ACCESSORIES", mr.fn$desc))),1,0)
mr.fn$luxury <- ifelse(grepl("WATCH", mr.fn$cat)|grepl("WATCH", mr.fn$desc)|
                         grepl("JEWELRY", mr.fn$cat)|grepl("JEWELRY", mr.fn$desc)|
                         grepl("RING", mr.fn$cat)|grepl("RING", mr.fn$desc)|
                         grepl("BRACELETS", mr.fn$cat)|grepl("BRACELETS", mr.fn$desc),1,0)



str(mr.fn) # Verifying to see if the output obtained is of numeric

mr.fn$tshirt <- ifelse(grepl("T-SHIRT", mr.fn$cat)|grepl("T-SHIRT", mr.fn$desc)|
                         grepl("TSHIRT", mr.fn$cat)|grepl("TSHIRT", mr.fn$desc),1,0)
mr.fn$face <- ifelse(grepl("FACE", mr.fn$cat)|grepl("FACE", mr.fn$desc),1,0)
mr.fn$game <- ifelse(grepl("GAME", mr.fn$cat)|grepl("GAME", mr.fn$desc),1,0)
mr.fn$lip <- ifelse(grepl("LIP", mr.fn$cat)|grepl("LIP", mr.fn$desc),1,0)
mr.fn$eye <- ifelse(grepl("EYE", mr.fn$cat)|grepl("EYE", mr.fn$desc),1,0)
mr.fn$care <- ifelse(grepl("CARE", mr.fn$cat)|grepl("CARE", mr.fn$desc),1,0)
mr.fn$top <- ifelse(grepl("TOP", mr.fn$cat)|grepl("TOP", mr.fn$desc),1,0)

mr.fn$pink <- ifelse(grepl("PINK", mr.fn$brand),1,0)
mr.fn$secret <- ifelse(grepl("VICTORIA'S SECRET", mr.fn$brand)|grepl("VICTORIA'S SECRET", mr.fn$desc),1,0)
mr.fn$nike <- ifelse(grepl("NIKE", mr.fn$brand)|grepl("NIKE", mr.fn$desc),1,0)
mr.fn$apple <- ifelse(grepl("APPLE", mr.fn$brand)|grepl("APPLE", mr.fn$desc)|
                        grepl("MAC", mr.fn$brand)|grepl("MAC", mr.fn$desc)|
                        grepl("IPHONE", mr.fn$desc)|grepl("I PHONE", mr.fn$desc)|
                        grepl("IPAD", mr.fn$desc)|grepl("I PAD", mr.fn$desc),1,0)
mr.fn$lularoe <- ifelse(grepl("LULAROE", mr.fn$brand)|grepl("LULAROE", mr.fn$desc),1,0)



mr.fn$size <- ifelse((grepl("SMALL", mr.fn$desc)|grepl("MEDIUM", mr.fn$desc)|
                        grepl("LARGE", mr.fn$desc)|grepl("XL", mr.fn$desc)|
                        grepl("XS", mr.fn$desc)|grepl("SIZE", mr.fn$desc)|
                        grepl("INCH", mr.fn$desc)),1,0)

mr.fn$color <- ifelse((grepl("RED", mr.fn$desc)|grepl("YELLOW", mr.fn$desc)|
                         grepl("BLUE", mr.fn$desc)|grepl("GREEN", mr.fn$desc)|
                         grepl("WHITE", mr.fn$desc)|grepl("BLACK", mr.fn$desc)|
                         grepl("DULL", mr.fn$desc)|grepl("GREY", mr.fn$desc)|
                         grepl("BROWN", mr.fn$desc)|grepl("PURPLE", mr.fn$desc)|
                         grepl("COLOR", mr.fn$desc)|grepl("COLOUR", mr.fn$desc)),1,0)  

mr.fn$leather <- ifelse(grepl("LEATHER", mr.fn$desc),1,0)

mr.fn$new <- 0 #without age information
mr.fn$new[grepl("NEW", mr.fn$desc)|grepl("NEVER", mr.fn$desc)|
            grepl("BNWT", mr.fn$desc)|grepl("NWT", mr.fn$desc)] <- 2 #New
mr.fn$new[grepl("ONCE", mr.fn$desc)|grepl("TWICE", mr.fn$desc)|
            (grepl("WORN", mr.fn$desc)&(!grepl("NEVER WORN", mr.fn$desc)))] <- 1 #Nearly New

mr.fn$desc <- as.character(mr.fn$desc)
mr.fn$len_des <- 1 #With description shorter or equal to 50 character
mr.fn$len_des[nchar(mr.fn$desc)>60] <- 2 #With description more than 50 character
mr.fn$len_des[grepl("NO DESCRIPTION YET", mr.fn$desc)] <- 0 #no description
mr.fn$desc <- as.factor(mr.fn$desc)

mr.fn$cond <- 0
mr.fn$cond[grepl("GOOD CONDITION", mr.fn$desc)|grepl("GREAT CONDITION", mr.fn$desc)] <- 1
mr.fn$cond[grepl("EXCELLENT CONDITION", mr.fn$desc)|grepl("PERFECT CONDITION", mr.fn$desc)] <-2


mr.fn$tags <- ifelse(grepl("WITH TAG", mr.fn$desc)|grepl("BNWT", mr.fn$desc)|grepl("NWT", mr.fn$desc),1,0)
mr.fn$no_tags <- ifelse(grepl("WITHOUT TAG", mr.fn$desc)|grepl("NO TAG", mr.fn$desc),1,0)



mr.fn$bundle <- ifelse(grepl("BUNDLE", mr.fn$name)|grepl("BUNDLE", mr.fn$desc),1,0)
mr.fn$lularoe <- ifelse(grepl("LULAROE", mr.fn$name)|grepl("LULAROE", mr.fn$desc),1,0)
mr.fn$coach <- ifelse(grepl("COACH", mr.fn$name)|grepl("COACH", mr.fn$desc),1,0)

mr.fn$americaneagle <- ifelse(grepl("AMERICAN", mr.fn$name)|grepl("AMERICAN", mr.fn$desc)|
                                grepl("EAGLE", mr.fn$name)|grepl("EAGLE", mr.fn$desc),1,0)
mr.fn$missme <- ifelse(grepl("MISS ME", mr.fn$name)|grepl("MISS ME", mr.fn$desc),1,0)

mr.fn$reserved <- ifelse((grepl("RESERVED", mr.fn$name)|grepl("RESERVED", mr.fn$desc)),1,0)

mr.fn$onhold <- ifelse(grepl("ON HOLD", mr.fn$name)|grepl("ON HOLD", mr.fn$desc),1,0)

mr.fn$michaekors <- ifelse(grepl("MICHAEL", mr.fn$name)|grepl("MICHAEL", mr.fn$desc)|
                grepl("KORS", mr.fn$name)|grepl("KORS", mr.fn$desc)|
mr.fn$converse <- ifelse(grepl("CONVERSE", mr.fn$name)|grepl("CONVERSE", mr.fn$desc)),1,0)
                                                                                 



dim(mr.fn)

name <- fread('mr.name.csv') 
# we seperate the name coloumn from combined train and test set and feed it into notebook as name and then cbind with mr.fn

name <- name[,-1]

mr.fn <- cbind(mr.fn,name)

mr.fn$bundle <- ifelse(grepl("BUNDLE", mr.fn$name)|grepl("BUNDLE", mr.fn$desc),1,0)
mr.fn$lularoe <- ifelse(grepl("LULAROE", mr.fn$name)|grepl("LULAROE", mr.fn$desc),1,0)
mr.fn$coach <- ifelse(grepl("COACH", mr.fn$name)|grepl("COACH", mr.fn$desc),1,0)

mr.fn$americaneagle <- ifelse(grepl("AMERICAN", mr.fn$name)|grepl("AMERICAN", mr.fn$desc)|
                                grepl("EAGLE", mr.fn$name)|grepl("EAGLE", mr.fn$desc),1,0)
mr.fn$missme <- ifelse(grepl("MISS ME", mr.fn$name)|grepl("MISS ME", mr.fn$desc),1,0)

mr.fn$reserved <- ifelse((grepl("RESERVED", mr.fn$name)|grepl("RESERVED", mr.fn$desc)),1,0)

mr.fn$onhold <- ifelse(grepl("ON HOLD", mr.fn$name)|grepl("ON HOLD", mr.fn$desc),1,0)

mr.fn$michaelkors <- ifelse(grepl("MICHAEL", mr.fn$name)|grepl("MICHAEL", mr.fn$desc)|
                grepl("KORS", mr.fn$name)|grepl("KORS", mr.fn$desc),1,0)
mr.fn$converse <- ifelse(grepl("CONVERSE", mr.fn$name)|grepl("CONVERSE", mr.fn$desc),1,0)
                                                                                

#In cell 57,There are 10 level in the first level of Category variable, 
#we would exclude "Others" and create 9 new variables.

#In cell 58, #There are more than 100 level in the second level of Category variable, 
#we have chosen the most frequent 12 of them and created 12 new variables.

#In cell 60, #There are more than 600 level in the third level of Category variable, 
#we have chosen the most frequent 7 of them and created 7 new variables.

#In cell 61,There are more than 600 level in brand variable
#we have chosen the most frequent 5 of them and created 5 variable

#In cell 62, From the word cloud of "item_description", 
#we have done the same way to choose the most frequent 8 of them and create 8 new variables.

#In cell 63 and 69,From the word cloud and barplot of "name", 
#we have done the same way to choose the most frequent 9 of them and create 9 new variables.
#we cannot create too much variable as it really slows the model down

colnames(mercari.df2)

num_level1 <- length(unique(mercari.df2$name))
levels(mercari.df2$name) <-  1:num_level1
mercari.df2$name <- as.numeric(mercari.df2$name)

num_level2 <- length(unique(mercari.df2$category_name))
levels(mercari.df2$category_name) <-  1:num_level2
mercari.df2$category_name <- as.numeric(mercari.df2$category_name)


num_level3 <- length(unique(mercari.df2$brand_name))
levels(mercari.df2$brand_name) <-  1:num_level3
mercari.df2$brand_name <- as.numeric(mercari.df2$brand_name)


str(mercari.df2)

colnames(mr.fn)

mr.fn2 <- mr.fn[,-c(1,2,3)]

mercari.df3 <- cbind(mercari.df2, mr.fn2)

#removing non-numerical data obtained through binding
mercari.df3 <- mercari.df3[,-48]
mercari.df3 <- mercari.df3[,-57]

summary(mercari.df3)

train_len <- dim(mercari.train)[1]
mercari_len <- dim(mercari.df3)[1]

train_x <- mercari.df3[1:train_len,]
test_x <- mercari.df3[(train_len+1):mercari_len,]

train_final <- as.matrix(train_x, sparse = TRUE) #this is the code to change train dataframe to matrix
test_final <- as.matrix(test_x, sparse = TRUE)

mercari.model <- xgboost(data = train_final, label = target.train$log_price, max.depth = 16, eta = 0.1, print_every_n = 15, nthread = 4, nround = 450, objective = "reg:linear")


mercari.predict <- predict(mercari.model, test_final)

actual.mercari <- fread('mercarisample_submission.csv')

actual <- actual.mercari$price

head(mercari.predict)

RMSLE(exp(mercari.predict),actual)

imp <- xgb.importance (model = mercari.model)
xgb.plot.importance (importance_matrix = imp[1:20])

mercari.predict <- as.data.frame(mercari.predict)

mercari.predict <- exp(mercari.predict)

mercari.final <- cbind(test$id,mercari.predict)

colnames(mercari.final) <- c("test_id", "price")
mercari.final <- data.frame(mercari.final)
mercari.final$test_id <- format(mercari.final$test_id, scientific=FALSE)
write.csv(mercari.final, "mercari.final.csv", row.names = F)
