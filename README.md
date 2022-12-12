# Policy-Implications-of-State-of-Union-Speeches

Final Project by Minha Kim and Ruqaya. 



We worked on text analysis from interesting data we found online on State of the Union speeches of American Presidents for over 200 years, from 1790 to 2018. From there, we analyzed academic articles, read transcripts and also listened to the SOTU speeches of a few presidents. 
 
So for those of us who don’t know, one of purposes of the SOTU speeches is to address Congress in the hopes that bills will be passed. So, Presidents mention the needs of the American people during that speech, and highlight the progress and achievements of the nation as well.  
 
But the main objective is the speech is to get the people of Congress to pass bills, for example, finance bills that will impact the American economy for the next few years. 
 
So, our RQ is Which national policies were Democratic and Republican Presidents more oriented towards during State of Union addresses from 1790 - 2018?  
1.         Education 
2.         Economy
3.         Infrastructure 
4.         Healthcare 
5.         Security 

We analyzed our data using different types of supervised and unsupervised ML and found that Economic, Security, and Education policies were the main focus of US Presidents during the SOTU speeches. We further analyzed these three categories by listing specific keywords and measured the impact these had on each political party and US President. 
 
Approach was layering and widening; meaning that worked on tools in supervised and unsupervised ML methods and expanding on each using different features and interpreting results 
◦            We also conducted data cleaning and EDA to get more meaningful topics
◦            And from there, we selected the relevant words for our topic model lists; for example in the topic model of economy, we selected economic, lower taxes, ports, immigration restriction, conservatism, deregulation, capitalism, corporate tax cuts, industrial. 
◦            And in security, trafficking, drugs, terrorism, shootings, firearms, border. 
 
Moreover, we picked the words included in the topic models based on the model  these on the number of times a word was repeatedly used in any of the 5 categories in their speeches. This was determined from models such as LDA; 




In the first section, we have run libraries as well as the url for the srapped Github SOTU Speeches.
library('tidyverse')
library('data.table')
library('tidytext')
library('randomForest')
library('caret')
library('nnet')
library('topicmodels')
library('quanteda')
library('caret')
library('GGally')




url <- 'https://raw.githubusercontent.com/BrianWeinstein/state-of-the-union/master/transcripts.csv'

df <- read.csv(url)
a <- str_length(df$transcript[1:5])


url <- 'https://gist.githubusercontent.com/namuol/2657233/raw/74135b2637e624848c163759be9cd14ae33f5153/presidents.csv'
president <- read.csv(url) %>%
  select(president = President, party = Party) %>%
  bind_rows(data.frame(president = c('Donald J. Trump', 'William J. Clinton', 'George Bush', 'Gerald R. Ford', 'Martin van Buren'), 
                       party = c('Republican', 'Democratic', 'Republican', 'Republican', 'Democratic')))

tail(president, 10)



*In the second section, we have divided the dataset into 5000, 8000, and 10,000 words respectively, created a bag of words, and analyzed which words were relevant to our policy-oriented categories.

speech <- df %>%
  left_join(president) %>%
  mutate(id = 1:nrow(.)) %>%
  mutate(transcript = substr(transcript, 1, 5000)) %>%
  mutate(party = str_trim(party)) %>%
  filter(party %in% c('Democratic', 'Republican')) %>%
  select(id, president, party, transcript)

head(speech)

speech %>%
  count(party)


* We have then removed the punctuations from the speeches: 

words <- speech %>%
  mutate(transcript = gsub("[[:punct:]]", " ", transcript)) %>%
  unnest_tokens(word, transcript, strip_punct=T) %>%
  filter(!grepl('[0-9]', word)) %>%
  anti_join(stop_words)

head(words)
words %>%
  count(word) %>%
  arrange(-n) %>%
  head(10)

* And included Bag of Words (BoW), which is a technique in text mining, used to analyze the script based on word count. 



* From there, these three steps are repeated three times. The next two steps will use 8000 words and 10000 words respectively. Keep this step in mind because we will label Bag of Words as bow1, bow2, and bow3 and apply it in the supervised and unsupervised ML techniques. 


In section 3, we split data and created random forest model using randomForest library. See below: 

set.seed(24)
library(randomForest)
rf <- randomForest(labels ~., data = train1, ntree=30, mtry=30)
y_pred <- predict(rf, newdata = test1)
confusionMatrix(y_pred, test1$labels)


rf$importance %>%
  as.data.frame() %>%
  mutate(feature = row.names(.)) %>%
  arrange(-MeanDecreaseGini) %>%
  head(10) %>%
  ggplot(aes(y=reorder(feature,MeanDecreaseGini) , x=MeanDecreaseGini)) + geom_col() +
  xlab('Importance') + ylab('Feature')

We have repeated this process 3 times, with the first model using a 30 no. of trees and 30 no. of variables, 50, and 100 


This model shows that the data is 60% accurate. Although it’s a low percent, it is substantial because the data was transcribed from speeches as far back as the 18th century. 

 


* We have also conducted PCA models for feature reduction. The PCA model shows the most words used by both Democratic and Republican Presidents. Although most words are used by both parties, by analyzing PCA models 2 and 3, we see that some words are used by one party of the other.

This section does not show which words each party uses and which words are clustered on specific areas of the model (which indicates higher prevalence), but gives us a visualization of the model.


pc <- prcomp(bow %>% select(-labels), center = T, scale. = T)


data.frame(pc1 = pc$x[,1], pc2 = pc$x[,2], label=bow$labels) %>%
  ggplot(aes(x=pc1, y=pc2, color=label)) + geom_point() +
  scale_color_manual(values=c('skyblue', 'salmon'))


pc2 <- prcomp(bow2 %>% select(-labels), center = T, scale. = T)

data.frame(pc1 = pc2$x[,1], pc2 = pc2$x[,2], label=bow$labels) %>%
  ggplot(aes(x=pc1, y=pc2, color=label)) + geom_point() +
  scale_color_manual(values=c('skyblue', 'salmon'))


pc3 <- prcomp(bow3 %>% select(-labels), center = T, scale. = T)

data.frame(pc1 = pc3$x[,1], pc2 = pc3$x[,2], label=bow$labels) %>%
  ggplot(aes(x=pc1, y=pc2, color=label)) + geom_point() +
  scale_color_manual(values=c('skyblue', 'salmon'))



* This section uses Linear Discriminant Analysis (LDA) which is used for topic modeling. Our models show the top 10 words used in 5 topics. In the model, we see that economic and security policies (with words such as debt, labor, united, industrial, war,and peace) are the focus of US Presidents during their State of Union address, and this will help us with the list of topic words for each category
mylda <- LDA(bow %>% select(-labels), k = 5, control=list(seed=123)) %>%
  tidy(matrix = 'beta')

mylda %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill=factor(topic))) + 
  geom_col(show.legend = F) + facet_wrap(~topic, scales='free') +
  scale_y_reordered() +
  xlab('') + ylab('')


mylda %>%
  mutate(topic = paste0('topic', topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>%
  mutate(log_ratio = log2(topic2/topic1)) %>%
  arrange(-abs(log_ratio)) %>%
  head(20) %>%
  ggplot(aes(y=reorder(term, log_ratio), x=log_ratio)) + geom_col()




 
 topic_words <- list(healthcare = c('healthcare', 'medical', 'insurance', 'cancer', 
                                    'covid', 'diabetes', 'mental health'),
                     economy = c('economy','economic','tax','economic policy', 
                                'union', 'tax cut', 'deficits' ),
                     infrastructure = c('infrastructure', 'road', 'railroad', 
                                        'energy infrastructure', 'transport'),
                     education = c('education', 'school', 'highschool', 'degree', 
                                   'teaching', 'learning', 'bullying', 'trauma'), 
                     security = c('security', 'illicit drugs', 'traffickers', 'drugs',
                                  'terrorism', 'guns', 'firearms', 'shootings'))
 
 
 
 topic_vals <- c()
 words_vals <- c()
 for (top in names(topic_words)){
   topic_vals <- c(topic_vals, rep(top, length(topic_words[[top]])))
   words_vals <- c(words_vals, topic_words[[top]])
 }
 
 topic_df <- data.frame(word = words_vals, topic=topic_vals)
 topic_df
 
 
 words %>%
   inner_join(topic_df) %>%
   count(topic, party) %>%
   ggplot(aes(x=n, y=reorder(topic, n), fill=party)) + geom_col(position='dodge') +
   scale_fill_manual(values=c('skyblue', 'salmon')) +
   xlab('Count') + ylab('Topic') + 
   ggtitle('Words by Party') + 
   theme(plot.title = element_text(hjust=.5))
 
 
 words %>%
   inner_join(topic_df) %>%
   count(topic, president) %>%
   pivot_wider(id_cols=president, values_from = n, names_from = topic, 
               values_fill=0)
 




* There is a slight increase in both Democratic and Republican Party after the inclusion of the word 'industrial'Presidents that focused on these topics were Obama, Calvin Coolidge, Eisenhower, Ford, George W.Bush, Nixon, Reagan, Truman, and Carter. It is also important to note that earlier Presidents have used the term 'industrial' in their speeches, implying that this was the focus on economic growth during the early 1900s. These Presidents include Roosevelt, Hoover, Truman, and Eisenhower

topic_words3 <- list(economy = c('economy','economic', 'tax', 'economic policy',
                                 'union', 'deficits', 'lower taxes', 
                                 'immigration restriction','conservatism', 
                                 'deregulation', 'capitalism',
                                 'corporate tax cuts', 'industrial'))

topic_vals3 <- c()
words_vals3 <- c()
for (top in names(topic_words)){
  topic_vals3 <- c(topic_vals3, rep(top, length(topic_words3[[top]])))
  words_vals3 <- c(words_vals3, topic_words3[[top]])
}

topic_df3 <- data.frame(word = words_vals3, topic=topic_vals3)
topic_df3

words %>%
  inner_join(topic_df3) %>%
  count(topic, party) %>%
  ggplot(aes(x=n, y=reorder(topic, n), fill=party)) + 
  geom_col(position='dodge') +
  scale_fill_manual(values=c('skyblue', 'salmon')) +
  xlab('Count') + ylab('Topic') + 
  ggtitle('Words by Party') + 
  theme(plot.title = element_text(hjust=.5))

words %>%
  inner_join(topic_df3) %>%
  count(topic, president) %>%
  pivot_wider(id_cols=president, values_from = n, names_from = topic, 
              values_fill=0)




* This model shows that Democrats spoke more about security policies, than Republicans. When the word 'border' was included in our topic model, we do not see substantial difference from the first model

topic_words5 <- list(security = c('security', 'illicit drugs', 'traffickers', 
                                  'drugs', 'terrorism', 'guns', 'firearms', 
                                  'shootings' , 'border'))


topic_vals5 <- c()
words_vals5 <- c()
for (top in names(topic_words)){
  topic_vals5 <- c(topic_vals5, rep(top, length(topic_words5[[top]])))
  words_vals5 <- c(words_vals5, topic_words5[[top]])
}

topic_df5 <- data.frame(word = words_vals5, topic=topic_vals5)
topic_df5

words %>%
  inner_join(topic_df5) %>%
  count(topic, party) %>%
  ggplot(aes(x=n, y=reorder(topic, n), fill=party)) + 
  geom_col(position='dodge') +
  scale_fill_manual(values=c('skyblue', 'salmon')) +
  xlab('Count') + ylab('Topic') + 
  ggtitle('Words by Party') + 
  theme(plot.title = element_text(hjust=.5))

words %>%
  inner_join(topic_df5) %>%
  count(topic, president) %>%
  pivot_wider(id_cols=president, values_from = n, names_from = topic, 
              values_fill=0)

