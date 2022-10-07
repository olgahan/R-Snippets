
#Example 1
#Preprocessing for sentiment analysis at sentence level
all = all %>% 
  unnest_tokens(sentence, text, token = "sentences")

get_sentiments("bing")

all_sent = all$sentence %>% 
  get_sentences() %>% 
  sentiment()

all_sent = aggregate(all_sent$sentiment, 
                   by = list(Category = all_sent$element_id), FUN = sum)

all = cbind(all,all_sent)

all$Category = NULL

colnames(all)[which(names(all) == "x")] = "sentiment"

#Finding texts with certain keywords (FATF and nominee directors)

#This includes texts with nominee AND director
all$nomdir = grepl("nomi.*dir|dir.*nomi", tolower(all$text))

#This includes texts with task force OR fatf
all$fatf = grepl("task force|fatf", tolower(all$text))



#Example 2
#Most frequently used positive and negative words for terrorism treatment:
tokall_terror = tokall[ which(tokall$terrorismtreat==1), ]

terror_sentiment <- tokall_terror %>%
  inner_join(get_sentiments("bing")) %>%
  count(country, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#percentage of sentiment 
terror_sentiment$percentage=terror_sentiment$sentiment/(terror_sentiment$negative+terror_sentiment$positive)

#most positive 15
terror_sentiment_ordered <- arrange(terror_sentiment, -percentage)
terrorpositivetop15 <- terror_sentiment_ordered[1:15,]
ggplot(terrorpositivetop15, aes(x = percentage, y = reorder(country,percentage))) + theme_bw() + geom_bar(stat = "identity")

#most negative 15
terror_sentiment_negordered = arrange(terror_sentiment, percentage)
terrornegativetop15 = terror_sentiment_negordered[1:15,]
ggplot(terrornegativetop15, aes(x = percentage, y = reorder(country,-percentage))) + theme_bw() + geom_bar(stat = "identity")

#plot most frequent positive and negative words
terror_sentiment_counts = tokall_terror %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

terror_sentiment_counts

terror_sentiment_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ggplot(aes(reorder(word, n), n, fill = sentiment)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

#Example 3

#Subgroup Analysis for OECD countries:

bankt_oecd <- bankt[ which(bankt$oecd==1), ]

bankt_oecd_sentiment <- bankt_oecd %>%
  inner_join(get_sentiments("bing")) %>%
  count(Country, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bankt_oecd_sentiment$percentage = bankt_oecd_sentiment$sentiment /
  (bankt_oecd_sentiment$negative + bankt_oecd_sentiment$positive)

#most positive 15 countries within oecd:
bankt_oecd_sentiment_ordered = arrange(bankt_oecd_sentiment, -percentage)
bankt_oecd_positivetop15 = bankt_oecd_sentiment_ordered[1:25,]
png(file = "bankt_oecd_positivetop15.png")
ggplot(bankt_oecd_positivetop15, aes(x = percentage, y = reorder(Country,percentage))) + theme_bw() + geom_bar(stat = "identity")
dev.off()

#Example 4
#similarity across bank responses
#purpose: we want to see whether a bank sends similar responses to different requests
#with subtle messages of corruption, terrorism etc.
#each bank is approached 3 times. 
#strategy: calculate levenshtein similarity score across responses.

#first part is dealing with NAs in responses. 
df = df[!is.na(df$translatedreplies.1) & !is.na(df$translatedreplies.2) |
           !is.na(df$translatedreplies.1) & !is.na(df$translatedreplies.3) |
           !is.na(df$translatedreplies.2) & !is.na(df$translatedreplies.3), ]

df$na_count = apply(df, 1, function(x) sum(is.na(x)))

#I calculate length of responses to use for weighting later.
clength=as.data.frame(cbind(nchar(df$translatedreplies.1),
                            nchar(df$translatedreplies.2),
                            nchar(df$translatedreplies.3)))

#to weight by average length
df$mean = rowMeans(clength, na.rm = TRUE)

clength[is.na(clength)] = 0

#to weight by maximum length
df$max = apply(X = clength, MARGIN = 1, FUN = max)

#similarity score across responses weighted by maximum length
wmax = sum(df$max*(rowSums(cbind(levenshteinSim(df$translatedreplies.1, df$translatedreplies.2),
                               levenshteinSim(df$translatedreplies.1, df$translatedreplies.3),
                               levenshteinSim(df$translatedreplies.3, df$translatedreplies.2)),
                         na.rm = TRUE)/(3-df$na_count)))/sum(df$max)

bank1 = rowSums(cbind(levenshteinSim(df$translatedreplies.1, df$translatedreplies.2),
                    levenshteinSim(df$translatedreplies.1, df$translatedreplies.3),
                    levenshteinSim(df$translatedreplies.3, df$translatedreplies.2)),
              na.rm = TRUE)/(3-df$na_count)

#similarity score across responses weighted by average length
wmean = sum(df$mean*(rowSums(cbind(levenshteinSim(df$translatedreplies.1, df$translatedreplies.2),
                                 levenshteinSim(df$translatedreplies.1, df$translatedreplies.3),
                                 levenshteinSim(df$translatedreplies.3, df$translatedreplies.2)),
                           na.rm = TRUE)/(3-df$na_count)))/sum(df$mean)

bp=as.data.frame(rbind(wmax,wmean))

#plot
png(file="banksimilarity.png")

barplot(bp$V1, main="Similarity for Banks Across Rounds", xlab="",ylim = c(0,0.3),  
        ylab="Average Similarity", names.arg=c("Weighted by Max","Weighted by Mean"), 
        border="blue")

dev.off()

#Example 5 
#job application on behalf of experiment confederates.
#I want to achieve the following:
#each job is applied through and intermediary. we can apply for max 10 jobs per intermediary.
#I want to include highest paying jobs within each intermediary as much as possible.
#

#RAs will apply for jobs some time after I send them the code and data. Hence, 
#they need to get rid of jobs with passed deadlines. I define time variables for that purpose.
#they can include more days if necessary.
t = format(t, format="%d-%b-%Y")
t1 = format(t1, format="%d-%b-%Y")
t2 = format(t2, format="%d-%b-%Y")
#t3 = format(t3, format="%d-%b-%Y")
#t4 = format(t4, format="%d-%b-%Y")
subset(df, df$deadline != t & df$deadline != t1 & df$deadline != t2)

#salary ranking (in Spanish)
#20 SMMLV en adelante
#6 a 9 SMMLV
#4 a 6 SMMLV
#2 a 4 SMMLV

#I create batches that make sure we include highest salary jobs as possible.
dfjonatan$batch =  ifelse(dfjonatan$`salary` == '20 SMMLV en adelante', 1, 
                          ifelse(dfjonatan$`salary` == '6 a 9 SMMLV', 2, 
                                 ifelse(dfjonatan$`salary` == '4 a 6 SMMLV', 3, 
                                        ifelse(dfjonatan$`salary` == '2 a 4 SMMLV', 4,
                                               ifelse(dfjonatan$`contract type` == 'Termino Fijo' | 
                                                        dfjonatan$`contract type`== 'Termino Indefinido', 5, 
                                                      ifelse(dfjonatan$`min education` == "Ninguno" | dfjonatan$`min education`== 'Básica Primaria(1-5)' | 
                                                               dfjonatan$salary == 'Menos de 1 SMMLV' | dfjonatan$salary == '1 SMMLV', 7, 6))))))

#first, I choose number of jobs for each intermediary. if n > 10, I choose highest paid ones.
#if n < 10, I choose all. 


df = df %>% 
  group_by(`prestadores asociados`) %>% 
  mutate(countint = n())

df$id = runif(nrow(df), 1, nrow(df))
df = df %>% 
  group_by(`prestadores asociados`) %>% 
  arrange(batch, id) 

df1=subset(df, countint<10)

df2=subset(df, countint>10)

df2 = df2 %>% 
  group_by(`prestadores asociados`) %>% 
  slice(1:10) 

#then, I order these n's for each intermediary. the order is important for the experiment.

df=rbind(df1,df2)

df=df %>% group_by(`prestadores asociados`) %>% mutate(order = sample(row_number()))

df$id=NULL
df$batch=NULL
df$countint=NULL

#now, I randomly assign treatments with different weights. control group (Z000) will take 
#.225 of jobs, and the rest will be divided equally among treatments.

m_each=c(rep(.775/11, 11), .225)

n=nrow(df)
m_each

#set seed so that the randomization is replicable
set.seed(1)

#declare randomization with parametes defined above
declaration <- declare_ra(N = n, blocks = df$order,
                          prob_each = m_each)

Z = conduct_ra(declaration = declaration)

levels(Z) = c("Z1000","Z0100","Z0001","Z1100","Z1001","Z0101",
              "Z1101","Z1010","Z1110","Z1011","Z1111","Z0000")

df$Z = Z

#reorder columns

df = df %>%
  relocate(code)
df = df %>%
  relocate(Z) 

#reoder factors in treatment variable, which is apparently more convenient for RAs when applying

order_v = c('Z0000', 'Z0001', 'Z1000', 'Z1001', 'Z1010', 'Z1011', 'Z1110', 
                 'Z1111', 'Z1100', 'Z1101', 'Z0100', 'Z0101')

df = df[order(sapply(df$Z, function(x) which(x == order_v))), ]


