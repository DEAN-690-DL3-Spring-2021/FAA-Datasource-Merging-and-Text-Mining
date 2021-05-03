![AID vis 1](https://user-images.githubusercontent.com/61568065/116832596-91327280-ab83-11eb-8dcf-8975d0dba469.png)
![AID vis 1](https://user-images.githubusercontent.com/61568065/116832602-998aad80-ab83-11eb-8bed-4b4bd6ce6cc3.png)
# Federal Aviation Administration (FAA) Data source Combination and Text Mining
The project aims to form the mechanism of data interaction and sharing between heterogeneous data sources in an elaborate system environment. The known issues that need to be addressed to accomplish the project’s objective are the data quality issues, duplicate data, scalability, multiple copies of record from several data sources that leads to unreliable data integration at the end. Therefore, the project study will puzzle out the major challenges of data merging, and therefore basically merge the data from disparate data sources into a single unified view in order to conduct the centralized analysis of the new data set to uncover the valuable insight. In addtion, the project will explore the insight extraction by using Structural Topic Modelling (STM) which is one of the Natural Language Processing (NLP) to highlight the key information from text corpus which is the remark column. 

# Problem Statement 
The Federal Aviation Administration uses numerous data repositories to document aviation related events such as aircraft service difficulty reports, aircraft incidents and accidents reports, and emergency operations network reports, among many others. Since these data are collected and entered by subject matter experts in various lines of business across various times, they are collected and saved in different databases. The databases are not connected and a complete and detailed picture in a unified space does not exist for aviation safety investigation. Having all this data in a unified system is crucial for prompt investigation to identify patterns and hidden cause-effect links. The FAA wishes to combine and merge these disparate data to form a unified view of aviation-related events, and prioritize hazards to prevent any future aviation-related failures, accidents, and incidents in order to maintain high standards of aviation safety.

# Alogorithm: Fuzzy matching alogorithm and Structural Topic Modeling (STM) 
Fuzzy join also known as  similarity join is a binary operation that takes two sets of elements as input and computes a set of similar element-pairs as output . Instead of marking out records as a ‘match’ or ‘non-match’, fuzzy matching identifies the probability that two records actually match based on whether they agree or disagree on the various identifiers. This is different compared to exact joins where records are matched based on the common keys. Fuzzy matching allows us to identify non-exact matches of target items. Generally, fuzzy matching is an algorithm for linking text to similar text.

Structural Topic Modelling (STM) is a model which is used to estimate a topic model and determine its relationship to document-level metadata. ISTM provides a comprehensive understanding of which variables are linked with which features of the text. The document-topic and topic-word distributions yield documents that have metadata associated with them. A topic is a combination of words that probably belong to the associated topic and a document is a mixture of multiple topics. The topic proportions for all topics within a document sum to one and the sum of word probabilities for each topic is also one. Topical prevalence helps determine how much of the given document is associated with a particular topic. On the other hand, topical content refers to the words that make up a topic.

# STM parallel workflow diagram
The diagram below represents the parallel workflow in the STM algorithm. The STM algorithm ingests the text or description column along with the associated metadata and it will pass it to textProcessor and then to a utility function (prepDocuments). This function automatically removes infrequent terms based on the set parameters. The output documents and metadata will be used for analysis and the results will be estimated for both topical prevalence and topical contents. The next step as shown in the STM parallel workflow diagram, will be the model selection and search, and understanding the topics and labeling them. Finally the results can be visualized based on topics’ prevalence
![STM diagram](https://user-images.githubusercontent.com/61568065/116450855-cbb9a980-a829-11eb-98da-c36dd07dd2f2.PNG)

# Top 25 topic by prevalence in SDR, AID and EON
The visulizations to understand the topic prevalence in the text corpus of each dataset, and which words contribute to each topic.
![AID vis 1](https://user-images.githubusercontent.com/61568065/116832612-a14a5200-ab83-11eb-8194-08a7d7b45d27.png)
![AID vis 3](https://user-images.githubusercontent.com/61568065/116832613-a5766f80-ab83-11eb-9e7d-1a7b19dfa870.png)
![EON vis 2](https://user-images.githubusercontent.com/61568065/116832618-a7d8c980-ab83-11eb-8f5e-b97fba3d8077.png)



# Requirements
Minimum requirements:

Python
* Python 2.7+
* NumPy 1.10+
* Fuzzy matcher

R 
* STM
* TM
* Stminsights
* Lubridate
* wordclod



