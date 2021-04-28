# Datasource Combination and Text Mining
The project aims to form the mechanism of data interaction and sharing between heterogeneous data sources in an elaborate system environment. The known issues that need to be addressed to accomplish the project’s objective are the data quality issues, duplicate data, scalability, multiple copies of record from several data sources that leads to unreliable data integration at the end. Therefore, the project study will puzzle out the major challenges of data merging, and therefore basically merge the data from disparate data sources into a single unified view in order to conduct the centralized analysis of the new data set to uncover the valuable insight. In addtion, the project will explore the insight extraction by using Structural Topic Modelling (STM) which is one of the Natural Language Processing (NLP) to highlight the key information from text corpus which is the remark column. 

# Problem Statement 
The Federal Aviation Administration uses numerous data repositories to document aviation related events such as aircraft service difficulty reports, aircraft incidents and accidents reports, and emergency operations network reports, among many others. Since these data are collected and entered by subject matter experts in various lines of business across various times, they are collected and saved in different databases. The databases are not connected and a complete and detailed picture in a unified space does not exist for aviation safety investigation. Having all this data in a unified system is crucial for prompt investigation to identify patterns and hidden cause-effect links. The FAA wishes to combine and merge these disparate data to form a unified view of aviation-related events, and prioritize hazards to prevent any future aviation-related failures, accidents, and incidents in order to maintain high standards of aviation safety.

# Alogorithm: Fuzzy matching alogorithm and Structural Topic Modeling (STM) 
Fuzzy join also known as  similarity join is a binary operation that takes two sets of elements as input and computes a set of similar element-pairs as output . Instead of marking out records as a ‘match’ or ‘non-match’, fuzzy matching identifies the probability that two records actually match based on whether they agree or disagree on the various identifiers. This is different compared to exact joins where records are matched based on the common keys. Fuzzy matching allows us to identify non-exact matches of target items. Generally, fuzzy matching is an algorithm for linking text to similar text.

Structural Topic Modelling (STM) is a model which is used to estimate a topic model and determine its relationship to document-level metadata. It is an algorithm for extracting topics from a collection of documents. It is a widely used text mining method in Natural Language Processing to gain insights about the text documents. A single document is often linked to many topics. At the same time, topics are present in many documents. Topic modeling allows us to organize and study the aviation report narratives from the text columns in our datasets. STM provides a comprehensive understanding of which variables are linked with which features of the text. The document-topic and topic-word distributions yield documents that have metadata associated with them. A topic is a combination of words that probably belong to the associated topic and a document is a mixture of multiple topics. The topic proportions for all topics within a document sum to one and the sum of word probabilities for each topic is also one. Topical prevalence helps determine how much of the given document is associated with a particular topic. On the other hand, topical content refers to the words that make up a topic.

# STM parallel workflow diagram
The diagram below represents the parallel workflow in the STM algorithm. The STM algorithm ingests the text or description column along with the associated metadata and it will pass it to textProcessor and then to a utility function (prepDocuments). This function automatically removes infrequent terms based on the set parameters. The output documents and metadata will be used for analysis and the results will be estimated for both topical prevalence and topical contents. The next step as shown in the STM parallel workflow diagram, will be the model selection and search, and understanding the topics and labeling them. Finally the results can be visualized based on topics’ prevalence
![STM diagram](https://user-images.githubusercontent.com/61568065/116450855-cbb9a980-a829-11eb-98da-c36dd07dd2f2.PNG)

# Latent Dirichlet Allocation (LDA) Theory and Assumptions
LDA stands for Latent Dirichlet Allocation. As time is passing by, data is increasing exponentially. Most of the data is unstructured and a few of them are unlabeled. It is a tedious task to label each and every data manually. How can we label such a huge amount of data if not manually? Here comes the LDA to our rescue. LDA is one of the topic modeling techniques which is used to analyze a huge amount of data, cluster them into similar groups, and label each group. It should be noted that LDA technique is for unsupervised learning which is used to label the data by grouping them into similar topics. Unlike K-Means clustering and other clustering techniques which uses the concept of distance between cluster center, LDA works on the probability distribution of topics belonging to the document.

(1) Topics are probability distribution over words:
![github TM assumption 1](https://user-images.githubusercontent.com/61568065/112349648-4a597f00-8c9f-11eb-818e-38de993f9ce6.PNG)
(2)Documents are probability distribution over topics:
![github TM assumption 2](https://user-images.githubusercontent.com/61568065/112354604-3ca5f880-8ca3-11eb-9d2b-f4f3115d4b1b.PNG)

# pyLDAVis
pyLDAVis is the most commonly used and a nice way to visualise the information contained in a topic model. Below is the implementation for LdaModel(). The visulization demonstrates the keywords of the topics and showing frequency of the key works for each topic. The follwing visualizations display the top 40 topics and top 20 topics.
![LDA Visual Screen Record](https://user-images.githubusercontent.com/61568065/112412974-ca123880-8cf5-11eb-9db8-5b9dd0e83e18.gif)
![@nd LDA Visual](https://user-images.githubusercontent.com/61568065/112413700-06926400-8cf7-11eb-9ce4-d74896fb9f6b.gif)



# Requirements
Minimum requirements:

* Python 2.7+
* NumPy 1.10+
* Chainer 1.5.1+
* spaCy 0.99+

Requirements for some features:
* CUDA support
* Testing utilities: py.test


