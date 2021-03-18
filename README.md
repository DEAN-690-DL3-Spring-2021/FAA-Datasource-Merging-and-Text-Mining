# Datasource Combination and Text Mining
The project aims to form the mechanism of data interaction and sharing between heterogeneous data sources in an elaborate system environment. The known issues that need to be addressed to accomplish the project’s objective are the data quality issues, duplicate data, scalability, multiple copies of record from several data sources that leads to unreliable data integration at the end. Therefore, the project study will puzzle out the major challenges of data merging, and therefore basically merge the data from disparate data sources into a single unified view in order to conduct the centralized analysis of the new data set to uncover the valuable insight. 

# Problem Statement 
The Federal Aviation Administration uses numerous data repositories to document aviation related events such as aircraft service difficulty reports, aircraft incidents and accidents reports, and emergency operations network reports, among many others. Since these data are collected and entered by subject matter experts in various lines of business across various times, they are collected and saved in different databases. The databases are not connected and a complete and detailed picture in a unified space does not exist for aviation safety investigation. Having all this data in a unified system is crucial for prompt investigation to identify patterns and hidden cause-effect links. The FAA wishes to combine and merge these disparate data to form a unified view of aviation-related events, and prioritize hazards to prevent any future aviation-related failures, accidents, and incidents in order to maintain high standards of aviation safety.

# Alogorithm: Fuzzy Matching, Natural Language Processing, Topic Modeling 
Fuzzy join also known as  similarity join is a binary operation that takes two sets of elements as input and computes a set of similar element-pairs as output . Instead of marking out records as a ‘match’ or ‘non-match’, fuzzy matching identifies the probability that two records actually match based on whether they agree or disagree on the various identifiers. This is different compared to exact joins where records are matched based on the common keys. Fuzzy matching allows us to identify non-exact matches of target items. Generally, fuzzy matching is an algorithm for linking text to similar text.

# Requirements
Minimum requirements:

* Python 2.7+
* NumPy 1.10+
* Chainer 1.5.1+
* spaCy 0.99+
* Requirements for some features:

* CUDA support
* Testing utilities: py.test

# Related Links from IEEE
https://ieeexplore-ieee-org.mutex.gmu.edu/document/687477
https://ieeexplore-ieee-org.mutex.gmu.edu/document/616348
