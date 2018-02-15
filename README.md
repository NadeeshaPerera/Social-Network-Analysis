# Social-Network-Analysis
Objective:
Perform social network analysis on anonymized email data from a large European research institute and display the results in a dashboard built using R shiny. 

Data files for the analysis are attached herewith:
1. email-Eu-core - This file contains a list links - the sender id and receiver id of each of the internal emails
2. email-Eu-core-department-labels - This file contains the employee id and the department id of all the employees

Details regarding the network:
There is an edge (u, v) in the network if person u sent person v at least one email. 
The e-mails only represent communication between institution members (the core), and the dataset does not contain incoming messages from or outgoing messages to the rest of the world.
Each individual belongs to exactly one of 42 departments at the research institute. 

Centrality measures calculated:
1. Degree centrality
2. Betweenness centrality
3. In-degree centrality

