## Species validation tool 
This tool can be used to validate a list containing trees' or plants' scientific names. Please see the manual (Manual_Species_Validation_Tool_<i>date</i>.PDF) for more information how set up and use the tool.

The input data as CSV format file should contain 
columns with the following names (headers): code, scientific_name<br>
(Note: 'code' is optional)

The data imported to the Species Validation tool are subject to a series of processing and checks.
The various databases are used to validate the inputted species names, and these are accessed either Online and Offline, as follows:
##### 1) LCVP - Leipzig Catalogue of Vascular Plants [offline] (see the Manual how download the database)
##### 2) Tropicos - Missouri Botanical Garden [online]
##### 3) Kew  - Plants of the World Online [online]
##### 4) NCBI - National Center for Biotechnology Information, db="taxonomy" [online]
##### 5) WFO  - World Flora Online [offline] (see the Manual how download the database)
##### 6) GBIF - Global Biodiversity Information Facility [online]
##### 7) GBIF - IUCN Red List status search [online], (no results for all species!)
##### 8) GTS - Global Tree Search [offline]. (Used to check name occurrences in this DB, not  actually to validate names)

![image](https://user-images.githubusercontent.com/37068938/113573498-8931f200-961a-11eb-8652-71c461f8e396.png)

![image](https://user-images.githubusercontent.com/37068938/113573321-32c4b380-961a-11eb-9a6e-dce19d56359b.png)

![image](https://user-images.githubusercontent.com/37068938/113573389-5ab41700-961a-11eb-8e5a-6f5f96cbd610.png)


### Validation Results
The result table shows the information about the species. 
This table contains several fields, as "code" (input),"scientific_name" (input),"Authors","Status","Accepted_Taxon", "iucnRL_scientific_name","iucnRL_Accepted_Taxon","iucnRL_Alternative","family"...... etc. 

![image](https://user-images.githubusercontent.com/37068938/113574540-99e36780-961c-11eb-8a45-9307dc1c9b87.png) 

#### Statistic Table
![image](https://user-images.githubusercontent.com/37068938/113574826-2b52d980-961d-11eb-9b92-d4a362e469d3.png)
