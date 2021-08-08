
# Red Team/Blue Team for COVID-19 Simulation and Modeling

This repo contains code and information for researchers interested in either simulating COVID-19 data (Red Team) or modeling that data (Blue Team). The hope is that by creativly exploring possible data generating processes (DGP) and developing corresponding models we will have more robust scientific understanding of the COVID-19 outbreak.

# Red Team Role

The Red Team develops data generating processes (DGPs) that simulate aspects of disease dynamics at the population level. These models are necessarily oversimplifications of what is actually happening in the world but they can help explore the dynamics of an oubreak. 

Stage 1: The current DGP is a simple SIRTD with an interactive iterface [https://blooming-lake-98194.herokuapp.com/](https://blooming-lake-98194.herokuapp.com/) model that will also generate tweets. The model loosely aligns with historical data from Brazil for deaths on the initial paramterization and there are sliders to try other values with csv output available for copying for experimenting with.  

Stage 2: Eventually the red team will create different models with different dynamics. The details of how transparent the intitial presentation remains to be decided but we anticipate that components like vaccination, age, location etc.. are all candidates for future simulations. 

# Blue Team Role

The Blue Team models the simulated COVID outbreak and assess performance by various to-be-agreed-upon metrics. 

# Software

In addition to the above SIRTD DGP above we have a simple script for running various configurations of simulations/real data/models in the same evaluation harness with some options to view graphs, view metrics and so on. 
