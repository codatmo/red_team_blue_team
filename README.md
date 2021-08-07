
# Red Team/Blue Team for COVID-19 Simulation and Modeling

This repo contains cod and information for researchers interestedin either simulating COVID-19 data (Red Team) or modeling that data (Blue Team). The hope is that by creativly exploring possible data generating processes (DGP) and developing corresponding models we will have more robust scientific understanding of the COVID-19 outbreak.

# Red Team Role

The red team develops data generating processes (DGPs) that simulate aspects of disease dynamics at the population level. These models are necessarily oversimplifications of what is actually happening in the world but they can help explore the dynamics of an oubreak. 

Stage 1: The current DGP is a simple SIRTD [LINK]() model that also generates tweets. The model loosely aligns with historical data from Brazil for deaths and tweets and several paramterizations are offered to testing models. Paramterizaions and source code are available as well as a Jupyter notebook that allow for varying paramterizations to be tried at [SIRTD Sim]() with downloadable data. 

Stage 2: Eventually the red team will create different models with different dynamics. The details of how transparent the intitial presentation remains to be decided but we anticipate that components like vaccination, age, location etc.. are all candidates for future simulations. 
