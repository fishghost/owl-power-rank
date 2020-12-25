# Community Power Rank Tool for Overwatch League
View the published app on [shinyapps](https://fishghost.shinyapps.io/OWLPR/).

This is an R Shiny app to allow the community to "power rank" teams from the Overwatch League in a way that can tracked over time. Users can submit their power rankings for aggregration (done separately) and retrieve submissions they have made previously. 

Submissions are saved on a secret Google sheet. To run the code in R, you will need to create and provide your own google sheet and authentication token. 

All code is contained within app.R. It is roughly separated into 3 sections: 
- **Google Sheet interatcion**: Pull initial information from Google sheets and set up token connection
- **Shiny UI code**: code to create fluid page 
- **Shiny server code**: code to dynamically create visualizations and react to interactivity 

More information about the visualizations and data can be found in the [app](https://fishghost.shinyapps.io/OWLPR/) by hitting `About` under `Options and Info`. 