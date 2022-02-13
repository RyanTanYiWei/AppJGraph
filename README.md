# J-Graph App (Topological Relationships of Spaces)

[![Video Demo](http://img.youtube.com/vi/Sts_uZadjAs/0.jpg)](http://www.youtube.com/watch?v=Sts_uZadjAs "JGraph App Demo (RShiny)")

[App Demo](https://rtyw.shinyapps.io/jgraph/)

<b>(Demo App) Visualizing Topological Relationship of Complex Spaces</b>

My colleagues are very aware of my obsession with drawing networks of rooms and hallways to study spatial planning in complex buildings typologies (Convex Map/J-graph analysis). Hence, I was inspired to build a simple R shiny app that automates this process for me so that I can focus more on the analysis process. The input data demonstrated in the video is from my previous study on the game spaces in "Among Us".

There are certainly other well-known network visualization softwares like Cytoscape and Gephi. However, I wanted an app that streamlines the basic graph computations with useful layout and visualization features typically used in such network analyses:

1) Generates a Nodelist with Centrality Measures
2) Multiple Layouts - Tree, Radial Tree, KK, FR
3) Reassignment of Root Node (for Tree/Jgraph Layout)
4) Visualizing Centrality Measures through Node Sizes
5) Adjustable Node and Label Size
6) Node Colouring function (multiple entries) - identified based on names (RegEx) of the nodes
7) Edge Colouring function - identified based on edge list column names

I would love to hear any suggestions for future improvements.

