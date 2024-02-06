## Data Visualisation Using `ggplot2`

During this course we provide a comprehensive introduction to data visualization in R using ggplot. We begin by providing a brief overview of the general principles data visualization, and an overview of the general principles behind ggplot. We then proceed to cover the major types of plots for visualizing distributions of univariate data: histograms, density plots, barplots, and Tukey boxplots. In all of these cases, we will consider how to visualize multiple distributions simultaneously on the same plot using different colours and "facet" plots. We then turn to the visualization of bivariate data using scatterplots. Here, we will explore how to apply linear and nonlinear smoothing functions to the data, how to add marginal histograms to the scatterplot, add labels to points, and scale each point by the value of a third variable. We then cover some additional plot types that are often related but not identical to those major types covered during the beginning of the course: frequency polygons, area plots, line plots, uncertainty plots, violin plots, and geospatial mapping. We then consider more fine grained control of the plot by changing axis scales, axis labels, axis tick points, colour palettes, and ggplot "themes". Finally, we consider how to make plots for presentations and publications. Here, we will introduce how to insert plots into documents using RMarkdown, and also how to create labelled grids of subplots of the kind seen in many published articles.

## Topics Covered

**Topic 1:** What is data visualization. Data visualization is a means to explore and understand our data and should be a major part of any data analysis. Here, we briefly discuss why data visualization is so important and what the major principles behind it are.

**Topic 2:** Introducing `ggplot`. Though there are many options for visualization in R, ggplot is simply the best. Here, we briefly introduce the major principles behind how ggplot works, namely how it is a layered grammar of graphics.

**Topic 3:** Visualizing univariate data. Here, we cover a set of major tools for visualizing distributions over single variables: histograms, density plots, barplots, Tukey boxplots. In each case, we will explore how to plot multiple groups of data simultaneously using different colours and also using facet plots.

**Topic 4:** Scatterplots. Scatterplots and their variants are used to visualize bivariate data. Here, in addition to covering how to visualize multiple groups using colours and facets, we will also cover how to provide marginal plots on the scatterplots, labels to points, and how to obtain linear and nonlinear smoothing of the plots.

**Topic 5:** More plot types. Having already covered the most widely used general purpose plots, we now turn to cover a range of other major plot types: frequency polygons, area plots, line plots, uncertainty plots, violin plots, and geospatial mapping. Each of these are important and widely used types of plots, and knowing them will expand your repertoire.

**Topic 6:** Fine control of plots. Thus far, we will have mostly used the default for the plot styles and layouts. Here, we will introduce how to modify things like the limits and scales on the axes, the positions and nature of the axis ticks, the colour palettes that are used, and the different types of ggplot themes that are available.

**Topic 7:** Plots for publications and presentations. Thus far, we have primarily focused on data visualization as a means of interactively exploring data. Often, however, we also want to present our plots in, for example, published articles or in slide presentations. It is simple to save a plot in different file formats, and then insert them into a document. However, a much more efficient way of doing this is to use RMarkdown to run the R code and automatically insert the resulting figure into a, for example, Word document, pdf document, html page, etc. In addition, here we will also cover how to make labelled grids of subplots like those found in many scientific articles.