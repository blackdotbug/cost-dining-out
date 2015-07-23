# cost-dining-out
The Show - final project for Summer 2015 Data Visualization course in Northwestern's MSPA program. Team focus: Food. Team topic: The Cost of Dining Out.

# the r-source folder
This is a blank project folder for R studio. Locally you can add CSV files to this folder which you can process in R and produce visualizations which you will save in the slides folder. 

# the slides folder
This is where the image files for the show will exist. Any image file format will work. Use file naming conventions for the web (no spaces in file names, no special characters). 

# Intro.html
This is where you add slides to the show. The first slide container begins on line 74. Each section must have a unique id. 

# the section tag
< section id="slide1" class="image" data-autoslide="25000" >
The first slide id is slide1. We can increment the slide id numbers with each slide, so the second slide id should be slide2, the third slide id should be slide3, etc.
The class attribute links this section to the style definition titled "image" in the theme's css. This should not be changed. 
The data-autoslide attribute determines in milliseconds how long the slide should be displayed if autoplay is turned on. 

# the img tag
< img width="1060" height="530" src="slides/slide1.png"  alt="slide1" >
Each slide section contains an image tag. 
Set the width and height attributes to match the dimensions of your slide image. 
Set the source (src) attribute to the location and filename of the image for this slide.
Set the alt attribute with appropriate alternative text for screen reader accessibility. 