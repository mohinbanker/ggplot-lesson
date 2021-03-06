# A Beginner's Guide to the ggplot2 package
Mohin Banker  
7/24/2017  





# What is ggplot2?

ggplot2 is a statistical graphics package created by Hadley Wickham in 2008. The 'gg' stands for the grammar of graphics, which decomposes graphics into smaller building blocks known as **layers**. The philosophy behind this grammar of graphics was inspired by Leland Wilkinson's book *The Grammar of Graphics*, published in 1999. 

Before the ggplot2 package in R, most programs that used this kind of grammar were very expensive pieces of software. Wickham was frustrated by the limitations of barebones R, and wrote the package after reading Wilkinson's book. Today, ggplot2 is Wickham's most famous package, despite him having created many of R's most used packages, including `dplyr`, `devtools`, `reshape`, and many more. Many mainstream media outlets actively use ggplot in their graphics, notably the New York Times, FiveThirtyEight, and The Economist.

## Why isn't it called ggplot1?

This package is the second iteration of ggplot. The original ggplot package [found here](https://github.com/hadley/ggplot1) was scrapped for this updated package. There are many difference between the packages, but the main difference is the syntax. The original ggplot took a functional programming perspective by nesting layer functions within each other, which can become messy. In ggplot2, layers are 'added' together instead of nested. Here's an example of equivalent code:

ggplot1:

`ggpoint(ggplot(data, list(x = x, y = y)))`

ggplot2:

`ggplot(data, aes(x = x, y = y)) +`
  
  `geom_point()`

# What kind of graphic should I make?

The answer to this question depends on your data. How many variables are you plotting, and what kind of values do they take?

## Univariate graphics

### Continuous variable
For univariate graphics, you are trying to see the distribution of the data, or the density of the variable across value ranges. For a continuous variable, you would use a histogram, where the x-axis are value ranges or "bins", and the y-axis is the frequency or density of that bin. Histograms work for small or large data. *Boxplots* or *violin plots* accomplish the same task and can facet the distribution by different factors. 


```r
ggplot(data = gapminder, aes(x = lifeExp
                             # ,y = ..density.. # Add this line to switch from frequency to density
                             )) +
  geom_histogram() +
  stat_bin(bins = 40) +
  labs(x = "Life Expectancy (Years)", y = "Count of Countries", title = "Histogram of Life Expectancy")
```

![](ggplot_tutorial_files/figure-html/continuous_univariate-1.png)<!-- -->

### Categorical variables
For categorical variables, you want visualize the count or proportion for each category of the variable. In most cases, you would want to use a **bar chart**. While pie charts can be occasionally useful to emphasize percentages of a whole, I would stay away from them because it's difficult estimate and compare two slices of a pie.


```r
ggplot(data = gapminder[, .(num_countries = length(unique(country))), .(continent)], # Transforming dataset within function
       aes(x = reorder(continent, -num_countries), y = num_countries, fill = continent)) +
  geom_bar(stat = "identity") +
  labs(x = "Continent", y = "Number of Observations", title = "Bar Chart of Countries by Continent") +
  guides(fill = F)
```

![](ggplot_tutorial_files/figure-html/categorical_univariate-1.png)<!-- -->

```r
ggplot(data = gapminder[, .(num_countries = length(unique(country))), .(continent)], # Transforming dataset within function
       aes(x = "", y = num_countries, fill = continent)) +
  geom_bar(stat = "identity", width = 0.2) +
  labs(x = "", y = "Number of Observations", title = "Stacked Bar Chart of Countries by Continent", fill = "Continent")
```

![](ggplot_tutorial_files/figure-html/categorical_univariate-2.png)<!-- -->

```r
ggplot(data = gapminder[, .(num_countries = length(unique(country))), .(continent)], # Transforming dataset within function
       aes(x = "", y = num_countries, fill = continent)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart of Countries by Continent", fill = "Continent", y = "", x = "")
```

![](ggplot_tutorial_files/figure-html/categorical_univariate-3.png)<!-- -->


## Multivariate graphics
Multivariate graphics are intended to show you the relationship between two or more variables. These types of graphics can become messy, so it's important that they communicate the data clearly.

### Continuous vs. Continuous
For two continuous variables, the simplest graph is a scatterplot. If you want to include additional continuous or categorical variables in the scatterplot, you can control the size, color, shape, and opacity of the points. You can also use regression lines (which can also be split by a categorical variable) to show the overall shape of the data. 

For extremely large datasets where points can blend together, it can be useful to make points more transparent to show striations in density. Alternatively, instead of a scatterplot, you can use a heatmap or a countour plot. Countour plots can also be used to show a third continuous 'z' variable in a flattened 2-D plot.

```r
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point() +
  scale_size_continuous(breaks = c(2.5*10^8, 5*10^8, 7.5*10^8, 1*10^9, 1.25*10^9), labels = c("250M", "500M", "750M", "1B", "1.25B")) +
  scale_x_log10() +
  labs(x = "Log GDP per Capita", y = "Life Expectancy (Years)", color = "Continent", size = "Population", title = "Scatterplot of Life Expectancy vs. GDP per Capita")
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, size = pop)) +
  geom_density_2d() +
  scale_size_continuous(breaks = c(2.5*10^8, 5*10^8, 7.5*10^8, 1*10^9, 1.25*10^9), labels = c("250M", "500M", "750M", "1B", "1.25B")) +
  scale_x_log10() +
  labs(x = "Log GDP per Capita", y = "Life Expectancy (Years)", color = "Continent", size = "Population", title = "Contour Plot of Life Expectancy vs. GDP per Capita")
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, size = pop, group = continent, color = continent)) +
  geom_smooth(method = "lm") +
  scale_size_continuous(breaks = c(2.5*10^8, 5*10^8, 7.5*10^8, 1*10^9, 1.25*10^9), labels = c("250M", "500M", "750M", "1B", "1.25B")) +
  scale_x_log10() +
  labs(x = "Log GDP per Capita", y = "Life Expectancy (Years)", color = "Continent", size = "Population", title = "Line Graph of Life Expectancy vs. GDP per Capita")
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

## Continuous vs Categorical
These are essentially an extension of univariate categorical graphics. Boxplots are most commonly used, but faceted or stacked histograms are also possible. A simple bar chart would also suffice; bar charts can include another categorical variable by transforming them into either stacked or multiple (known as dodged) bar charts. 


```r
gapminder[, popCategory := cut(pop, breaks = quantile(pop, seq(0, 1, 0.5)),
                               labels = c("Small", "Big"))]

ggplot(data = gapminder, aes(x = continent, y = lifeExp, fill = continent)) +
  geom_violin() +
  labs(y = "Life Expectancy (Years)", x = "Continent", title = "Violin Plot of Life Expectancy") +
  guides(fill = F)
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
ggplot(data = na.omit(gapminder), aes(x = continent, y = lifeExp, fill = popCategory)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(y = "Life Expectancy (Years)", x = "Continent", title = "Multiple Bar Chart of Life Expectancy", fill = "Country Population Size")
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-2-2.png)<!-- -->



## Categorical vs. Categorical
These are more difficult to show, but are best shown through a stacked or multiple (dodged) bar chart. One of the categorical variables must be converted to a percentage for these visualizations. For smaller datasets, using a scatterplot with jittered points is possible. For larger datasets, a heatmap is also possible.


```r
ggplot(data = gapminder[year == 2007], aes(x = continent, y = popCategory, col = continent)) +
  geom_jitter(height = 0.2) +
  geom_text(aes(label = ifelse(continent == "Oceania", as.character(country), "")),
            show.legend = F, nudge_y = 0.15) +
  labs(x = "Continent", y = "Population Size", main = "Jittered Scatterplot of Population Size by Continent") +
  theme(legend.position="none")
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-3-1.png)<!-- -->




# Base R Plotting vs. ggplot

R has a few built-in functions for visualizing data that include:

* `hist()` for histograms
* `plot()` for scatterplots
* `barplot()` for bar charts
* `boxplot()` for box-and-whiskers charts

These are all somewhat customizable by specifying color or size, drawing lines or points, and including legends, titles, and captions. These base plotting functions are rigid in their specifications, don't look aesthetically pleasing, and can become difficult to use for complicated graphics. These aren't issues in ggplot.

When you are only doing exploratory analysis and graphs don't need polish, the base plotting functions are great. Notably, they plot a lot faster than ggplot, and they require less typing. **But, in all other scenarios, ggplot would be the preferred package.** \n\n\n\n

## Comparing plotting times for a simple scatterplot with 10k observations

![](ggplot_tutorial_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```
0.3245468 seconds
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

```
0.5078659 seconds
```

# The Actual Plotting
## Syntax
There are three necessities to create a ggplot graph:

* Data (stored as a dataframe or a data.table)
* Variables
* A geom (specifying scatterplot, barplot, boxplot, etc.)

To create a graph in ggplot, you must start with the `ggplot()` function. In here, you can (but don't have to) specify:

* Data
* Aesthetics (x, y, color, size, group, etc.)
  
This produces a ggplot object, which you can output directly or store as a variable. `myGraph <- ggplot()` is a valid statement. To view the graph, just enter the name of the object (e.g. `myGraph`).

To a ggplot object, you can add layers on top. You can add layers with many functions in the form of:

* `geom_***()`
* `scale_x_***()`
* `scale_y_***()`
* `stat_***()`
* `coord_***()`
* And more...
    
If data and aesthetics weren't specified in the `ggplot()` function, then they must be specified in each `geom_***` function.

## Core Layers

* **Mappings.** 
    + *Which variables are being compared?*
    + This is the `aes()` function.
* **Geometric objects.** 
    + *How is the comparison shown?*
    + These are the `geom_point()` or `geom_bar()` functions for scatter or barplots, for example.

### Additional Layers

* **Statistical transformations.**
    + *How are the data being transformed?*
    + Useful for making counts or bins with `..count..` or `stat_bin()`, for example.
* **Scales** and **Coordinate Systems**
    + *How will the comparison be stylized?*
    + Changing and labeling axes, legends, and gridlines. This is the `scale_x_continuous()`, `scale_x_discrete()`, or  `coord_polar()` functions.
* **Faceting**
    + *How will the data be partitioned?*
    + Displays the same graph layout for all subsets. This is essentially only `facet_grid()` or `facet_wrap()`.

If you want a full list of all the layers and functions included in the ggplot2 package, check out the [reference guide](http://ggplot2.tidyverse.org/reference/).

Since I didn't include any plots demonstrating faceting, here's an example:

```r
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp, color = year, size = pop)) +
  geom_point() +
  facet_grid(. ~ continent) +
  scale_size_continuous(breaks = c(2.5*10^8, 5*10^8, 7.5*10^8, 1*10^9, 1.25*10^9), labels = c("250M", "500M", "750M", "1B", "1.25B")) +
  scale_x_log10(breaks = round(median(gapminder$gdpPercap)), labels = comma) +
  labs(x = "Log GDP per Capita", y = "Life Expectancy (Years)", color = "Year", size = "Population", title = "Scatterplot of Life Expectancy vs. GDP per Capita")
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


## Building a plot layer by layer
This plot and data was taken with permission from Peter Fontana.
We first draw an empty rectangle to show an empty plot.


```r
baseball <- fread("baseball_salaries.csv")
hist_plot_1 <- ggplot()
hist_plot_1 + theme(panel.border     = element_rect(fill = NA, colour = "grey20"))
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Then, we add a simple histogram of basaeball player salaries (from the 1986-1987 season).


```r
hist_plot_1 +  geom_histogram(data=baseball,aes(x=Salary, y=..count..),binwidth=250,closed="left")
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Then, we add color to the bars of the histogram to emphasize the count in each bar.

```r
hist_plot_2 <- hist_plot_1 + geom_histogram(data=baseball,aes(x=Salary, y=..count..,fill=..count..),binwidth=250,color="black",closed="left")
hist_plot_2
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Now, we overlay another smaller histogram to show a more granular distribution. It's important that we plotted this after the first histogram; otherwise, it would have been covered.

```r
hist_plot_3 <- hist_plot_2 + geom_histogram(data=baseball,aes(x=Salary,y=..count..), binwidth=25, color="black", fill=rgb(255,255,255,max=255), closed="left")
hist_plot_3
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Then, we add text above each bar to display the count in each bar of the histogram using `geom_text`.


```r
hist_plot_4 <- hist_plot_3 + geom_text(data=baseball, aes(x=Salary, label=..count.., vjust=-1),stat="bin",binwidth=250,size=4,closed="left")
hist_plot_4
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Next, we change the color palette of the larger histogram through `scale_fill_gradient`.


```r
hist_plot_5 <- hist_plot_4 +  scale_fill_gradient(low=rgb(127,0,255,max=255),high=rgb(0,0,255,max=255),name="Number of \nPlayers")
hist_plot_5
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

The axis labels are a bit too spaced out and far away. We modify the axes labels so that 0's appear closer to the origin with `scale_x_continuous` and `scale_y_continuous`.

```r
hist_plot_6 <- hist_plot_5 + scale_x_continuous(expand= c(0,0)) + scale_y_continuous(expand=c(0,0))
hist_plot_6
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

The text for our largest bar is outside of the window, so we have to change the limits of the y-axis through `coord_cartesian`.

```r
hist_plot_7 <- hist_plot_6 +  coord_cartesian(ylim=c(0,120))
hist_plot_7
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

Finally, we add labels and title. This is our final graph!

```r
hist_plot_f <- hist_plot_7 + labs(title = "Histogram of Players' Salaries", x = "Salary ($K)", y = "Number of Players") 
hist_plot_f
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-14-1.png)<!-- -->



# Themes
Themes are ways of specifying fonts, styles, and color schemes for your graphs. They dramatically change the appearance of your graphs, but can be time-consuming to create by yourself. Fortunately, other than ggplot's default theme, `theme_gray()`, it also comes with another theme called `theme_bw()`. 

There are additional themes found in the `ggthemes` packages that range from a solarized color scheme to styles imitating the Economist. Here are a few examples:


```r
run <- fread("run.csv")

g <- ggplot(data = run, aes(x = time_index/60, y = speed_mph, group = run, col = as.factor(run))) +
  geom_smooth() +
  labs(x = "Time Running (min)", y = "Speed (mph)", 
       col = "Run Number", title = "Arjun's Speed When Running")
  
g + labs(subtitle = "Gray Theme")
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
g + theme_bw() + labs(subtitle = "B&W Theme")
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-15-2.png)<!-- -->

```r
g + theme_economist() + labs(subtitle = "Economist Theme") + scale_color_economist()
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-15-3.png)<!-- -->

```r
g + theme_solarized() + labs(subtitle = "Solarized Theme") + scale_color_solarized()
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-15-4.png)<!-- -->

```r
g + theme_solarized(light = F) + labs(subtitle = "Dark Solarized Theme") + scale_color_solarized()
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-15-5.png)<!-- -->

```r
g + theme_fivethirtyeight() + labs(subtitle = "FiveThirtyEight Theme") + scale_color_fivethirtyeight()
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-15-6.png)<!-- -->

```r
g + theme_stata() + labs(subtitle = "Stata Theme") + scale_color_stata()
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-15-7.png)<!-- -->

Here's an example of how to create your own theme from scratch:


```r
# theme code
theme_bw_pf <- function(base_size = 11, base_family = "sans", title_family="serif") {
  # Starts with theme_grey and then modify some parts
  half_line <- base_size/2
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text = element_text(size=rel(0.8)),
      axis.title.x = element_text(lineheight=1.2, margin = margin(1.5*half_line,0,0,0.8*half_line/2.0)),
      axis.title.y = element_text(angle = 90, lineheight=1.2, margin = margin(0, 1.5*half_line, 0, 0.8*half_line/2.0)),
      # axis.ticks = element_line(size = rel(0.8)),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      legend.title = element_text(size = rel(1.0), hjust = 0, lineheight = 1.3,face="bold", family=title_family),
      legend.key       = element_blank(),
      legend.background = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border     = element_blank(),
      #panel.grid.major = element_line(colour = "grey92"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(2*half_line,"pt"),
      strip.background = element_rect(fill = "grey80", colour = "grey80", size=0.2),
      plot.title = element_text(size= rel(1.2), family=title_family, face="bold", lineheight=1.2, margin= margin(0,0,half_line*2.0,0), hjust = 0)
    )
}
```


# Appendix

## qplot
The `qplot()` function (also called through `quickplot()`) is intended to make ggplot objects easier and faster to create. The syntax for these functions is also very similar to the syntax of base R plots, which can make it easier for people used to base R to dabble in ggplot. 

Unfortunately, since all arguments are contained within one function, it is harder to create complex graphics with `qplot()`. It is better in the long run to learn how to use the `ggplot()` function.

## Maps and Animations in an Example

### Maps

```r
# Scatterplots
ggplot(data = run, aes(x = lon, y = lat)) +
  geom_point()
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
# Specify color of the point
ggplot(data = run, aes(x = lon, y = lat, col = run)) +
  geom_point()
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-17-2.png)<!-- -->

```r
# Switch from a continuous scale to a discrete scale
ggplot(data = run, aes(x = lon, y = lat, col = as.factor(run))) +
  geom_point()
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-17-3.png)<!-- -->

```r
# Only graph a subset of the data
ggplot(data = run[run != 4], aes(x = lon, y = lat, col = as.factor(run))) +
  geom_point()
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-17-4.png)<!-- -->

```r
# Change to a connected line graph, and add in size to describe elevation
ggplot(data = run[run != 4], aes(x = lon, y = lat, col = as.factor(run), size = ele, group = run)) +
  geom_path()
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-17-5.png)<!-- -->

```r
# Overlay a map
run <- run[run != 4]

map <- get_map(c(-122.44, 37.74, -122.39, 37.785), source = "stamen", color = "bw", maptype = "toner-lite")

ggmap(map) +
  geom_path(data = run, aes(x = lon, y = lat, col = as.factor(run), size = ele, group = run)) +
  guides(col = F, size = F) +
  labs(x = "Longitude", y = "Latitude", main = "Arjun's Runs over 7 Days")
```

![](ggplot_tutorial_files/figure-html/unnamed-chunk-17-6.png)<!-- -->

### Animation
The `gganimate` package adds in a new aesthetic, called "frame". This aesthetics specifies a variable that the plot will animate over, usually some kind of time variable. Then, by calling the `gganimate()` function, an animated file is produced. Unfortunately, I don't have much experience with this package, and had trouble getting it to work.

```r
p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent, frame = year)) +
  geom_point() +
  scale_x_log10()

#gganimate(p)
```
