NFL Passing Analysis
================
Tucker Morgan
9/27/2021

<style type="text/css">
pre {
  max-height: 150px;
  overflow-y: auto;
}

pre[class] {
  max-height: 150px;
}

.scroll-100 {
  max-height: 150px;
  overflow-y: auto;
  background-color: inherit;
}
</style>

``` r
library(tidyverse)
library(RColorBrewer)
library(viridis)
```

## Purpose

Teams in the National Football League (NFL) are racking up more passing
yards than ever, so how can current quarterback (QB) passing stats be
evaluated against historical data? This analysis will aim to answer that
question and provide insight into how passing stats and QB performances
have changed over the past 25 years.

All of the data used in this analysis was obtained from
<https://www.pro-football-reference.com/>.

## Data Import and Cleaning

First, I’ll load in the data from the 1996 NFL season, the oldest data
set I’m including here. I’ll take a look at the provided parameters and
trim down unneeded info to streamline our analysis. I’m starting with
1996 because I want consistent parameters across data sets, and I assume
that more parameters may have been tracked in more recent seasons.

``` r
passing_1996 <- read_csv("nfl_data/passing_1996.csv")
head(passing_1996)
```

    ## # A tibble: 6 × 30
    ##      Rk Player   Tm      Age Pos       G    GS QBrec   Cmp   Att `Cmp%` Yds...12
    ##   <dbl> <chr>    <chr> <dbl> <chr> <dbl> <dbl> <chr> <dbl> <dbl>  <dbl>    <dbl>
    ## 1     1 Mark Br… JAX      26 QB       16    16 9/7/…   353   557   63.4     4367
    ## 2     2 Vinny T… BAL      33 QB       16    16 4/12…   325   549   59.2     4177
    ## 3     3 Drew Bl… NWE      24 QB       16    16 11/5…   373   623   59.9     4086
    ## 4     4 Brett F… GNB      27 QB       16    16 13-3…   325   543   59.9     3899
    ## 5     5 Jeff Bl… CIN      26 QB       16    16 8/8/…   308   549   56.1     3624
    ## 6     6 Gus Fre… WAS      25 QB       16    16 9/7/…   270   470   57.4     3453
    ## # … with 18 more variables: TD <dbl>, TD% <dbl>, Int <dbl>, Int% <dbl>,
    ## #   1D <dbl>, Lng <dbl>, Y/A <dbl>, AY/A <dbl>, Y/C <dbl>, Y/G <dbl>,
    ## #   Rate <dbl>, Sk <dbl>, Yds...25 <dbl>, NY/A <dbl>, ANY/A <dbl>, Sk% <dbl>,
    ## #   4QC <dbl>, GWD <dbl>

``` r
names(passing_1996)
```

    ##  [1] "Rk"       "Player"   "Tm"       "Age"      "Pos"      "G"       
    ##  [7] "GS"       "QBrec"    "Cmp"      "Att"      "Cmp%"     "Yds...12"
    ## [13] "TD"       "TD%"      "Int"      "Int%"     "1D"       "Lng"     
    ## [19] "Y/A"      "AY/A"     "Y/C"      "Y/G"      "Rate"     "Sk"      
    ## [25] "Yds...25" "NY/A"     "ANY/A"    "Sk%"      "4QC"      "GWD"

I think the primary variables I’m interested in are:

-   Player
-   Age
-   G (games)
-   Cmp (completions)
-   Att (attempts)
-   Yds (yards)
-   TD (touchdowns)
-   Int (interceptions)

So I’ll select for those variables and clean up the variable names. I’m
also going to generate a histogram to look at our data distribution and
see if we need to exclude some data.

``` r
passing_1996 <- select(passing_1996, 
                       player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
                       yards = Yds...12, td = TD, int = Int)
ggplot(passing_1996, aes(x = yards)) + geom_histogram()
```

![](nfl_passing_files/figure-gfm/1996%20explore-1.png)<!-- -->

``` r
tail(passing_1996)
```

    ## # A tibble: 6 × 8
    ##   player             age games completions attempts yards    td   int
    ##   <chr>            <dbl> <dbl>       <dbl>    <dbl> <dbl> <dbl> <dbl>
    ## 1 Jerry Rice*+        34    16           0        1     0     0     0
    ## 2 Barry Sanders*      28    16           0        1     0     0     1
    ## 3 Tom Tupa            30    16           0        2     0     0     0
    ## 4 Chris Walsh         28    15           0        1     0     0     0
    ## 5 Sherman Williams    23    16           0        1     0     0     0
    ## 6 Mark Royals         31    16           1        1    -8     0     0

``` r
ggplot(passing_1996, aes(x = games)) + geom_histogram()
```

![](nfl_passing_files/figure-gfm/1996%20explore-2.png)<!-- -->

There appear to be dozens of QBs with near zero yards passing for the
season. These instances won’t really tell us much about the history of
the NFL and its passing trends, so I’d like to exclude some of the
noise. I’ve also looked at a histogram of games played. I’m going to set
8 games as the cutoff for our QBs, since that is equivalent to playing
in half of the games in a regular season (16 games total).

We can also filter for a minimum number of attempts in an effort to
exclude the likes of Jerry Rice and Barry Sanders, players who had only
1 passing attempt but played a full season at other positions. I’ll set
this cut off at 100 attempts.

``` r
passing_1996 <- filter(passing_1996,
                       games > 8, attempts > 100)
ggplot(passing_1996, aes(x = yards)) + geom_histogram()
```

![](nfl_passing_files/figure-gfm/1996%20filter-1.png)<!-- -->

``` r
ggplot(passing_1996, aes(x = games)) + geom_histogram()
```

![](nfl_passing_files/figure-gfm/1996%20filter-2.png)<!-- -->

The histograms above look great, particularly the yards plot, which
somewhat resembles a normal distribution. The last thing I need to do is
add a `year` variable to distinguish this data set from other years when
I bind them together for the larger analyses.

``` r
passing_1996 <- mutate(passing_1996, year = 1996)
passing_1996 <- mutate(passing_1996,
         perf_over_median = yards / median(pull(passing_1996, yards)))
```

I will now perform this data cleaning for the years 1997 - 2020. The
code can be viewed in the scroller below.

``` r
passing_1997 <- read_csv("nfl_data/passing_1997.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 1997) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_1998 <- read_csv("nfl_data/passing_1998.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 1998) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_1999 <- read_csv("nfl_data/passing_1999.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 1999) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2000 <- read_csv("nfl_data/passing_2000.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2000) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2001 <- read_csv("nfl_data/passing_2001.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2001) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2002 <- read_csv("nfl_data/passing_2002.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2002) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2003 <- read_csv("nfl_data/passing_2003.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2003) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2004 <- read_csv("nfl_data/passing_2004.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2004) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2005 <- read_csv("nfl_data/passing_2005.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2005) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2006 <- read_csv("nfl_data/passing_2006.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2006) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2007 <- read_csv("nfl_data/passing_2007.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2007) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2008 <- read_csv("nfl_data/passing_2008.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2008) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2009 <- read_csv("nfl_data/passing_2009.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2009) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2010 <- read_csv("nfl_data/passing_2010.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2010) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2011 <- read_csv("nfl_data/passing_2011.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2011) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2012 <- read_csv("nfl_data/passing_2012.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2012) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2013 <- read_csv("nfl_data/passing_2013.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2013) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2014 <- read_csv("nfl_data/passing_2014.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2014) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2015 <- read_csv("nfl_data/passing_2015.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2015) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2016 <- read_csv("nfl_data/passing_2016.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2016) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2017 <- read_csv("nfl_data/passing_2017.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2017) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2018 <- read_csv("nfl_data/passing_2018.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2018) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2019 <- read_csv("nfl_data/passing_2019.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2019) %>% 
  mutate(perf_over_median = yards / median(yards))

passing_2020 <- read_csv("nfl_data/passing_2020.csv") %>% 
  select(player = Player, age = Age, games = G, completions = Cmp, attempts = Att,
         yards = Yds...12, td = TD, int = Int) %>% 
  filter(games > 8, attempts > 100) %>% 
  mutate(year = 2020) %>% 
  mutate(perf_over_median = yards / median(yards))
```

Now, I’ll use `bind_rows` to stack the data sets into one for analysis.
I’ve also cleaned the QB names because some of them have special
characters indicating MVP or Pro-Bowl seasons. This might be useful in
another analysis, but I’m thinking I won’t need it here in this form.

``` r
nfl_passing <- bind_rows(
  passing_1996, passing_1997, passing_1998, passing_1999, passing_2000,
  passing_2001, passing_2002, passing_2003, passing_2004, passing_2005,
  passing_2006, passing_2007, passing_2008, passing_2009, passing_2010,
  passing_2011, passing_2012, passing_2013, passing_2014, passing_2015,
  passing_2016, passing_2017, passing_2018, passing_2019, passing_2020) %>% 
  mutate(player = str_replace_all(player, "[^[:alnum:]]", ""))
```

## Analysis

First, I’ll take a look at box plots for `yards` for each `year` and add
a trendline to see how things have changed over the past 25 years.

``` r
passing_boxes <- ggplot(
  nfl_passing, aes(group = year, x = year, y = yards, fill = year)) + 
  geom_boxplot(alpha = 0.9) + 
  geom_smooth(se = FALSE, color = "black", group = 1) +
  scale_fill_viridis(option = "D") +
  xlab("Season") +
  ylab("Total Yards") +
  ggtitle("Distributions of Total Passing Yards for NFL QBs (> 8 games, > 100 att.)") +
  theme(legend.position = "none")
passing_boxes
```

![](nfl_passing_files/figure-gfm/box%20plot%20graphing-1.png)<!-- -->

``` r
ggsave(
  "passing_boxes.pdf",
  plot = passing_boxes,
  device = "pdf",
  scale = 1
)

## I'm thinking that from here, I can sort by top performers over median and note who those individuals are. I could also filter for the median quarterback in each season and note those individuals.
```

A couple of things I like about this plot:

-   The box plots are relatively consistent in spread. There are
    fluctuations year to year in terms of interquartile range, but I
    would expect some changes year-to-year because of the nature of the
    NFL and passing yards. Certain quarterbacks could get injured
    halfway through the season or a quarterback could come in for
    garbage time in a few games and throw off the distribution, however
    these types of events are assumed to be equally likely in each
    season.
-   The trendline shows a general increase in individual passing yards
    over time. This aligns with my expectations and the perceptions in
    sports media about the league. Not a big surprise here, but good to
    see that the data backs up intuition / perception. Of further note
    here is that there was more drastic increase in the 2007 - 2010
    range that has tapered off a bit in recent years. We could take a
    look at major events in NFL history (e.g., rule changes) to see if
    we can find an explanatory variable other than time.
-   The original motivation for this analysis was a position from Ryen
    Russillo of the Ryen Russillo Podcast in which he stated that 4,000
    yards isn’t as impressive of a benchmark now as it used to be. In
    the trend analysis, it’s clear that very few quarterbacks reached
    4,000 or more yards in the early 2000’s compared to recent years.
    I’ll look more into this later.

``` r
passing_bins <- ggplot(
  nfl_passing, aes(group = year, x = yards, fill = year)) +
  geom_histogram(binwidth = 1000, boundary = 4000, alpha = 1) +
  labs(x = "Total Passing Yards in a Season",
       y = "Count of QBs",
       title = "Number of QBs (> 8 games, > 100 att.) in Each 1,000 yd Bucket") +
  scale_fill_viridis(name = "Season", option = "D")
passing_bins
```

![](nfl_passing_files/figure-gfm/total%20passing%20yards%20histogram-1.png)<!-- -->

``` r
ggsave(
  "passing_bins.pdf",
  plot = passing_bins,
  device = "pdf",
  scale = 1
)
```

    ## Saving 7 x 5 in image
