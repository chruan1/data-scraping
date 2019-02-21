Homework 4
================
Chunjin Ruan
Due by 2:20 pm, Fri. 2/15

Push your knitted homework assignment (.Rmd and .md files) to GitHub by the given deadline.

Also let me know:

**Who you worked with:**

### Problem 1

The vector called `words` in `stringr` contains a corpus of 980 words used in text analysis. Use regular expressions with `stringr` to find the words that satisfy the following descriptions:

-   begin with `b`
-   contain `q`, `x`, or `z`
-   contain `th` or `ch`
-   end with `g` but not `ng`
-   are 10 letters long
-   have 3 or more vowels in a row
-   start and end with the same letter

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.6
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ───────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(readr)
library(stringr)
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(ggrepel)
```

#### begin with `b`

``` r
words %>%
  subset(str_detect(words, "^b"))
```

    ##  [1] "baby"      "back"      "bad"       "bag"       "balance"  
    ##  [6] "ball"      "bank"      "bar"       "base"      "basis"    
    ## [11] "be"        "bear"      "beat"      "beauty"    "because"  
    ## [16] "become"    "bed"       "before"    "begin"     "behind"   
    ## [21] "believe"   "benefit"   "best"      "bet"       "between"  
    ## [26] "big"       "bill"      "birth"     "bit"       "black"    
    ## [31] "bloke"     "blood"     "blow"      "blue"      "board"    
    ## [36] "boat"      "body"      "book"      "both"      "bother"   
    ## [41] "bottle"    "bottom"    "box"       "boy"       "break"    
    ## [46] "brief"     "brilliant" "bring"     "britain"   "brother"  
    ## [51] "budget"    "build"     "bus"       "business"  "busy"     
    ## [56] "but"       "buy"       "by"

#### contain `q`, `x`, `z`

``` r
words %>%
  subset(str_detect(words, "q|x|z"))
```

    ##  [1] "box"        "equal"      "exact"      "example"    "except"    
    ##  [6] "excuse"     "exercise"   "exist"      "expect"     "expense"   
    ## [11] "experience" "explain"    "express"    "extra"      "next"      
    ## [16] "organize"   "quality"    "quarter"    "question"   "quick"     
    ## [21] "quid"       "quiet"      "quite"      "recognize"  "require"   
    ## [26] "sex"        "six"        "size"       "square"     "tax"

#### contain `th` or `ch`

``` r
words %>%
  subset(str_detect(words, "th|ch"))
```

    ##  [1] "achieve"   "although"  "another"   "approach"  "authority"
    ##  [6] "birth"     "both"      "bother"    "brother"   "catch"    
    ## [11] "chair"     "chairman"  "chance"    "change"    "chap"     
    ## [16] "character" "charge"    "cheap"     "check"     "child"    
    ## [21] "choice"    "choose"    "church"    "clothe"    "each"     
    ## [26] "either"    "father"    "further"   "health"    "kitchen"  
    ## [31] "lunch"     "machine"   "match"     "month"     "mother"   
    ## [36] "much"      "north"     "other"     "otherwise" "rather"   
    ## [41] "research"  "scheme"    "school"    "south"     "such"     
    ## [46] "switch"    "teach"     "than"      "thank"     "the"      
    ## [51] "then"      "there"     "therefore" "they"      "thing"    
    ## [56] "think"     "thirteen"  "thirty"    "this"      "thou"     
    ## [61] "though"    "thousand"  "three"     "through"   "throw"    
    ## [66] "thursday"  "together"  "touch"     "watch"     "whether"  
    ## [71] "which"     "with"      "within"    "without"   "worth"

#### end with `g` but not `ng`

``` r
words %>%
  subset(str_detect(words, "[^n]g$"))
```

    ## [1] "bag" "big" "dog" "egg" "leg"

#### are 10 letters long

``` r
words %>%
  subset(str_detect(words, "^.{10}$"))
```

    ## [1] "department" "difference" "experience" "individual" "particular"
    ## [6] "photograph" "television" "understand" "university"

#### have 3 or more vowels in a row

``` r
words %>%
  subset(str_detect(words,"[aeiouy]{3,}"))
```

    ##  [1] "beauty"   "eye"      "obvious"  "previous" "quiet"    "serious" 
    ##  [7] "various"  "year"     "you"      "young"

#### start and end with the same letter

``` r
words %>%
  subset(str_detect(words, "^(.).*\\1$"))
```

    ##  [1] "america"    "area"       "dad"        "dead"       "depend"    
    ##  [6] "educate"    "else"       "encourage"  "engine"     "europe"    
    ## [11] "evidence"   "example"    "excuse"     "exercise"   "expense"   
    ## [16] "experience" "eye"        "health"     "high"       "knock"     
    ## [21] "level"      "local"      "nation"     "non"        "rather"    
    ## [26] "refer"      "remember"   "serious"    "stairs"     "test"      
    ## [31] "tonight"    "transport"  "treat"      "trust"      "window"    
    ## [36] "yesterday"

### Problem 2

Revisit the `words` vector. What word, or words, in this vector has the highest number of vowels? What word has the highest proportion of vowels?

``` r
library(stringr)
words <- as.tibble(words)
words %>%
  mutate(vowels = str_count(value, "[aeiouy]")) %>%
  filter(vowels == max(vowels)) 
```

    ## # A tibble: 12 x 2
    ##    value       vowels
    ##    <chr>        <int>
    ##  1 appropriate      5
    ##  2 associate        5
    ##  3 authority        5
    ##  4 available        5
    ##  5 colleague        5
    ##  6 encourage        5
    ##  7 experience       5
    ##  8 individual       5
    ##  9 opportunity      5
    ## 10 television       5
    ## 11 university       5
    ## 12 yesterday        5

**Answer** appropriate, associate, authority, available, colleague, encourage, experience, individual, opportunity, television are the highest number of vowels.

``` r
words %>%
  mutate(vowels = str_count(value, "[aeiouy]")) %>%
  mutate(prop_vowels = vowels /str_length(value)) %>%
  arrange(desc(prop_vowels))
```

    ## # A tibble: 980 x 3
    ##    value vowels prop_vowels
    ##    <chr>  <int>       <dbl>
    ##  1 a          1       1    
    ##  2 eye        3       1    
    ##  3 you        3       1    
    ##  4 area       3       0.75 
    ##  5 away       3       0.75 
    ##  6 easy       3       0.75 
    ##  7 idea       3       0.75 
    ##  8 okay       3       0.75 
    ##  9 year       3       0.75 
    ## 10 age        2       0.667
    ## # ... with 970 more rows

**Answer** a, eye, you are the highest number of vowels. \#\#\# Problem 3

(Combining exercise 15.6 and 15.7)

Project Gutenberg contains the full text of *The Complete Works of William Shakespeare*. (<http://www.gutenberg.org/files/100/100-0.txt>)

**(a)** Use the `read_lines()` in **readr** to import the text data.

``` r
shakespeare <-read_lines("http://www.gutenberg.org/files/100/100-0.txt")
glimpse(shakespeare)
```

    ##  chr [1:153386] "" ...

**(b)** Use regular expressions to determine the number of speaking lines in *The Complete Works of William Shakespeare*. Speaking lines in Shakespeare’s plays are identified by a line that starts with two spaces, then a string of capital letters and spaces (the character’s name) followed by a period. Here, we care only about how many times a character speaks—not what they say or for how long they speak.

``` r
lines <-shakespeare %>%
  subset(str_detect(shakespeare, "^\\s\\s[A-Z]+[A-Z\\s]+[.]"))
length(lines)
```

    ## [1] 19965

**Answer** There are 19965 speaking lines in Shakespeare's plays are identified.

**(c)** Make a bar chart displaying the top 100 characters with the greatest number of lines. *Hint:* you may want to use either the `str extract()`. \#\#\#\#Extract names and find the occurrance of the names

``` r
shakespeare <-as_tibble(shakespeare)
shakespeare %>%
  filter(str_detect(value, "^\\s\\s[A-Z]+[A-Z ]+[.]")) %>%
  separate(value, into = c("name", "line"), sep = "[.]") %>%
  group_by(name) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:100) %>%
  ggplot(aes(x = fct_reorder(name, -count), y = count)) +
  geom_col() +
  labs(title = "The top 100 characters with the greatest number of lines", x = "character", y = "number of lines")
```

    ## Warning: Expected 2 pieces. Additional pieces discarded in 8093 rows [2, 3,
    ## 4, 5, 6, 7, 8, 13, 14, 24, 26, 27, 28, 29, 30, 31, 33, 35, 36, 37, ...].

![](hw4_files/figure-markdown_github/unnamed-chunk-13-1.png)

### Problem 4

Scrape the table of data found at <https://en.wikipedia.org/wiki/List_of_United_States_cities_by_crime_rate> and create a plot showing violent crime rate (total violent crime) vs. property crime rate (total property crime). Identify outlier cities by using a plotting command such as:

``` r
#Load data
tables <-read_html("https://en.wikipedia.org/wiki/List_of_United_States_cities_by_crime_rate") %>%
  html_nodes("table") 
#Check the tables
crimes_table1 <- html_table(tables[[1]])
crimes_table2 <- html_table(tables[[2]], fill = TRUE)
crimes_table3 <- html_table(tables[[3]])

crime <-crimes_table2[c(1,2,4,9)]
crime
```

    ##                   State                   City Violent Crime
    ## 1                 State                   City         Total
    ## 2            New Mexico            Albuquerque         965.8
    ## 3            California                Anaheim         363.7
    ## 4                Alaska              Anchorage        1070.9
    ## 5                 Texas              Arlington         502.1
    ## 6               Georgia                Atlanta        1119.6
    ## 7              Colorado                 Aurora         460.8
    ## 8                 Texas                 Austin         372.5
    ## 9            California            Bakersfield         484.1
    ## 10             Maryland              Baltimore        1535.9
    ## 11        Massachusetts                 Boston         706.8
    ## 12             New York                Buffalo        1118.6
    ## 13              Arizona               Chandler         189.3
    ## 14       North Carolina  Charlotte-Mecklenburg         677.6
    ## 15             Illinois                Chicago         903.8
    ## 16           California            Chula Vista         265.8
    ## 17                 Ohio             Cincinnati         925.0
    ## 18                 Ohio             Cleveland*        1334.3
    ## 19             Colorado       Colorado Springs         438.3
    ## 20                 Ohio               Columbus         546.3
    ## 21                Texas         Corpus Christi         645.0
    ## 22                Texas                 Dallas         694.2
    ## 23             Colorado                 Denver         673.9
    ## 24             Michigan                Detroit        1759.6
    ## 25       North Carolina                 Durham         847.2
    ## 26                Texas                El Paso         366.6
    ## 27              Indiana             Fort Wayne         378.9
    ## 28                Texas             Fort Worth         525.4
    ## 29           California                 Fresno         551.2
    ## 30       North Carolina             Greensboro         597.0
    ## 31               Nevada              Henderson         168.5
    ## 32               Hawaii               Honolulu         243.9
    ## 33                Texas                Houston         966.7
    ## 34              Indiana           Indianapolis        1288.0
    ## 35           California                 Irvine          55.8
    ## 36              Florida           Jacksonville         648.3
    ## 37           New Jersey            Jersey City         521.6
    ## 38             Missouri            Kansas City        1417.3
    ## 39                Texas                 Laredo         379.3
    ## 40               Nevada              Las Vegas         920.7
    ## 41             Kentucky              Lexington         332.4
    ## 42             Nebraska                Lincoln         370.6
    ## 43           California             Long Beach         580.7
    ## 44           California            Los Angeles         634.8
    ## 45             Kentucky       Louisville Metro         631.8
    ## 46            Tennessee                Memphis        1740.1
    ## 47              Arizona                   Mesa         418.7
    ## 48              Florida                  Miami        1021.3
    ## 49            Wisconsin              Milwaukee        1596.1
    ## 50            Minnesota            Minneapolis        1062.9
    ## 51              Alabama                Mobile2         610.8
    ## 52            Tennessee Nashville Metropolitan        1101.0
    ## 53            Louisiana            New Orleans         949.6
    ## 54             New York               New York         585.8
    ## 55           New Jersey                Newark*        1077.7
    ## 56           California                Oakland        1442.5
    ## 57             Oklahoma          Oklahoma City         765.6
    ## 58             Nebraska                  Omaha         515.0
    ## 59              Florida                Orlando         940.6
    ## 60         Pennsylvania           Philadelphia        1029.0
    ## 61              Arizona                Phoenix         593.8
    ## 62         Pennsylvania             Pittsburgh         706.2
    ## 63                Texas                  Plano         153.0
    ## 64               Oregon              Portland*         472.8
    ## 65       North Carolina              Raleigh**         392.3
    ## 66           California              Riverside         446.0
    ## 67           California             Sacramento         737.4
    ## 68                Texas            San Antonio         587.2
    ## 69           California              San Diego         398.6
    ## 70           California          San Francisco         776.8
    ## 71           California               San Jose         329.6
    ## 72           California              Santa Ana         482.1
    ## 73           Washington                Seattle         598.7
    ## 74             Missouri              St. Louis        1817.1
    ## 75            Minnesota               St. Paul         703.3
    ## 76              Florida         St. Petersburg         741.9
    ## 77           California               Stockton        1352.0
    ## 78              Florida                  Tampa         630.7
    ## 79                 Ohio                 Toledo        1128.9
    ## 80              Arizona                 Tucson         655.5
    ## 81             Oklahoma                  Tulsa         903.6
    ## 82             Virginia         Virginia Beach         138.3
    ## 83 District Of Columbia             Washington        1202.6
    ## 84               Kansas                Wichita         984.8
    ##        Property Crime
    ## 1  Aggravated Assault
    ## 2              6073.2
    ## 3              2872.3
    ## 4              3917.5
    ## 5              3443.6
    ## 6              5499.3
    ## 7              2936.7
    ## 8              3771.0
    ## 9              4161.4
    ## 10             4980.4
    ## 11             2316.1
    ## 12             4330.2
    ## 13             2083.2
    ## 14             3767.9
    ## 15             2946.3
    ## 16             1741.6
    ## 17             5510.0
    ## 18             5434.4
    ## 19             3648.0
    ## 20             3934.3
    ## 21             3465.6
    ## 22             3440.2
    ## 23             3529.9
    ## 24             4093.6
    ## 25             4115.8
    ## 26             1914.2
    ## 27             3058.4
    ## 28             3585.7
    ## 29             4148.3
    ## 30             3568.5
    ## 31             1893.1
    ## 32             3110.7
    ## 33             4397.5
    ## 34             4790.8
    ## 35             1498.1
    ## 36             3673.0
    ## 37             1594.9
    ## 38             4441.3
    ## 39             3370.9
    ## 40             2995.3
    ## 41             3949.7
    ## 42             3265.9
    ## 43             3010.0
    ## 44             2359.6
    ## 45             4166.0
    ## 46             5630.8
    ## 47             2527.4
    ## 48             4367.4
    ## 49             4264.2
    ## 50             4193.9
    ## 51             4311.6
    ## 52             3805.8
    ## 53             3874.2
    ## 54             1518.7
    ## 55             2851.2
    ## 56             5856.8
    ## 57             3956.1
    ## 58             3595.6
    ## 59             6015.5
    ## 60             3147.4
    ## 61             3491.3
    ## 62             3224.5
    ## 63             1799.1
    ## 64             5234.8
    ## 65             3063.0
    ## 66             3259.7
    ## 67             3369.5
    ## 68             5029.5
    ## 69             2082.0
    ## 70             6138.0
    ## 71             2427.1
    ## 72             2155.3
    ## 73             5522.0
    ## 74             6316.1
    ## 75             3282.1
    ## 76             5622.7
    ## 77             4263.2
    ## 78             2295.9
    ## 79             4475.0
    ## 80             6642.8
    ## 81             5203.2
    ## 82             2205.6
    ## 83             4516.2
    ## 84             5041.2

``` r
crime <- crime %>%
  as.tibble() %>%
  slice(-1) %>% 
  rename(vio_crime = "Violent Crime",
         prop_crime = "Property Crime")
crime$vio_crime <- as.numeric(crime$vio_crime)
crime$prop_crime <- as.numeric(crime$prop_crime)
```

``` r
ggplot(crime, aes(x = vio_crime, y = prop_crime, label = City)) +
    geom_point() +
    geom_label_repel(data=subset(crime, vio_crime > 1500 | prop_crime > 6500), 
              check_overlap = TRUE, size = 2.5, nudge_y = 200) + 
    labs(title = "Violent crime rate vs. property crime rate", x = "violent crime rate", y = "property crime rate")
```

    ## Warning: Ignoring unknown parameters: check_overlap

![](hw4_files/figure-markdown_github/unnamed-chunk-15-1.png)

Hints:

-   after reading in the table using `html_table()`, create a data frame with just the columns you want using column numbers. Otherwise, R gets confused since it appears as if several columns all have the same column name.
-   then, turn `crimes` into a tibble with `as.tibble(crimes3)` and do necessary tidying: get rid of unneeded rows, parse columns into proper format, etc.

Alternatives to `geom_text()`:

If you want to try something new, check out the **ggrepel** package to label the outliers.

### Problem 5

Scrape the data from IMDB's top grossing films released in 2018 at <https://www.imdb.com/search/title?year=2018&title_type=feature&sort=boxoffice_gross_us,desc>. Create a tibble that contains the title, gross, imdbscore, and metascore for the top 50 films. Then generate a scatterplot of one of the ratings vs. gross, labeling outliers as in Problem 4 with the title of the movie.

``` r
website <- read_html("https://www.imdb.com/search/title?year=2018&title_type=feature&sort=boxoffice_gross_us,desc") 
movie_title <- website %>%
  html_nodes(".lister-item-header") %>%
  html_text() %>%
  parse_character()
movie_title
```

    ##  [1] "\n        1.\n    \n    Black Panther\n    (2018)\n"                               
    ##  [2] "\n        2.\n    \n    Avengers: Infinity War\n    (2018)\n"                      
    ##  [3] "\n        3.\n    \n    Incredibles 2\n    (2018)\n"                               
    ##  [4] "\n        4.\n    \n    Jurassic World: Fallen Kingdom\n    (2018)\n"              
    ##  [5] "\n        5.\n    \n    Aquaman\n    (2018)\n"                                     
    ##  [6] "\n        6.\n    \n    Deadpool 2\n    (2018)\n"                                  
    ##  [7] "\n        7.\n    \n    The Grinch\n    (2018)\n"                                  
    ##  [8] "\n        8.\n    \n    Mission: Impossible - Fallout\n    (2018)\n"               
    ##  [9] "\n        9.\n    \n    Ant-Man and the Wasp\n    (2018)\n"                        
    ## [10] "\n        10.\n    \n    Solo: A Star Wars Story\n    (2018)\n"                    
    ## [11] "\n        11.\n    \n    Venom\n    (2018)\n"                                      
    ## [12] "\n        12.\n    \n    Bohemian Rhapsody\n    (2018)\n"                          
    ## [13] "\n        13.\n    \n    A Star Is Born\n    (2018)\n"                             
    ## [14] "\n        14.\n    \n    Ralph Breaks the Internet\n    (2018)\n"                  
    ## [15] "\n        15.\n    \n    A Quiet Place\n    (2018)\n"                              
    ## [16] "\n        16.\n    \n    Spider-Man: Into the Spider-Verse\n    (2018)\n"          
    ## [17] "\n        17.\n    \n    Crazy Rich Asians\n    (2018)\n"                          
    ## [18] "\n        18.\n    \n    Mary Poppins Returns\n    (2018)\n"                       
    ## [19] "\n        19.\n    \n    Hotel Transylvania 3: Summer Vacation\n    (2018)\n"      
    ## [20] "\n        20.\n    \n    Fantastic Beasts: The Crimes of Grindelwald\n    (2018)\n"
    ## [21] "\n        21.\n    \n    Halloween\n    (I) (2018)\n"                              
    ## [22] "\n        22.\n    \n    The Meg\n    (2018)\n"                                    
    ## [23] "\n        23.\n    \n    Ocean's 8\n    (2018)\n"                                  
    ## [24] "\n        24.\n    \n    Ready Player One\n    (2018)\n"                           
    ## [25] "\n        25.\n    \n    Bumblebee\n    (2018)\n"                                  
    ## [26] "\n        26.\n    \n    Mamma Mia! Here We Go Again\n    (2018)\n"                
    ## [27] "\n        27.\n    \n    The Nun\n    (2018)\n"                                    
    ## [28] "\n        28.\n    \n    Creed II\n    (2018)\n"                                   
    ## [29] "\n        29.\n    \n    Peter Rabbit\n    (2018)\n"                               
    ## [30] "\n        30.\n    \n    The Mule\n    (2018)\n"                                   
    ## [31] "\n        31.\n    \n    The Equalizer 2\n    (2018)\n"                            
    ## [32] "\n        32.\n    \n    A Wrinkle in Time\n    (2018)\n"                          
    ## [33] "\n        33.\n    \n    Fifty Shades Freed\n    (2018)\n"                         
    ## [34] "\n        34.\n    \n    Rampage\n    (2018)\n"                                    
    ## [35] "\n        35.\n    \n    Christopher Robin\n    (2018)\n"                          
    ## [36] "\n        36.\n    \n    Running for Grace\n    (2018)\n"                          
    ## [37] "\n        37.\n    \n    I Can Only Imagine\n    (2018)\n"                         
    ## [38] "\n        38.\n    \n    Smallfoot\n    (2018)\n"                                  
    ## [39] "\n        39.\n    \n    Night School\n    (2018)\n"                               
    ## [40] "\n        40.\n    \n    The First Purge\n    (2018)\n"                            
    ## [41] "\n        41.\n    \n    Game Night\n    (I) (2018)\n"                             
    ## [42] "\n        42.\n    \n    Book Club\n    (I) (2018)\n"                              
    ## [43] "\n        43.\n    \n    The House with a Clock in Its Walls\n    (2018)\n"        
    ## [44] "\n        44.\n    \n    Skyscraper\n    (2018)\n"                                 
    ## [45] "\n        45.\n    \n    Instant Family\n    (2018)\n"                             
    ## [46] "\n        46.\n    \n    Insidious: The Last Key\n    (2018)\n"                    
    ## [47] "\n        47.\n    \n    Green Book\n    (2018)\n"                                 
    ## [48] "\n        48.\n    \n    Blockers\n    (2018)\n"                                   
    ## [49] "\n        49.\n    \n    Pacific Rim: Uprising\n    (2018)\n"                      
    ## [50] "\n        50.\n    \n    Maze Runner: The Death Cure\n    (2018)\n"

``` r
movie_gross <- website %>%
  html_nodes(".sort-num_votes-visible") %>%
  html_text() 
movie_gross
```

    ##  [1] "\n                Votes:\n                460,581\n    |                Gross:\n                $700.06M\n        "
    ##  [2] "\n                Votes:\n                584,843\n    |                Gross:\n                $678.82M\n        "
    ##  [3] "\n                Votes:\n                175,930\n    |                Gross:\n                $608.58M\n        "
    ##  [4] "\n                Votes:\n                197,577\n    |                Gross:\n                $417.72M\n        "
    ##  [5] "\n                Votes:\n                164,947\n    |                Gross:\n                $329.15M\n        "
    ##  [6] "\n                Votes:\n                347,298\n    |                Gross:\n                $324.59M\n        "
    ##  [7] "\n                Votes:\n                23,352\n    |                Gross:\n                $270.60M\n        " 
    ##  [8] "\n                Votes:\n                214,083\n    |                Gross:\n                $220.16M\n        "
    ##  [9] "\n                Votes:\n                194,318\n    |                Gross:\n                $216.65M\n        "
    ## [10] "\n                Votes:\n                203,945\n    |                Gross:\n                $213.77M\n        "
    ## [11] "\n                Votes:\n                232,313\n    |                Gross:\n                $213.52M\n        "
    ## [12] "\n                Votes:\n                247,078\n    |                Gross:\n                $210.88M\n        "
    ## [13] "\n                Votes:\n                178,129\n    |                Gross:\n                $208.91M\n        "
    ## [14] "\n                Votes:\n                47,559\n    |                Gross:\n                $197.63M\n        " 
    ## [15] "\n                Votes:\n                273,870\n    |                Gross:\n                $188.02M\n        "
    ## [16] "\n                Votes:\n                94,960\n    |                Gross:\n                $180.45M\n        " 
    ## [17] "\n                Votes:\n                78,169\n    |                Gross:\n                $174.53M\n        " 
    ## [18] "\n                Votes:\n                32,986\n    |                Gross:\n                $169.96M\n        " 
    ## [19] "\n                Votes:\n                36,393\n    |                Gross:\n                $167.51M\n        " 
    ## [20] "\n                Votes:\n                118,086\n    |                Gross:\n                $159.45M\n        "
    ## [21] "\n                Votes:\n                75,340\n    |                Gross:\n                $159.34M\n        " 
    ## [22] "\n                Votes:\n                97,346\n    |                Gross:\n                $145.44M\n        " 
    ## [23] "\n                Votes:\n                126,878\n    |                Gross:\n                $139.38M\n        "
    ## [24] "\n                Votes:\n                277,824\n    |                Gross:\n                $137.69M\n        "
    ## [25] "\n                Votes:\n                50,494\n    |                Gross:\n                $125.97M\n        " 
    ## [26] "\n                Votes:\n                52,637\n    |                Gross:\n                $120.63M\n        " 
    ## [27] "\n                Votes:\n                78,307\n    |                Gross:\n                $117.44M\n        " 
    ## [28] "\n                Votes:\n                37,360\n    |                Gross:\n                $115.62M\n        " 
    ## [29] "\n                Votes:\n                25,704\n    |                Gross:\n                $115.25M\n        " 
    ## [30] "\n                Votes:\n                18,865\n    |                Gross:\n                $102.74M\n        " 
    ## [31] "\n                Votes:\n                83,152\n    |                Gross:\n                $102.08M\n        " 
    ## [32] "\n                Votes:\n                32,562\n    |                Gross:\n                $100.48M\n        " 
    ## [33] "\n                Votes:\n                40,790\n    |                Gross:\n                $100.41M\n        " 
    ## [34] "\n                Votes:\n                105,887\n    |                Gross:\n                $99.35M\n        " 
    ## [35] "\n                Votes:\n                41,709\n    |                Gross:\n                $99.22M\n        "  
    ## [36] "\n                Votes:\n                480\n    |                Gross:\n                $89.77M\n        "     
    ## [37] "\n                Votes:\n                10,248\n    |                Gross:\n                $83.48M\n        "  
    ## [38] "\n                Votes:\n                18,876\n    |                Gross:\n                $83.24M\n        "  
    ## [39] "\n                Votes:\n                18,036\n    |                Gross:\n                $77.34M\n        "  
    ## [40] "\n                Votes:\n                35,911\n    |                Gross:\n                $69.09M\n        "  
    ## [41] "\n                Votes:\n                149,290\n    |                Gross:\n                $69.00M\n        " 
    ## [42] "\n                Votes:\n                15,989\n    |                Gross:\n                $68.57M\n        "  
    ## [43] "\n                Votes:\n                27,411\n    |                Gross:\n                $68.55M\n        "  
    ## [44] "\n                Votes:\n                70,082\n    |                Gross:\n                $67.80M\n        "  
    ## [45] "\n                Votes:\n                10,437\n    |                Gross:\n                $67.36M\n        "  
    ## [46] "\n                Votes:\n                39,256\n    |                Gross:\n                $67.35M\n        "  
    ## [47] "\n                Votes:\n                63,011\n    |                Gross:\n                $62.46M\n        "  
    ## [48] "\n                Votes:\n                51,871\n    |                Gross:\n                $59.84M\n        "  
    ## [49] "\n                Votes:\n                83,542\n    |                Gross:\n                $59.19M\n        "  
    ## [50] "\n                Votes:\n                85,184\n    |                Gross:\n                $58.03M\n        "

``` r
movie_imdbscore <- website %>%
  html_nodes(".ratings-bar") %>%
  html_text()

movie_info <- tibble(
  title = movie_title,
  gross = movie_gross,
  imdbscore = movie_imdbscore
)

movie_info1 <-movie_info %>%
  separate(title, into = c("title", "year"), sep = "[(]\\d{4}") %>%
  separate(gross, into = c("vote", "gross"), sep = "[$]") %>%
  separate(imdbscore, into = c("imdbscore", "metascore"), sep = "Rate") %>%
  separate(metascore, into = c("what", "metascore"), sep = "X") %>%
  mutate(title = str_remove_all(title, "\n"),
         title = str_trim(title),
         gross = parse_number(gross),
         imdbscore = parse_number(imdbscore),
         metascore = parse_number(metascore)
         )
```

    ## Warning: 1 parsing failure.
    ## row # A tibble: 1 x 4 col     row   col expected actual                      expected   <int> <int> <chr>    <chr>                       actual 1    36    NA a number "\n \n    \n            \n"

``` r
movie_info1 %>%
  separate(title, into = c("number", "title"), sep = "[.]") %>%
  ggplot(aes(x = imdbscore, y = gross)) +
  geom_point() +
  geom_label_repel(data=subset(movie_info1, imdbscore > 8 | gross > 450),
                   aes(label = title),
              check_overlap = TRUE, size = 2.5, nudge_y = 2) +
  labs(title = "The scatterplot of imdbscore by gross income",y = "Gross income(in millions)", x = "imdb score")
```

    ## Warning: Ignoring unknown parameters: check_overlap

![](hw4_files/figure-markdown_github/unnamed-chunk-16-1.png)
