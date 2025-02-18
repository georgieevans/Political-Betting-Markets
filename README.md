# Political-Betting-Markets

## Example

Load the scraping functions:

```{r, message = FALSE}
source("scrape_function.R")
```

This will load three functions. `scrape_bets_wayback()` will scrape historic bettings odds from the internet archives. `scrape_bets_today()` will scrape today's betting odds. Both take a single argument: the url of the bet market from oddschecker. The third function is discussed below. 

To find the url, go to <https://www.oddschecker.com/politics> to find the betting market of interest to you. 

Let's focus on the bets for the most seats in the next UK general election: <https://www.oddschecker.com/politics/british-politics/next-uk-general-election/most-seats>. 


## Today's odds 

First let's have a look at today's odds: 

```{r, message = FALSE, warning = FALSE}
general_election <- scrape_bets_today("https://www.oddschecker.com/politics/british-politics/next-uk-general-election/most-seats")
# Let's look at the data 
glimpse(general_election)
# We can approximately convert betting odds into implied probilities 
# Since bookies are trying to make money, these 'probabilities' won't necessarily sum to 1
general_election <- win_prob_data(general_election)
general_election %>% select(bet, probability, odds, datetime) %>% arrange(desc(probability)) %>% head()
```

## Historic odds

Perhaps we are interested in knowing how these probabilities have evolved over time. The wayback function will find all archived pages with the same url and extract the data. 

```{r, eval = F}
historic_odds <- scrape_bets_wayback("https://www.oddschecker.com/politics/british-politics/next-uk-general-election/most-seats")
# The structure of the output is the same as scrape_bets_today()
# win_prob_data(historic_odds) will add a column for win probabilities
