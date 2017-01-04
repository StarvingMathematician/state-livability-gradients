#### Purpose

I was curious about the migration of people across state lines in search of better job opportunities/higher quality of life, and how this would play out in a state such as Florida, which by most quality-of-life metrics is "better" than its neighboring states of Alabama and Georgia, and would therefore be expected to act as a population sink. To explore this idea further I obtained state-level quality-of-life data, population data, etc, and used it to simulate the long-run steady-state distribution of the US population as modeled by a Markov chain in which migration can only occur between adjacent states at each time step. I examined the behavior of two such Markov chains with different edge weightings.

#### File Descriptions

* 'graph_generation2.R' - R code used to compute the Markov chain steady-states and visualize the results

* 'data/*' - contains the data files on which the Markov chains were based

* 'visualizations/difference-weighted_markov_chain_steady-state.png' - transition function is (normalized) P(s1,s2) = max(0,s2-s1); weighted edges shown in black, steady-state shown in red; the initial state is a probability vector where each state's probability is proportional to its population

* 'visualizations/ratio-weighted_markov_chain_steady-state.png' - transition function is (normalized) P(s1,s2) = s2; weighted edges shown in black, steady-state shown in red; the chain is ergodic and therefore independent of initial state

#### Data Sources

* State livability data obtained from the "Quality of Life" subscore in CNBC's "America's Top States for Business 2016" ranking. Scores are out of a possible total of 325. Link to ranking: http://www.cnbc.com/2016/07/12/americas-top-states-for-business-2016-the-list-and-ranking.html

* Link to methodology: http://www.cnbc.com/2016/06/23/americas-top-states-for-business-2016-our-methodology.html

* Adjacency list obtained from here: https://writeonly.wordpress.com/2009/03/20/adjacency-list-of-states-of-the-united-states-us/

* Latitude/Longitude obtained from here: https://inkplant.com/code/state-latitudes-longitudes

* Population data obtained from 2016 estimates here: https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population
