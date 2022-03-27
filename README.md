## About this project

The idea behind this project was to develop an **[educational app](https://lthevenard.shinyapps.io/decision-theory__lotteries/)** to help Law students better understand the concept of expected value. It is part of a course on Decision Theory at **[FGV's Rio de Janeiro Law School](https://direitorio.fgv.br)**, in which the students have to grapple with the problem of how to evaluate multiple 'States of the World' in the context of decisions under risk. In the course, we use the metaphor of a lottery as a simplification of this evaluation problem, and the first evaluation method the students learn is to calculate the expected value of a lottery that has finite discrete outcomes and known probabilities. In this context, the problem becomes: how much would you be willing to pay for a lottery ticket that can give you a prize X, Y, Z ... with probabilities Px, Py, Pz ..., respectively?

While preparing the course, the idea then came to their teacher to use a little bit of programming to simulate different lotteries, and to compare the actual results of these simulations with the expected value of each lottery. The goal is to let the students play with different scenarios, so they can better understand why – and convince themselves that – the mean outcome of a probabilistic experiment tend towards its theoretical expected value when the experiment is reproduced many times. 

Another objective of this app is to show the limitations of the concept of expected value, particularly it's disregard - as a measure of central tendency - for the dispersion of the outcomes in different lotteries. This is why the app offers the possibility of comparing two lotteries, and the idea is to compare two lotteries with different sets of outcomes and probabilities, but with the same expected value.

### Structure of the app

The app currently accepts lists of payoffs and probabilities (entered as text inputs) and a maximum number of lottery tickets (N) for the simulations. The user selects N from a slider input that creates an integer with a minimum value of 100 and a maximum value of 1000. In future versions of the app I intend to focus on efficiency, as the simulations currently make the app a bit slow, specially at N = 1000. 

In the back-end, there are two important sets of reactive expressions that make the app run when the user presses the 'Simulate' button. We could think about these reactive expressions as a *description* and a *simulation*. 

The `describe()` reactive expression generates basic information about the lotteries, from the text inputs the user has provided. These include the calculation of expected value and theoretical standard deviations for each lottery, the naming convention used to describe the lotteries and a few other background values used in the app. From the output of the `describe` function, the app generates all the outputs in the `Lottery 1` and `Lottery 2` tabs.

The `simulate()` reactive expression takes some of the description information and generates simulations that represent buying 1 to N tickets, for each lottery. The app then outputs different representations of these simulations, which include an animated bar graph showing the percentage of each outcome for different values of N in both lotteries, two scatter plots showing the mean returns of the experiments in reference to the expected value of each respective lottery, and two tables with the complete results of the simulations. The simulation tables contain the following columns:

* **Tickets**: the number of tickets simulated in each experiment.
* **A, B, C...**: columns for each possible outcome, indicating the total number of times that outcome has occurred in each experiment.
* **Returns**: the total sum of the payoffs of all outcomes that have occurred in each single experiment.
* **Mean Returns**: the mean payoff received in that single experiment, calculated as: `Returns / Tickets`.
* **Profit**: the total amount of returns minus what a buyer would have spent if he had payed the expected value of the lottery to obtain each ticket, calculated with the formula `Returns - (EV * Tickets)`, where EV indicates the expected value of the lottery.

### Hope you like it!

I sincerely hope you enjoy this experiment. Please note that this app was developed in a hurry by a young teacher in the middle of a turbulent semester. Many improvements can be made to the current code, and I will strive to make the app a little better every time a have the chance to play with it for a while. 

Cheers,

LT
