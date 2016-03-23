# Ranking-hospitals-in-all-states

"rankall.R" takes two arguments: an outcome name (outcome) and a hospital rank-
ing (num). The function reads the outcome-of-care-measures.csv and returns a 2-column data frame
containing the hospital in each state that has the ranking specied in num. For example the function call
rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
are the best in their respective states for 30-day heart attack death rates. The function should return a value
for every state (some may be NA). The rst column in the data frame is named hospital, which contains
the hospital name, and the second column is named state, which contains the 2-character abbreviation for
the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
hospitals when deciding the rankings.
