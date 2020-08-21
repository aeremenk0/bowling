American 10-pin Bowling
Instructions:
Complete as many of the below steps as you can, but do not spend more than about 60-90 minutes or so on the problem.  If you cannot complete all of them in the time allotted, that's OK.  Please do your work in a fresh git repository, committing your changes periodically (as you would typically do during normal development).  When complete, please either upload to github (public or private share is fine), or send a .tar.gz of the entire project (including the .git directory) back to Dave.  If you have any questions, please let us know!

Problem, step one:
See the this link for a problem description.  Given an input string representing a valid sequence of bowled frames, compute the score.  Add some unit tests with the given test cases, and any others you may wish to add.

Problem, step two:
Validate the input.  Identify sequences of input that do not constitute valid games.  Specifically, the number of knocked-down (not bonus) pins in each frame must not exceed 10.  Additionally, there must be exactly 10 frames total (allowing necessary bonus throws).  Expand your test cases to cover this.

Problem, bonus:
Allow partial games.  Score sequences of input that do not yet constitute a full game but are otherwise valid partial games.  Produce the score for the last fully-scored frame (and identify which frame this is, for partial games).  Expand your test cases to cover this.
