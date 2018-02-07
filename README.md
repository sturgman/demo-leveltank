# Can you beat the controller?

This is an implementation (and proof of concept) of a level feedback controller for a tank with inlet and outlet liquid streams. Serves as a demonstration of why automatic process control is important for chemical processes.

A live version of the demo can be found [here](http://paws.kettering.edu/~sturgmancohen/realtime "Live Version of the Demo").

Inspiration came from:
  * [TechTeach](http://techteach.no/simview/)
  * [LearnChemE](http://www.learncheme.com/simulations)
  
The nice thing about this implementation is that it can be used directly from the browser.

# To Do

This is still a very raw implementation although it has already used in class and it accomplishes its basic objectives. A list of features/changes that are planned or desired:

  [] Add styling via css.
  [] Implement a time delay in the mathematical model. This will require adding the history of other parameters to the model.
  [] Streamline the way the math model is provided.
  [] Make the plot self-rescaling. Initially the time range of the plot would be something like 30 and every time the data reaches the end of the plot rescales.
  
# Main objective

The ultimate objective is to build an elm package or framework that enables someone to specify a set of parameters and mathematical models from which simple (opinionated) simulation demos are generated. Obviously I am still far away from such an objective.
