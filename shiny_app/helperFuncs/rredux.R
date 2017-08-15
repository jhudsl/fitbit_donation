#THe beginings of the package for statecontainer management in R modeled after REDUX>
stateManager <- R6::R6Class("stateManager",
                            public = list(
                              state = NULL,
                              reducer = NULL,
                              initialize = function(state = reactiveValues(), actionReducer = NA) {
                                # Initialize the state. 
                                self$state <- actionReducer(state, action = list(type = "INITIALIZE"))
                                self$reducer <- actionReducer
                              },
                              dispatchAction = function(action) {
                                self$state <- self$reducer(state = self$state, action)
                              }
                            )
)
