# PG DSL

## Installation

To install the project from source, you need [Cabal](https://cabal.readthedocs.io/en/stable/index.html) and [GHC](https://www.haskell.org/ghc/) installed on your system. The easiest way to install is via [GHCup](https://www.haskell.org/ghcup/).

Once this is done, you can go on and install pg-dsl.

### Make sure to cd into the correct directory

If you're currently on the reposiory's top level, you achieve this with `cd pg-dsl`

### Build the project
`cabal build`

### Install the project

To have the project available in your path, run `cabal install`

### Get language support

To have Syntax Highlighting and Code Snippets available, you can install the [PG Tools extension](https://marketplace.visualstudio.com/items?itemName=bsuehling.pg-tools) for VSCode.

## Getting Started

Let's assume we wish to model a robot which works depending on the state of a light barrier.

### Project structure

A pg-dsl project always expects a `main.pg` file on your project's top level. This file contains a model's error graphs, plausibility checks, and hazards. What's more, a model may contain arbitrarily many program graphs. Each program graph has to be specified in a separate file. A program graph file ends with `.pg` and can have an arbitrary name apart from that. It's recommended, though, to name the files after the graph names, i.e., when you have the two graphs *Robot* and *LightBarrier*, your files are best named `robot.pg` and `lightBarrier.pg`. These files can be arbitrarily nested within your project. The compiler finds them automatically. Therefore, a possible directory structure is the following.
```
.
├── graphs
│   ├── lightBarrier.pg
│   └── robot.pg
└── main.pg
```

### The main file - basics

For now the `main.pg` file may look as simple as this:
```
model Robot {

}
```
The `main.pg` file always starts with the keyword `model`, followed by the name of the model - in this case `Robot`. The model's name has to start with an upper case letter. In general, all variable names may only contain roman letters, numbers, and underscores and have to start with a letter. To separate language constructs from each other, we use curly brackes. Line breaks and additional whitespaces will always be ignored by the compiler. The specification of error graphs, plausibility checks, and hazards is saved for later as it builds upon the error graphs which will be introduced next.

### Modelling a program graph - overview

Let's get started with modelling the robot as a program graph. Here's what the basic structure of the file looks like.
```
graph Robot {
    vars {}
    
    states { Idle, Busy, Inactive }

    init: Idle {}

    transitions {}
}
```
As you see, a program graph starts with the `graph` keyword followed by the graph's name which in this case is `Robot`. The graph's name has to start with an upper case letter. The graph then contains building blocks for variables (`vars`), states (`states`), initial state and formula `init`, and the graph's transitions (`transitions`). These building blocks have to appear in this exact order and only once per graph. The `vars` block is optional.

To specify the states of a graph, just list them within the `states` block, separated by commas. All state names have to start with an uppercase letter. So, our robot now has the states *Idle*, *Busy*, and *Inactive*. After the colon in the `init` block, we specify the initial state of the graph. Our graph has `Idle` as initial state. You always have to explicitly specify the initial state.

### Modelling a program graph - variables

We now want to add variables to our graph in order to be able to model more powerfully.
```
graph Robot {
    vars {
        int num [0..7]
        bool truthy
        enum options { option1, option2, option3 }
    }
    
    states { Idle, Busy, Inactive }

    init: Idle {
        num = 0 & ! truthy & (options = option1 | options = option2)
    }

    transitions {}
}
```
There are three types of variables we can define in our program graphs - integers, booleans, and enumeration. All these variables have to start with a lowercase letter.

- An integer is created with the keyword `int`, followed by its name. It always has a lower and upper bound specified in square brackets and separated by two dots. Note that both lower and upper bound are included in the range.
- A boolean is simply created with the `bool` keyword followed by its name.
- An enumeration is created with the `enum` keyword followed by its name. Just as for the states of a program graph, you can specify the possible enumeration values by listing them within curly brackets, comma-separated. Enumeration options have to start with lowercase letters.

From other programming languages, you're probably used to initializing variables with exactly one value, e.g., `num: int = 0` in Python. That's different for pg-dsl. Variables in program graphs are not enforced to have exactly one value, but their possible values are instead narrowed down using logical formulas. In this example, we specify that `num` should indeed be initialized with the value 0. But we could also have written something like `num <= 2 | num + 1 > 5` instead. Apart from that, `truthy` is initialized with the value `false`, as we can deduct from `! truthy`. Finally, `options` has to be `option1` or `option2` initially. We could also have written `options != option3` instead.

Note that we can use arbitray logical formulas for the variable initialization, as long as we comply to the variable's types.
- With integer variables, we can do arithmetics and compare them using comparison operators. Note that integer variables cannot be interpreted in a truthy context, e.g., interpreting 0 as `false` and all other values as `true`.
- With boolean variables, we can do boolean algebra
- With enumeration variables, we can only do comparisons using `=` and `!=`.
- You can also use graph names in your expression, though only in the way `G = s` where `s` has to be a state defined for `G`.

For the remaining part of the tutorial, we will use only the variable `num`.

### Modelling a program graph - transitions

The only thing missing to create a program graph is adding transitions.

```
graph Robot {
    vars {
        int num [0..7]
        bool truthy
        enum options { option1, option2, option3 }
    }
    
    states { Idle, Busy, Inactive }

    init: Idle { num = 0 }

    transitions {
        Idle -> Busy {
            guard { LightBarrier = Signal }
            action { num := num + 1 }
        }

        Busy -> Idle {
            guard { num < 3 }
        }

        Busy -> Busy {}

        Busy -> Inactive {
            guard { num >= 3 }
        }
    }
}
```

As we've already seen earlier, the section in which we define transitions is started with the `trasitions` keyword and surrounded by curly brackets. Each single transition then begins with the schema `<pre-state> -> <post-state>`. Inside curly brackets, we then define a `guard` and an `action`, both optionally. For `Busy -> Busy`, for example, we have neither a guard nor an action.

A `guard` specifies under which condition the transition may be triggered. It has to be any boolean expression. It can use variables from any graph in the same project without explicitly importing them.

An `action` specifies how to modify the graph's variables when the transition is triggered. It has the form `var := expr`. `var` is the variable which we assign a new value to and has to be defined within the same graph. `expr` has to be an expression of the same type as `var`, but - as the variables in `expr` are only read and not written - the used variables are also allowed to be defined in a different graph, just as it's the case for guards. When you define an action where you modify an int variable, the compiler will do quite simple range checks, but it does not guarantee variables' range are complied to in an action.

When you want to assign new values to more than one variable within an action, you can seperate the assignments with semicolons:

```
action { var1 := expr1; var2 := expr2 }
```

Congratulations! You now know everything to define your program graphs.

### The main file - specifications

Now that we've learned how to build graphs, let's see how we can do safety analysis with them. Let's therefore assume we've already defined a second graph `LightBarrier`:

```
graph LightBarrier {
    states { Idle, Signal }

    init: Idle {}

    transitions {
        Idle -> Idle {}

        Idle -> Signal {}

        Signal -> Idle {}

        Signal -> Signal {}
    }
}
```

Firstly, we wish to find out whether the model we specified is safe when all transitions work as expected. For this, we can add plausibility checks within a `specify` block:

```
model Robot {
    specify {
        "Once inactive, always inactive" {
            G (Robot = Inactive => (G Robot = Inactive))
        }
        "num never reaches 5" {
            G num < 5
        }
    }
}
```

Each specification begins with a name; this name is defined within quotation marks so that you can use as many white spaces as you wish. After the name, the actual specification follows once again in curly brackets. You can define specifications either in linear temporal logic (LTL) or computation tree logic (CTL). You can of course add arbitrarily many specifications after each other.

### The main file - error graphs

We now want to be able to check what happens when parts of the system don't work as expected. This way, we can guarantee the whole system is still operable when single parts fail. We do this using error graphs. As they always have the same strucure, there's no need for you to define them as real graphs in separate files; you can instead define them directly in the main file within the `errors` block.

```
model Robot {
    errors {
        persistent PersistentError
        transient TransientError
    }
}
```

As you see, defining an error graph is as simple as deciding between `persistent` and `transient` errors and given the graph a name. In the background, a persistent error looks like this

```
graph PersistentError {
    states { No, Yes }

    init: No {}

    transitions {
        No -> No {}
        No -> Yes {}
        Yes -> Yes {}
    }
}
```
and a transient error like this.

```
graph TransientError {
    states { No, Yes }

    init: No {}

    transitions {
        No -> No {}
        No -> Yes {}
        Yes -> No {}
        Yes -> Yes {}
    }
}
```

We can now use these error graphs within our other graphs as we had defined them explicitly like above.

```
graph Robot {
    vars {
        int num [0..7]
        bool truthy
        enum options { option1, option2, option3 }
    }
    
    states { Idle, Busy, Inactive }

    init: Idle { num = 0 }

    transitions {
        Idle -> Busy {
            guard { LightBarrier = Signal }
            action { num := num + 1 }
        }

        Busy -> Idle {
            guard { num < 3 }
        }

        Busy -> Busy {}

        Busy -> Inactive {
            guard { num >= 3 }
        }

        Inactive -> Idle {
            guard { PersistentError != No }
        }
    }
}
```

We have just defined a new transition for the graph `Robot` which leads from `Inactive` to `Idle` and may be triggered only when `PersistentError` is not in state `No`.

In our plausibility checks, we always have the keyword `nofaults` available. This keywords always evaluates to an expression where all error graphs are in the state `No`, in this case to

```
PersistentError = No & TransientError = No
```

This comes in handy when you want to execute the plausibility checks under the assumption that no errors ever occur:

```
model Robot {
    errors {
        persistent Errorgraph
    }

    specify {
        "Once inactive, always inactive" {
            G nofaults =>
            G (Robot = Inactive => (G Robot = Inactive))
        }
    }
}
```

### The main file - hazards

Hazards in program graphs are model states we want to avoid under all circumstances. We can define them using the `hazards` block.

```
model Robot {
    errors {
        persistent Errorgraph
    }

    hazards {
        "num ist zu gross" {
            num = 5
        }
    }
}
```

The hazards are defined just like plausibility checks: each one starts with a name in quotation marks, followed by the LTL or CTL formula.

We can later use these hazards to automatically execute a DCCA.

## Integration into the PG Tools framework

Using pg-dsl, you can still enjoy all advantages of the pg-tools framework, like automated DCCA, execution of plausibility checks, and PlantUML visualizations of your program graphs. For this to work, you need the pg-tools ruby gem installed. The pg-dsl compiler will then create a JSON output and call the correct pg-tools function automatically. The pg-tools function will then read the JSON and the output will be in the same format as when using the ruby dsl directly.

These are the commands available in pg-dsl:

Just do the transformation to JSON for the project located at path `fp`. Default value for `fp` is `.`:
```
pgdsl [fp]
```
The following flags are available, though may not be used at the same time:

Run plausibility checks:
```
--test | -t
```
Run a simulation:
```
--simulate | -s
```
Execute a DCCA:
```
--dcca | -d
```
Do a transformation to PlantUML, JSON, or YAML:
```
(--show | -o) (puml | json | yaml)
```
The flags have to occur between pgdsl and the filepath specification.


## Examples

- [Robot](./examples/robot/)
- [Pressure tank](./examples/drucktank/)


Congrats, you've just finished the introduction to pg-dsl. Have fun implementing your own models!