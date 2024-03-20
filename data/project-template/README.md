# Welcome to your project 🎉

PG verify is a CLI application which allows you to develop program graph models
and verify characteristics of those models.

To get started you can run `pg-verify doctor` to verify your installation and guide
you through the steps needed to install external addons like the NuSMV model checker.

You can always run `pg-verify help` to get a list of available commands.
Run `pg-verify help <command>` to get more information about a specific command and its options.
For example try running `pg-verify show png` to render a PNG image of your program graph
and save that to your working directory.

## Project files

There are a couple of prelude files and directories in your project to get you started:

- `program-graph.rb`: This file defines the default program graph you will be working on
- `.pg-verify.yml`: This file can be used to configure pg-verify
- `addon/`: This is the directory where you will place addon resources like the NuSMV executable.

## Writing specifications

The specification framework for the Ruby DSL is inspired by the popular 
testing library [rspec](https://rspec.info/). Here is an example of the syntax:

```ruby
# Wrap two specifications which are related to some "Car" model.
specify "The car" do
    it "won't crash" => :"G distance_to_tree > 0" # Becomes: The car won't crash
    it "will drive"  => :"F velocity > 0"         # Becomes: The car will drive
end
```

You wrap your specification using one or more `specify` blocks.
Within those blocks you can declare a *specification* using the `it` keyword.
Each specification consists of two parts:
- A **text** which is used to describe the spec in natural language
- An **expression** given in LTL or CTL. This will actually be checked

The outer block serves as a wrapper for specifications which are related.
In the example above we declare two specifications which are both concerning
some "Car" model.

You can read those specifications from outside to inside.
The `it` semantically refers to the outer block (the car in our case).
So when reading `it "won't crash"`, **it** refers to **the car**.
Thus this specification becomes "The car won't crash" when it is expanded.

### Assumption blocks

You can consolidate multiple preconditions for your specifications into
an `assuming` block. Here's the syntax:

```Ruby
specify "The car" do
    assuming "the breaks don't fail" => :"G BreakFailure == No" do
        it "won't crash" => :"G distance_to_tree > 0" # Becomes: The car (assuming the breaks don't fail) won't crash
        it "will drive"  => :"F velocity > 0"         # Becomes: The car (assuming the breaks don't fail) will drive
    end
end
```

Much like the specifications themselves, the assuming block expects a `text` and an `expression`.
For each contained specification, the assumption expression will be prepended to the spec
while expanding. Thus for the example above we get two expanded specifications:

```
1) The car (assuming the breaks don't fail) won't crash
( G BreakFailure == No ) => G distance_to_tree > 0

2) The car (assuming the breaks don't fail) will drive
( G BreakFailure == No ) => F velocity > 0
```

### Assuming no errors

The imagined use case for the `assume` block is to specify model properties under exclusion of errors.

```Ruby
specify "The car" do
    assuming no_errors do
        it "won't crash" => :"G distance_to_tree > 0" # Becomes: The car (assuming the breaks don't fail) won't crash
        it "will drive"  => :"F velocity > 0"         # Becomes: The car (assuming the breaks don't fail) will drive
    end
end
```

The thing that's new here is the `no_errors` keyword.
All this does is generate a *text* and *expression* to be used by the `assuming` block.
Say you had three error graphs: `BreakFailure`, `UsageFailure` and `MotorFailure`.
Then `no_errors` would produce this expression:

```
G ( BreakFailure == No && UsageFailure == No && MotorFailure == No )
```

Instead of `no_errors` you can also use `only(...)` to limit the possibility of errors to the
ones you provide. e.g: `assuming only(:BreakFailure, :UsageFailure) do ...`.
