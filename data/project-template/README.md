# Welcome to your project 🎉

PG verify is a CLI application which allows you to develop program graph models
and verify characteristics of those models.

To get started you can run `pg-verify doctor` to verify your installation and guide
you through the steps needed to install external addons like the NuSMV model checker.

You can always run `pg-verify help` to get a list of available commands.
For example try running `pg-verify show png` to render a PNG image of your program graph
and save that to your working directory.

## Project files

There are a couple of prelude files and directories in your project to get you started:

- `program-graph.rb`: This file defines the default program graph you will be working on
- `addon/`: This is the directory where you will place addon resources like the NuSMV executable.
