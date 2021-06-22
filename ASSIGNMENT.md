## Deadline: 23:59 (UTC + 3), 22 June

You will need to create your CLI (command line interface). Further
describes the functionality that needs to be supported.

### Commands

Your CLI should support the following commands:
1. `cat`, `echo`, `wc` - these commands should work in the same way as they behave in a Unix terminal. You don't need to support additional flags.
2. `grep` - basic functionality similar to grep from a Unix terminal. Input to the command is:
regular expression and text in either argument or `stdin`. The output is expected to list all lines of text in which the given pattern occurs. Two flags must be supported: `-A n` and` -B n`. When they are specified, in addition to the line with the pattern, `n` subsequent (after) / previous (before) lines are displayed.
3. `shell` is a command that can be passed any string as an argument. The line should be executed on a Unix terminal, and the corresponding `stdout` should be printed on your terminal.

### Environment Variables

Your terminal should be able to set and use environment variables.
Variables must be set using an expression like `export <var_name>=<var_val>`.
Inserting variables into commands should be done similarly to the corresponding functionality from a Unix terminal - using the `$` symbol.

It is important that our CLI has its own environment variables - i.e. environment variables from a Unix terminal can be ignored.

Repeat the behavior of a Unix terminal when working with environment variables inside single and double
quotes.

Examples of using environment variables:
```bash
> export varName="aaaa"
> echo $varName
aaaa

> export otherVar="bbb$varName"
> echo $otherVar
bbbaaaa

# An important example! Other commands can be written to environment variables.
> export echoVar=echo
> $echoVar "echo this"
echo this
```

### Pipelines

Your terminal should be able to combine commands into pipelines. This is the mode in which the `stdout` of one command is passed as` stdin` to another command.
And so for an arbitrary number of sequential commands.

Example:
```bash
> cat test1.txt | grep -B 2 ".*le.*"
Valjean, at last
We see each other plain
Monsieur le Maire
------------------------------------------
Before you chain me up like a slave again
Listen to me, there is something I must do
This woman leaves behind a suffering child
```


## Additional requirements

### stack project

CLI should be designed as a stack project. You can create a new stack project using
`stack new <name_of_project>` commands.

### Logical division into modules

Your project should have a logical division into modules. Write code
the whole CLI in one file is strictly prohibited. Don't forget to take out the domain types
into a separate module `Type.hs`.

### Tests

All the core functionality of your CLI should be covered by tests that can be run
using the `stack test` command. Recommended as a test framework
use [hspec](https://hackage.haskell.org/package/hspec).