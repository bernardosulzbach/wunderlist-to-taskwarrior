# Wunderlist to Warrior

## Configuration

You must get an API key from Wunderlist before using this for your own stuff.

Create a file at `~/.tokens.json` file with the following contents:

```javascript
{
   "clientId":     "6502................",
   "clientSecret": "2684........................................................",
   "accessToken":  "bcc4........................................................"
}
```

Note that you must obtain these values for yourself, the dots are just there to
give you an idea of how long each field is.

## Installation

A simple project that fetches your tasks from Wunderlist and inserts them into
TaskWarrior.

```sh
# Build the project.
stack build

# Install the executable.
stack install

# Run the test suite.
stack test

# Run the benchmarks.
stack bench

# Generate documentation.
stack haddock
```
