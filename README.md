# Apollo
Apollo is a basic dynamically typed programming language which features a familiar C-style syntax and has an interpreter written in Rust.

## Features
Currently, Apollo is very new and while being usable, it is lacking features. Some features currently implemented are:
* Functions
* Basic Objects
* Closures
* Lambdas
* Conditions
* Loops
* I/O

## Example Program
```
def counter() {
  let count = 0;
  return def(n) print(count += n);
}
let example = counter();
for (let i = 0; i < 10; i += 1)
  example(i);
```

## Usage
You can launch into the REPL by running without arguments: `apollo`.  
Or, if you prefer to run code from a file: `apollo -f /path/to/file.ap`.  
Run `apollo -h` for further help.

## Contributing
Just open a pull request! They are *very* welcome.
