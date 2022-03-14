## Problems & Solutions



## Usage

This project runs on Scala `2.13.1` and sbt `1.3.3`.

Use the following template to write a solution:

```Scala
package adventofcode.solutions

import adventofcode.Day

object Day01 extends Day(1) {

  override def solutionA = ???

  override def solutionB = ???

}
```
(change `1` to the current problem day and fill in the `???`)

Then, to run your code start by entering the sbt shell:
```
$ sbt
```

And type the following command:
```
> day 1
```

The output will be printed to the console and stored to the corresponding files in `output/`.

Alternatively, since `Day` extends `App`, all singleton children can be run as regular applications.

## License

This repository is licensed under the MIT License, please refer to the `LICENSE` file.
