### pi-calculus-interpreter
Interpreter for an (as of yet) unnamed programming language based on the pi calculus.

## Syntax
variable names must be lowercase while channel names must be uppercase. Basic integer arithmetic and boolean logic are implemented.

The following operations are supported.
1. `[new CHAN]{P}`   Create new channel named CHAN and then execute P
2. `[CHAN->var]{P}`  Receive on channel CHAN, bind to variable var, execute P
3. `[CHAN<-var]{P}`  Send variable var on channel CHAN from P
4. `P%Q`             Execute processes P and Q simultaneously
5. `!P`              Repeat P forever
6. `_`               Terminate a process

## Known bugs
Only parsing is available right now
1. simultaneously execution usually fails inside bind/receive/send
2. the sequences `Arith % Term` and `Log % Term` do not parse
