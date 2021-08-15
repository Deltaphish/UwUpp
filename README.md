![Logo](./UwUpp-logo.png)

## UwU++ : Degeneracy evolved©
### warning: WIP

### Index:
- [Information](#information)
- [Installation](#installation)
- [Examples](#examples)

## Information:

### Syntax spec in the wiki
* Have you ever felt that you can't express yourself with todays languages? <br>
* Do you have problem of being too respected as a programmer/human being? <br>
* Are you a person who enjoys pain? <br>

UwU++ is a imperative programming language which aims to compete with brainf*ck to be the most painfull programming language to read and write in.
As the next evolution of UwU-Lang, it retains all of the "charm" with new features like:

* Arrays
* Functions
* <,=,> comparators
* division,minus operators
* If statements
* Actual error messages in the parser
* Runtime printing

TODO:
* Compiler
* Webasm target

# Installation
`UwUpp` is in the AUR as `uwupp-git`. Its only dependency is `stack`. For all other systems:

1. Install [stack](https://docs.haskellstack.org)
2. Enter the root folder of this project and run `stack install`
3. The previous command will place the interpreter binary in the directory given by `stack path --local-bin`, usually `~/.local/bin`.

# Examples

### Hello World
~~~~
UwU?
nuzzels("Hewwo World")
~~~~

### finds the first Fibonacci number above 100
~~~~
UwU is this fibonacci?

a iws 1
b iws 1
c iws 0

OwO *notices 100 gweatew twan b *
    c iws b
    b iws b pwus a
    a iws c
stawp

nuzzels(b)
~~~~

### print all prime numbers under 1000
~~~~
UwU is twis the numbwe of pwimes undwe 1000?
wimit iws 1000
siewe iws awway<wimit>
i iws 2
OwO *notices i wess twan wimit diwide 10*
    *notices siewe[i] eqwall twoo 0*
        j iws i twimes i
        c iws 0
        OwO *notices j pwus c wess twan wimit*
            siewe[j pwus c] iws 1
            c iws c pwus i
        stawp
    stawp
    i iws i pwus 1
stawp


i iws 2
count iws 0
OwO *notices i wess twan wimit*
    *notices siewe[i] eqwall twoo 0*
        nuzzels(i)
        count iws count pwus 1
    stawp
    i iws i pwus 1
stawp

nuzzels("Towtal numbwa of pwimes:")
nuzzels(count)
~~~~

### Print the nth Fibonacci number, now with recursion! 

~~~~
nyaa *fiwb(a)*
    *notices a gweatew twan 1*
        c iws fiwb(a minwus 1) pwus fiwb(a minwus 2)
    stawp
    *notices a eqwall twoo  1*
        c iws 1
    stawp
    *notices a eqwall twoo 0*
        c iws 0
    stawp
wetuwn c

nuzzels(fiwb(20))

~~~~

### UwU++ webserver

~~~~
UwU? iws twis a websewer?
UwU Run this as "while true; do UwUpp-exe ./examples/webserver.uwu | nc -l 9090 -q 1; done"

http iws "HTTP/1.0 200 UwU iws twis a websever???\r\n Server: UwU++\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n"


UwU? is this dynawic contwent?
nyaa *fiwb(a)*
    *notices a gweatew twan 1*
        c iws fiwb(a minwus 1) pwus fiwb(a minwus 2)
    stawp
    *notices a eqwall twoo  1*
        c iws 1
    stawp
    *notices a eqwall twoo 0*
        c iws 0
    stawp
wetuwn c

dynawic iws fiwb(20)

htmlStawt iws "<html>
                <head></head>
                <body>
                    <h1>"

htmlStawp iws " </h1>
            </body>
        </html>"

nuzzels(http pwus htmlStawt pwus dynawic pwus htmlStawp)
~~~~
