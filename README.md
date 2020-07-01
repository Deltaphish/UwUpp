## UwU++ : Degeneracy evolved©

### warning: WIP

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

## Code example:
### Hello World
~~~~
UwU?
nuzzels "Hewwo World"
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

nuzzels b
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
        nuzzels i
        count iws count pwus 1
    stawp
    i iws i pwus 1
stawp

nuzzels "Towtal numbwa of pwimes:"
nuzzels count
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

nuzzels fiwb(20)
~~~~
