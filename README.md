# URSL?

-   Universal Reduced Stack Language
-   Usably Reduced Stack Language
-   Universal Redstone Stack Language
-   Usable Redstone Stack Language
-   ```js
    /U(niver)?sab?l[ey]? Red(uced|stone) Stack Language/
    ```

Note that in this document, sequences where multiple "instructions" are written one after each other inline, they are formatted like ``inst``; ``inst`` with a semicolon separator, even if the language is supposed to use newlines to separate them.

To use the compiler in this document, first [install rust](https://rustup.rs/), and then just do ``cargo run -- -i input.ursl -o output.urcl`` with the flags at the end there as you like. (``--`` tells cargo to stop parsing arguments, otherwise ``cargo run --help`` would give you help stuff for ``cargo run``) Do ``cargo run -- --help`` for all the goodies that you can customize. Binaries are not distributed in this repo, but you're free to compile it and do whatever with the resulting binaries.

URSL is an abstraction which is somewhat higher than URCL. It is very similar to WASM text format and .NET CIL. URSL is a stack-oriented language with functions and label scopes within those functions. It is designed to be as easy as possible to compile to URCL, which is why for example memory instructions are literally 1:1 on URCL's available memory instructions. I plan on using this to compile languages such as .NET CIL and WASM to URCL. Stack machines allow for a simplified parser and binary representation of code, because instructions never take more than one immediate operand, and most only take from the operand stack. They are also somewhat easier to compile *to*, because it allows for very simple representation of nested expressions in reverse polish notation, and lowering of code can just translate to a set of stack instructions, without worrying about such things as temporary registers and using the correct available one, because URSL handles that and ensures a register is always available.

Just like WASM text and CIL, instructions are written in lowercase. This helps it look sorta like URCL, but obviously different just by the casing. Oh, and also, most instructions are written as actual english words, because i think it's a lot nicer to read, and URSL's primary purpose isn't to be written by a human, so it's not a huge concern for instructions to be short and faster to write. Some are still abbreviated if their name is actually long, but i'm not keeping it to 3 chars.

Instructions that have signed/unsigned variants generally give the name to the signed variant, and the unsigned variant gets a prefix of ``un``. This is the opposite of URCL, but i like it better. It also matches CIL.

The stack pointer (SP) and program counter (PC) are entirely opaque to URSL code. Memory is not abstracted in any way for URSL, and it is possible to overwrite the callstack, which will cause issues, obviously. Numeric literals in URSL are always unsigned, because i don't wanna deal with signed number literals in the output. The compiler internally keeps track of everything as ``u64``, which is why you cannot use URSL with ``BITS >= 65`` unless you would like to modify the compiler itself to use some kind of bigint. If you want to use negative numbers, you should load them like ``const 0``; ``neg``

# Predefined data

At the start of the file, there can be predefined data to keep in RAM. All data definitions must be labeled with a data label (``.name``), which is followed by a literal which is just the same as the ``DW`` operand in URCL (char, number, or array of either), and compiles directly to that instruction. Newlines are technically not signficant in URSL, at least not any more than other whitespace (carefully done to create a simpler grammar) which means that you're free to format arrays however you think makes sense. All definitions are outputted as ``DW``s in the same order, but i really don't recommend you try to do any arithmetic on the pointers to them, and there is no guarantee of what happens if you do so. An exception to this is obviously arrays, whose behaviour is well defined until the end of the array.

``RUN RAM``/``ROM`` is not distinguished in URSL. Depending on the behaviour of the target ISA, either one of these may be fit. URSL expects ``DW``s to be writable, but it will never try to read or write from an instruction label, or jump to any value that isn't an immediate label. URSL respects that instructions may be stored in addressable memory, and does not require ``#0`` to be a specific value it can figure out just by the data definitions. Any pointers that are outside the heap (less than ``#0``) are undefined behaviour in URSL, except for data labels.

# Core concepts

At any given point in code, the operand stack height is known statically. That's because the operand stack is internally stored as registers, which are not dynamically indexable. What URCL refers to as "the stack" is used as a callstack in URSL, and that's how i will refer to it. "the stack" in URSL is ambiguous, but usually refers to the operand stack, which again, isn't stored as a stack, but in the registers. URSL does not have a concept of "registers", but it does have local variables.

The callstack is where arguments and locals are located. Just as with WASM, they are accessed using two unified instructions which i called ``get`` and ``set`` (as opposed to .NET's  ``ldarg``, ``ldloc``, ``starg``, ``stloc``). It takes one immediate value, which is the index of the local variable. ``get 0`` is the first argument, and for example if there are 2 arguments, ``get 2`` will be the first local variable.

Stack entries start at ``R1`` and as you load more, they will expand towards higher-valued registers.

For example, this code:

```ursl
const 1
const 2
const 3
add
add
```

will calculate the value of ``1 + (2 + 3)`` (the code is essentially reverse polish notation for this expression). When compiled to URCL, it will look like this, assuming the stack was empty (height 0):

```arm
IMM R1 1
// stack height is 1
IMM R2 2
// stack height is 2
IMM R3 3
// stack height is 3
ADD R2 R2 R3 // after reading operands, stack height is 1, this is why it uses the same register
// stack height is 2
ADD R1 R1 R2 // after reading operands, stack height is 0, this is why it uses the same register
//stack height is 1
```

If the stack was not empty such as with a height of 1, then all the registers will be shifted upwards so the code above becomes ``IMM R2 1``; ``IMM R3 2``; ... and so on with the last add being ``ADD R2 R2 R3``.

# Functions

Because of how functions work in regards to stack manipulation, they are implemented specially in URSL, and are not just label jumps with a return pointer.

Functions are declared using a syntax like ``func $name args -> returns + stacklocals``, where ``args``, ``returns``, ``stacklocals`` are all numeric literals. The stack behaviour of calling a function is determined by the ``args -> returns`` part. The ``+ stacklocals`` part is optional, defaulting to zero. The stack behaviour part is also optional, defaulting to zero also. That's nice especially for the ``$main`` function, which minimally is declared only as ``func $main { ret }``. The ``$main`` function must take zero arguments and return zero values. It can have locals, and it is the entrypoint of a URSL program. Additionally, if a function returns zero values, it can "fall out" of its block with an empty stack. (i.e. the ``ret`` at the end is optional for zero-returning functions)

Inside a function, the operand stack starts with the values of the arguments, and ``ret`` must have stack height equal to the return count. This makes ``ret`` pretty much translate directly to a URCL ``RET`` instruction when no stack locals need to be deallocated, with all the heavy lifting being done at the callsite. For most local variables, i recommend you use [``let`` bindings](#let-bindings) to keep them in the registers, but if you need the **address** of a variable to pass it indirectly, you should use stack locals. That will allocate space on the callstack for additional locals, and you can use the ``stack`` instruction to load their address.

When calling a function like ``call $example``, its input arguments is however many off the top of the caller's stack it should receive. Take, for example, ``$example 2 -> 3`` (has 2 arguments, and returns 3 values), and the caller has 4 items on their stack. It should translate into this URCL code:

```arm
// unload extra op stack entries to callstack
PSH R1
PSH R2
// arguments are passed as the bottom registers
// 2 arguments being passed
MOV R1 R3
MOV R2 R4
// return pointer
PSH ~+2
JMP .example // the label may be mangled
// return from function: stack height is 5 but we need to get the extra 2 as well.
// the return pointer was popped, so only our two extra operands are on the stack
// we need to shift everything up 2 regs
MOV R5 R3
MOV R4 R2
MOV R3 R1
// and pop the last 2 operands
POP R2
POP R1
// continue with the next instructions
```

For function pointers to work

# Function pointers

You can use function pointers in URSL. Functions are "constant values", much like data labels, and can appear in the data section or inside a ``const`` instruction. This will load the value of its mangled label onto the stack, and erases the signature. You then *must* remember its signature externally, and you can call it with ``icall``. The ``icall`` instruction takes a function signature as an immediate argument, and it behaves exactly as ``call``, except it takes one more stack operand than the arguments. The arguments are at the top of the stack, and just below all of the arguments is the pointer to the function to call. Take for example, the stack height is 5, and you do ``icall 2 -> 3``. That will consume the top *3* stack operands, output 3 more and translate to the following URCL code:

```
// unload extra op stack entries to callstack. Notice that there are only two extras here, not (5 - 2) = 3, because top is the function pointer
PSH R1
PSH R2
// 2 arguments being passed
MOV R1 R3
MOV R2 R4
// return pointer
PSH ~+2
// call the function
JMP R5 // The function pointer does not need to be shifted down
// return from function: stack height is 5 but we need to get the extra 2 as well.
// the return pointer was popped, so only our two extra operands are on the stack
// we need to shift everything up 2 regs
MOV R5 R3
MOV R4 R2
MOV R3 R1
// and pop the last 2 operands
POP R2
POP R1
// continue with the next instructions
```

It is important to put emphasis on the exact place a function pointer exists in. Take this example:

```
// arguments
call $example
```

The equivalent indirect call is **NOT** this:

```diff
- const $example
// arguments
icall 0 -> 0
```

It is this:

```diff
// arguments
+ const $example
icall 0 -> 0
```

# Labels and jumps

In URSL, instruction labels are marked with ``:`` prefix, and data labels with a ``.`` prefix. This allows easier differentiation, as the two kinds of labels are used quite differently, and in URSL they are not even interchangable.

The stack height is enforced across jumps. Jumps to a label must either have the correct stack height from the instruction immediately preceding the label, or the label must be preceded by a ``height`` directive, which is always preceded by an unconditional control flow instruction (that is, ``ret``, ``halt``, ``jump``, which never execute the instruction after). The ``height`` directive informs the compiler of the expected stack height in the next instruction, in cases where the compiler does not know.

# Let bindings

A ``let`` binding will bind the values at the top of the stack to the names given. There must be at least one name. The ``let`` binding also includes a scope of instructions, after which the values that were bound are popped. ``let`` bindings are a performant way to have local variables without using the stack.

The instructions inside a ``let`` binding's scope are verified as if it starts with 0 *effective* height, for underflow purposes, but the given names can be used in the ``get``/``set`` instructions to get and set its value. Jumps can go across ``let`` scopes, in which case their *actual* heights must match. This can involve completely different bound names. ``height`` directives inside ``let`` bindings are verified using the *effective* height.

The order of ``let`` bindings' names are written from the bottom of the stack, with the last name being the top entry.

Here is an example, to clear up some things above:

```
// fill stack with some excess items
const 0
const 0
const 0
let a b {
    // pop // this would be an error: stack underflow
    get a
    get b
    add

    height 1 // effective height of 1
    const 0
    eq
    branch :outside // valid! *actual* height is 3 + 1 = 4 (and branch consumes 1)

    height 0 // effective height of 0
    // add 2 more for an actual height of 5
    const 0
    const 0
} // adds 1 extra
pop pop
halt

height 0 // height 0, add 3 things
const 0
const 0
const 0
// here, stack is 3 unnamed items, but due to branch above, might be named bindings that now live longer than its scope
:outside
halt
```

Which should translate to this URCL:

```arm
IMM $1 0
IMM $2 0
IMM $3 0
// let start: a = $2, b = $3
MOV $4 $2
MOV $5 $3
ADD $4 $4 $5
IMM $5 0
// consumes 4 and 5, leaving 3 regs
BRE .outside $4 $5
// this is the end of the let binding: a and b are removed now, and everything above is *shifted down*
MOV $2 $4
MOV $3 $5
// pops that consume $2 and $3
NOP
NOP
HLT
// height 0
IMM $1 0
IMM $2 0
IMM $3 0
// here you can clearly see, only 3 regs are used, so despite the effective height of the branch being 1, this is completely legal
.outside
HLT
```

``ret`` consumes the effective height in the given ``let`` scope. That is, the bindings that contribute to the height when jumping do *not* contribute to the height when returning. This is done for consistency, so that ``ret`` and the end of a ``let`` scope is the same as putting it after the scope.

# Custom instructions

Custom instructions are declared with the ``inst`` keyword, and they act like custom URSL instructions, meaning they don't have the ``$`` prefix and are inlined into the caller. They are not called with the ``call`` instruction, but just by naming them as any other instruction.

Custom instructions are not implemented in URSL, but in a syntax more similar to URCL. This functionality exists so that you can use new URCL instructions without needing me to add them to URSL, and for cases where you need lower level access to URCL. Also, most of the default instructions are defined in terms of  which allows URSL to have a smaller compiler that doesn't explicitly define how to emit boring instructions that only take a single line for the translation in this document.

Custom instructions are not emitted exactly as the original code (some transformations do modify it, which i'll explain later), but all instructions are mapped one-to-one in outputted code in the same order.

Custom instructions have the instruction/data label abstraction that URSL has, meaning that jumps look like ``JMP :label``, not ``JMP .label`` (as they do in URCL). Every instruction in a URCL function can be labeled, and they are converted into relatives for the output, since i didn't wanna deal with unique stateful mangled labels. An instruction can only have *one* label. Data labels can only appear as source operands, and instruction labels can only appear as destination operands.

In custom instructions, there are 3 operand kinds: instruction label, immediate value, and registers. Immediate values are the same as the arguments to the ``const`` instruction in URSL. Memory locations are immediate values, and unlike in URCL, they cannot start with ``M``. They must be prefixed with ``#``. Same with registers, those are always ``$``. This allows me to make a simpler parser, because if ``R`` was allowed, then the instructions cannot be arbitary (i.e. they are parsed with a regex like ``\w+``), since it would cause an ambiguity whether an instruction has 3 operands or if it has 2 operands and a new instruction starts. Unlike URCL, in URSL all whitespace is equivalent, including newlines and comments, so i did not want to make the grammar newline sensitive just because you want ``R``/``M`` prefixes.

Almost all URCL(-like) instructions take the form of ``OPCODE Destination Source`` or ``OPCODE Destination Source1 Source2``. ``Destination`` is either a register, or an instruction label. This allows arbitrary branch instructions that may be added to URCL in the future. By design, the URCL opcode really doesn't give the compiler any info except for the 3 special cases, so therefore if the destination must be allowed to be a label so ``BRZ :some_other_place $1`` would work. The source operand is either a register, or an immediate value. Immediate values include data labels. URSL can be broken by jumping to a register within a custom instruction, because the compiler assumes that the destination being a register means it is a calculation, and the register being a label means it is a jump.

There are three specially handled instructions:

- ``IN $0 %PORT`` The source operand can be a port. The parser still allows it to be a generic instruction taking anything else as the source operands too (i.e. ``IN $0 $0`` is a regular instruction that doesn't actually exist in URCL). However, when the source for ``IN`` is a port, the destination must be a register.
- ``OUT %PORT $0`` The destination operand can be a port and if so, the instruction must have exactly one source operand (of register or imm)
- ``JMP :label`` The destination can be a label, and this instruction can have zero source operands. This is the only instruction that does not have 2 or 3 operands that this URCL-based syntax supports.

These rules are carefully chosen to intentionally leave out this functionality:

- ``PSH $0`` and ``POP $0`` The stack in URSL is exclusively used as a callstack. ``urcl`` functions may not break this rule.
- ``CAL $function`` You may expect to be able to call other functions from ``urcl`` functions (including URSL functions) but i did not want to bother implementing a way to do this safely.
- ``CAL :function`` I do not want to encourage you to write complex algorithms in URSL via the URCL sub-syntax. You can use jumps, but not ``CAL``s.
- ``RET`` You might expect this to work like the URSL ``ret``, but that is very different from URCL's ``RET``. I did not want to allow any instructions that do not map one-to-one, and the URCL definition of ``RET`` violates the same rule as ``PSH`` and ``POP`` would. Instead, you should have a ``NOP`` at the end labeled like ``:ret NOP`` and then ``JMP :ret``.
- ``HLT`` URSL has built-in support for the ``halt`` instruction, which translates to ``HLT``. In ``urcl`` functions, i did not think it is sufficiently useful to be able to halt within. And i did not want to deal with any kind of analysis within ``urcl`` functions, such that ``halt`` could be implemented as a ``urcl`` function.
- ``SP`` and ``PC`` These are entirely opaque to URSL code, including ``urcl`` functions.

As an optimization, if an instruction is meant to return a boolean value, it may have a branching variant after the main body. The syntax for this is like so:

```
inst eq 2 -> 1 {
    SETE $1 $1 $2
} branch :dest {
    BRE :dest $1 $2
}
```

The branch block is used when a custom instruction is used like ``eq branch :somewhere``. In this case, if there is a ``branch`` block, it will not use the main translation. These are translated together and only emits the contents of the branch block. In a branch block, return values are completely ignored, and it is called as if there are zero return values. The main special thing about branch blocks that makes them work is that they take an additional label parameter, which is the label in the ``branch`` instruction. Branch supporting instructions only return 1 value that should always be ``0`` or ``@MAX``, but the value is not enforced. However, the stack height *is* enforced.

Now, the only thing remaining to be covered about these is the calling convention. Custom instructions are inlined, and as such their parameters already exist in the registers. ``$0`` is always translated to the zero register, but ``$1`` is translated as the "excess height" plus one. The "excess height" is the number of additional stack entries (i.e. occupied registers), so ``$1`` refers to the first parameter to the function. If you have a function like ``urcl add 2 -> 1``, the two terms of the addition will be in ``$1`` and ``$2``. Reading registers higher than the amount of arguments may contain garbage data, as they are not cleared when they become unused. Since they are unused, you may write to them as temporary registers for whatever work you need to do within. The return values of a ``urcl`` function are also stored in the lower registers. Since ``add`` returns 1, the return value is stored in ``$1`` and ``$2`` is free to store whatever temporary garbage you need while working on the ``add`` operation. A reasonable implementation of the ``add`` function may be like so:

```
urcl add 2 -> 1 {
    ADD $1 $1 $2
}
```

# Instructions

Most instructions will pop 2 operands off the top of the stack, and push 1 result. This usually translates to a single URCL instruction, which makes most code very efficient.

Some instructions will also take an immediate operand, which is written directly after the instruction. Usually, this affects the resulting URCL more than a simple substitution. These are the most important instructions in URSL, even if some of them aren't that exciting, like the jumps.

Here is a list of instructions, and what they do. Their name is written with an example of an operand of that type. Some may have an explanation in parenthesis as a C-like expression where operands are represented as letters from the start of the alphabet. Signed/unsignedness of instructions are only stated in the descriptions, and not in the title explanation. There are also translations for most of these, where registers that depend on stack height are specified with the ``S`` prefix, written as if the stack height was exactly the amount of input operands. (if that is actually true, the ``S`` registers translate directly to actual registers without any arithmetic) An immediate operand is just ``operand`` in the translations. Their stack behaviour is also written in the ``args -> returns`` syntax that functions use (``args`` is omitted when zero)

## ``height 0``

After an instruction that always manages control flow (such as ``ret``, ``ret``, ``halt``) the stack height after it will be undefined. If instructions come after it, they must be jumped to, and since there is no stack height information already, ``height`` will specify the height that the code after should have. You can also use height directives to assert that the stack has a certain height at any given moment, and it may especially be useful in compiler outputs targeting URSL, because then the URSL compiler will refuse any code that does not match the expected height.

## ``const 0`` -> 1

This pushes a constant value (numeric/char literal like ``0``/``'\n'``, a macro value like ``@MAX``, a heap address like ``#0``, a data label like ``.label``, or a function pointer like ``$func``) onto the stack. In the future, i might add optimizations that ``const`` stack values are always translated to immediate values. This is the only instruction that accepts multiple operand types. And it's overloaded with a *lot* of types that you may wanna load.

```arm
IMM $1 operand
```

## ``stack 0`` -> 1

This will push the address of the given callstack local index to the operand stack. This is bounds checked against the locals for the given function. This value technically can live past its deallocation, and dereferencing it after that will lead to no good things, so i don't recommend doing so.

```
ADD $1 SP operand
```

## ``get name`` -> 1

This will push the variable bound by a [``let`` binding](#let-bindings) to the top of the stack.

```arm
MOV $1 {depends}
```

## ``set name`` 1 -> 0
This will write the top item on the stack to the variable bound by a [```let binding``](#let-bindings)

```arm
LSTR SP operand $1 
```

## ``call $name``

The stack behaviour of this instruction is that of the function being called. This is one of the most complex translations of all of URSL. The translation written here is python-ish pseudocode, because it isn't a simple substitution.

```py
# inclusive lower bound, exclusive upper bound (0..x means 0 <= i < x)
target: { args, returns }
keep = height - target.args
for i in 0..keep {
    PSH R{i + 1}
}
# remember, first arg is pushed last
for i in keep..height {
    MOV R{i - keep + 1} R{i + 1}
}
PSH ~+2
# this label will be mangled slightly in the output
JMP .{target}
# this is omitted if keep = 0
for i in reverse(0..returns) {
    MOV R{i + keep + 1} R{i + 1}
}
for i in reverse(0..keep) {
    POP R{i + 1}
}
```

## ``icall args -> returns`` args + 1 -> returns

Calls a function from a [function pointer](#function-pointers) on the stack, with the given signature. This allows for dynamic dispatch, like for virtual functions. The translation is pretty much the same as ```call`` if you pop one value first, and replace ``.target`` with that value.

## ``ret``

Returns from a function. The return values are passed directly as registers, and really the parts after ``JMP`` in the translation from ``call`` are more part of handling the return values, but of course do not fit in the translation for ``ret`` at all. Because the stack area is just deallocated in bulk, it leaves garbage which needs to be cleaned up by overwriting it with new data or zero-initializing local variables, as seen in the ``call`` translation.

```arm
ADD SP SP {args + locs}
RET
```

## ``halt``

Stops execution of the program, so the CPU or emulator needs to be manually reset. Code after this will require a ``height`` directive.

```arm
HLT
```

## ``in %port`` -> 1

Reads from a port.

```arm
IN $1 %operand
```

## ``out %port`` 1 -> 0

Writes to a port.

```arm
OUT %operand $1
```

## ``jump :dest``

This unconditionally transfers control to the label in the immediate operand. Unlike other instructions where the stack height must be at least the input height, here the stack height must be exactly that of the destination. After a jump, the stack height is undefined and requires a ``height`` directive.

```arm
JMP .operand // this label will be mangled in the output
```

## ``branch :dest`` 1 -> 0

This conditionally branches to the label in the immediate operand. Unlike other instructions where the stack height must be at least the input height, here the (output) stack height must be exactly that of the destination. You cannot branch on the output of a function, or from a constant value. You must use an instruction that has a branching variant, like ``eq`` or ``bool``. **In the core instructions above, there are no branching instructions**.

# Extra instructions

The following instructions are not actually part of the core of the language, but are automatically inserted by the compiler prior to actually parsing your functions. You can turn this off with the ``--minimal`` parameter.

## ``dup`` 1 -> 2

This will copy the value on top of the stack, such that it appears twice.

```arm
MOV $2 $1
```

## ``pop`` 1 -> 0

This will pop the top value off the stack, and discard it. In actual emitted output, this is a nop, since all it does is just decrease the (compile time) stack height and leave garbage in the next entry. However, this translation best captures the intent of what the instruction actually does.

```arm
MOV $0 $1
```

## ``load`` 1 -> 1

This will load the value at the given address from the operand stack, and push it onto the stack.

```arm
LOD $1 $1
```

## ``store`` 2 -> 0 (*A := B)

This will store the value at the top of the stack in the memory address below it.

```arm
STR $1 $2
```

## ``copy`` 2 -> 0 (*A := *B)

This will copy the value from the address at the top of the stack and store it in the address below it.

```arm
CPY $1 $2
```

If you expected ``LSTR`` and ``LLOD`` equivalents here, sorry, i just couldn't think of a name i loved for them other than ``list.load`` and ``list.store``, but after figuring out how to omit ``.`` from every other instruction, i really didn't like these two outliers. You can always just use ``add``; ``load`` and ``add``; (value); ``store``. I'll be sure to eventually implement optimizations that convert those to ``LLOD`` and ``LSTR``

## ``bool`` 1 -> 1 (A != 0)

This normalizes a boolean value on top of the stack. If it is zero (false), it will push zero. Otherwise (nonzero, true) it will push the ones complement of zero. (all ones) This allows bitwise operators to work as boolean logic (most notably ``not``, the rest are the same for LSB representation). Although URSL doesn't actually have any types on its stack, in the future the compiler may be able to optimize to more specific instructions by internally keeping track of whether or not any given element is a boolean value. (i.e. the output of ``bool`` or one of the comparison instructions, or a boolean-safe instruction such as the bitwise instructions) The results of this can also be interpreted as the twos complement of 0 and 1. If/when i do implement such optimizations, ``bool`` on a boolean value will be a nop, and will be omitted from output if determined to do such, therefore i recommend you put ``bool`` for any value that will be part of a boolean condition (also improves code clarity of your actual intent, if nothing else)

```arm
SETNE $1 $1 0
```

## ``not`` 1 -> 1 (~A)

```arm
NOT $1 $1
```

## ``xor`` 2 -> 1 (A ^ B)

Exclusive OR

```arm
XOR $1 $1 $2
```

## ``and`` 2 -> 1 (A & B)

```arm
AND $1 $1 $2
```

## ``or`` 2 -> 1 (A | B)

```arm
OR $1 $1 $2
```
## ``xnor`` 2 -> 1 (~(A ^ B))

Exclusive NOR

```arm
XNOR $1 $1 $2
```

## ``nand`` 2 -> 1 (~(A & B))

Not AND

```arm
NAND $1 $1 $2
```

## ``nor`` 2 -> 1 (~(A | B))

Not OR

```arm
NOR $1 $1 $2
```

## ``carry`` 2 -> 1 (A + B > @MAX)

Carry out of ``add`` operation. This takes two operands, adds them together, discards the result, but keeps the carry bit and extends it to every bit.

```arm
SETC $1 $1 $2
```

## ``add`` 2 -> 1 (A + B)

```arm
ADD $1 $1 $2
```

## ``sub`` 2 -> 1 (A - B)

```arm
SUB $1 $1 $2
```

## ``inc`` 1 -> 1 (A + 1)

When writing URCL#, i didn't like how ``INC``, ``DEC`` in URCL translated to ``ldc 1`` and ``add``/``sub`` instructions in CIL. That's why i'm including ``inc``/``dec`` in URSL. Oh, and something about having nearly the same instructions as URCL plus stack-specific ones or something.

```arm
INC $1 $1
```

## ``dec`` 1 -> 1 (A - 1)

```arm
DEC $1 $1
```

### ``neg`` 1 -> 1 (-A)

Two's complement. Equivalent to ``not`` followed by ``inc``.

```arm
NEG $1 $1
```

## ``rsh`` 2 -> 1 (A >> B)

(Logical) right shift. This shifts ``A`` to the right ``B`` bits, filling in zeroes in the most significant bits.

```arm
RSH $1 $1 $2
```

## ``ash`` 2 -> 1 (A >> B)

Arithmetic (right) shift. This shifts ``A`` to the right ``B`` bits, filling in the MSB of A in the most significant bits.

```arm
SRS $1 $1 $2
```

## ``lsh`` 2 -> 1 (A << B)

(Logical) left shift. There is no arithmetic left shift, because it would be equivalent to logical left shift.

```arm
LSH $1 $1 $2
```

## ``sgt`` 2 -> 1 (A > B)

Signed greater than.

```arm
SSETG $1 $1 $2
```

## ``sgte`` 2 -> 1 (A >= B)

Signed greater than or equals.

```arm
SSETGE $1 $1 $2
```

## ``slt`` 2 -> 1 (A < B)

Signed less than.

```arm
SSETL $1 $1 $2
```

## ``slte`` 2 -> 1 (A <= B)

Signed less than or equals.

```arm
SSETLE $1 $1 $2
```

## ``gt`` 2 -> 1 (A > B)

Unsigned greater than.

```arm
SETG $1 $1 $2
```

## ``gte`` 2 -> 1 (A >= B)

Unsigned greater than or equals.

```arm
SETGE $1 $1 $2
```

## ``lt`` 2 -> 1 (A < B)

Unsigned less than.

```arm
SETL $1 $1 $2
```

## ``lte`` 2 -> 1 (A <= B)

Unsigned less than or equals.

```arm
SETLE $1 $1 $2
```

## ``eq`` 2 -> 1 (A == B)

Equality.

```arm
SETE $1 $1 $2
```

## ``ne`` 2 -> 1 (A != B)

Inequality.

```arm
SETNE $1 $1 $2
```

## ``mult`` 2 -> 1 (A * B)

Multiplication.

```arm
MLT $1 $1 $2
```

## ``div`` 2 -> 1 (A / B)

Signed division.

```arm
SDIV $1 $1 $2
```

## ``undiv`` 2 -> 1 (A / B)

Unsigned division.

```arm
DIV $1 $1 $2
```

## ``mod`` 2 -> 1 (A mod B)

Signed modulo.

```arm
SMOD $1 $1 $2
```

## ``unmod`` 2 -> 1 (A mod B)

Unsigned modulo.

```arm
MOD $1 $1 $2
```