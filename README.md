# URSL?

-   Universal Reduced Stack Language
-   Usably Reduced Stack Language
-   Universal Redstone Stack Language
-   Usable Redstone Stack Language
-   ```js
    /U(niver)?sab?l[ey]? Red(uced|stone) Stack Language/
    ```

Note that in this document, sequences where multiple "instructions" are written one after each other inline, they are formatted like ``inst``; ``inst`` with a semicolon separator, even if the language is supposed to use newlines to separate them.

To use the compiler in this repo, first [install rust](https://rustup.rs/), and then run the following command:

```ps1
$ cargo install --git "https://github.com/Terrain2/URSL"
```

Then you can run ``ursl -i input.ursl -o output.urcl -as`` with the flags at the end there as you like. Do ``ursl --help`` for all the goodies that you can customize. Binaries are not distributed in this repo, but you're free to compile it and do whatever with the resulting binaries.

URSL is an abstraction which is somewhat higher than URCL. The 2 main problems it abstracts away is calling conventions, and register allocation. It is very similar to WASM text format and .NET CIL. URSL is a stack-oriented language with functions and label scopes within those functions. It is designed to be as easy as possible to compile to URCL, which is why for example memory instructions are literally 1:1 on URCL's available memory instructions. I plan on using this to compile languages such as .NET CIL and WASM to URCL. Stack machines allow for a simplified parser and binary representation of code, because instructions never take more than one immediate operand, and most only take from the operand stack. They are also somewhat easier to compile *to*, because it allows for very simple representation of nested expressions in reverse polish notation, and lowering of code can just translate to a set of stack instructions, without worrying about such things as temporary registers and using the correct available one, because URSL handles register allocation and ensures it just works. At least, it's supposed to, but this software is provided without warranty.

Just like WASM text and CIL, instructions are written in lowercase. This helps it look sorta like URCL, but obviously different just by the casing. Oh, and also, most instructions are written as actual english words, because i think it's a lot nicer to read, and URSL's primary purpose isn't to be written by a human, so it's not a huge concern for instructions to be short and faster to write. Some are still abbreviated if their name is actually long, but i'm not keeping it to 3 chars.

The stack pointer (SP) and program counter (PC) are entirely opaque to URSL code. Memory is not abstracted in any way for URSL, and it is possible to overwrite the callstack, which will cause issues, obviously. Numeric literals in URSL are always unsigned, because i don't wanna deal with signed number literals in the output. The compiler internally keeps track of everything as ``u64``, which is why you cannot use URSL with ``BITS >= 65`` unless you would like to modify the compiler itself to use some kind of bigint. If you want to use negative numbers, you should load them like ``const 0``; ``neg``

# Headers

All headers must be specified at the beginning of a program and have no defaults. They are the same as in URCL.

- ``bits 0`` corresponds to URCL's ``BITS == 0``. There are no ``>=`` or ``<=`` variants in URSL.
- ``minheap 0`` corresponds to URCL's ``MINHEAP 0``
- ``minstack 0`` corresponds to URCL's ``MINSTACK 0``
- The compiler automatically emits ``MINREG`` to be the exact number of registers it uses.

# Predefined data (DWs)

At the start of the file, there can be predefined data to keep in RAM. All such definitions must be labeled with a data label (``.name``), which is followed by a literal which is just the same as the ``DW`` operand in URCL. That is, char, number, label (which can be ``$func`` or ``.data_label``), strings (a somewhat common extension) or an array of any of these. You can also nest arrays. Data definitions will compile directly to a ``DW``. All definitions are outputted as ``DW``s in the same order, but i really don't recommend you try to do any arithmetic on the pointers to them, and there is no guarantee of what happens if you do so. An exception to this is obviously arrays, whose behaviour is well defined until the end of the array. If you know you can rely on cross-DW values (i.e. out of bounds array indices) on your target platform, you can safely rely on them in URSL too.

``RUN RAM``/``ROM`` is not distinguished in URSL. Depending on the behaviour of the target ISA, either one of these may be fit. URSL expects ``DW``s to be writable (i.e. it always allows writing to a data label, but obviously that won't happen unless your code actually writes to a data label), but URSL output will never try to read or write from an instruction label, or jump to any value that isn't an immediate label. URSL respects that instructions may be stored in addressable memory, and does not require ``#0`` to be a specific value it can figure out just by the data definitions. Any pointers that are outside the heap (less than ``#0``) are undefined behaviour in URSL, unless they're made from data labels, or from the ``ref`` instruction (given that stack frame still exists)

# Core concepts

At any given point in code, the operand stack height is known statically. That's because the operand stack is internally stored as registers, which are not dynamically indexable. What URCL refers to as "the stack" is used as a callstack in URSL, and that's how i will refer to it. "the stack" in URSL is ambiguous, but usually refers to the operand stack, which again, isn't stored as a stack, but in the registers. URSL does not have a concept of "registers", but it does have local variables.

The callstack is where arguments and locals are located. Just as with WASM, they are accessed using two unified instructions which i called ``get`` and ``set`` (as opposed to .NET's  ``ldarg``, ``ldloc``, ``starg``, ``stloc``). It takes one immediate value, which is the index of the local variable. ``get 0`` is the first argument, and for example if there are 2 arguments, ``get 2`` will be the first local variable.

Stack entries usually start at ``R1`` and as you load more, they will expand towards higher registers. The compiler is, however, free to use literally any register it wants to. This often happens to reduce the number of registers used.

As an example of how URSL may be translated, take this code:

```ursl
const 1
const 2
const 3
add
add
```

It will calculate the value of ``1 + (2 + 3)`` (the code is essentially reverse polish notation for this expression). When compiled to URCL, it will look something like this if it started at an empty stack:

```arm
// stack height is 0
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

Because of some optimizations with permutations and ``const``s and picking the shortest of several emit paths, code usually looks more like this:

```arm
ADD R1 2 3
ADD R1 1 R1
```

This is the shortest that example gets without the compiler needing to know what ``ADD`` means.

# Functions

Because of how functions work in regards to stack manipulation, they are treated well in URSL, and are not just neglected with label jumps and a return pointer.

Functions are declared using a syntax like ``func $name args -> returns + locals``, where ``args``, ``returns``, ``locals`` are all numeric literals. The stack behaviour of calling a function is determined by the ``args -> returns`` part. The ``+ locals`` part is optional, defaulting to zero. The stack behaviour part is also optional, defaulting to zero also. That's nice especially for the ``$main`` function, which minimally is declared only as ``func $main { ret }``. The ``$main`` function must take zero arguments and return zero values. It can have locals, and it is the entrypoint of a URSL program. Additionally, if a function returns zero values, it can "fall out" of its block with an empty stack. (i.e. the ``ret`` at the end is optional for zero-returning functions)

Inside a function, the operand stack starts at height 0, and ``ret`` must have stack height equal to the return count. This makes ``ret`` pretty much translate directly to a URCL ``RET`` instruction when no locals need to be deallocated, with all the heavy lifting being done at the callsite.

When calling a function like ``call $example``, its input arguments is however many off the top of the caller's stack it should receive. Take, for example, ``$example 2 -> 5 + 3`` (has 2 arguments, and 3 locals, and returns 5 values), and the caller has 4 items on their stack. It should translate into something like this URCL code:

```arm
// unload extra op stack entries to callstack
PSH R1
PSH R2
// arguments are passed in reverse because SP points to arg 0, locals are allocated within the function which allows function pointers without needing the locals to be part of the signature
// 2 arguments being passed
PSH R4
PSH R3
CAL .example // the label will be mangled
// bulk pop args from stack
ADD SP SP 2
// the function returns 5 values in R1..R5, but we have 2 more to be popped.
POP R7
POP R6
// continue with the next instructions
```

# Function pointers

You can use function pointers in URSL. Functions are "constant values", much like data labels, and can appear in the data section or inside a ``const`` instruction. This will load the value of its mangled label onto the stack, and erases the signature. You then *must* remember its signature externally, and you can call it with ``icall``. The ``icall`` instruction takes a function signature as an immediate argument, and it behaves exactly as ``call``, except it takes one more stack operand than the arguments. The arguments are at the top of the stack, and just below all of the arguments is the pointer to the function to call. Take for example, the stack height is 5, and you do ``icall 2 -> 3``. That will consume the top *3* stack operands, output 3 more and translate to the following URCL code:

```arm
// unload extra op stack entries to callstack
PSH R1
PSH R2
// arguments are passed in reverse because SP points to arg 0, locals are allocated within the function which allows function pointers without needing the locals to be part of the signature
// 2 arguments being passed
PSH R5
PSH R4
CAL R3 // the label will be mangled
// bulk pop args from stack
ADD SP SP 2
// the function returns 5 values in R1..R5, but we have 2 more to be popped.
POP R7
POP R6
// continue with the next instructions
```

It is important to put emphasis on the exact place a function pointer exists in. Take this example:

```
  // arguments
  call $example
```

The equivalent indirect call is **NOT** this:

```diff
  // arguments
- const $example
  icall 0 -> 0
```

It is this:

```diff
+ const $example
  // arguments
  icall 0 -> 0
```

# Labels and jumps

In URSL, instruction labels are marked with ``:`` prefix, and data labels with a ``.`` prefix. This allows easier differentiation, as the two kinds of labels are used quite differently, and in URSL they are not even interchangable.

The stack height is enforced across jumps. Jumps to a label must either have the correct stack height from the instruction immediately preceding the label, or the label must be preceded by a ``height`` directive, which is always preceded by an unconditional control flow instruction (that is, ``ret``, ``halt``, ``jump``, which never execute the instruction after). The ``height`` directive informs the compiler of the expected stack height in the next instruction, in cases where the compiler does not know.

# Name mangling

There are three different kinds of "labels" in URSL that look the exact same in URCL. ``$func``, ``:inst_label`` and ``.data_label`` must all compile to a single URCL label of the form ``.label``.

Naturally, since they look completely different in code, the same name should be usable with a different sigil and get a unique label? You can. And for instruction labels, they are even unique to each individual function! This is where the name mangling comes into play.

The name mangling isn't like, entirely stable for sure 100%, but i'm fairly happy with it as it stands right now, and pretty sure it's not gonna change, so i will document it here:

The current name mangling relies on a balanced set of underscores. (i.e. there is always an even number of underscores in the label name)

The name mangling uses several "fields" and "values" for those fields. Every label starts with ``URSL``, and then comes the fields. The prefix is useful for ffi, because it prevents name collisions with arbitrary other labels.

Every field becomes an undescore, followed by the field name (guaranteed to be unique by choosing the field names carefully in the compiler, not escaped in any way, currently only ``data``, ``func``, ``label``) and then another underscore. Then comes the value without any delimiters.

For example, the ``$main`` function becomes ``.URSL_func_main``. Labels can also contain underscores. For readability of the output, underscores are simply escaped as double undescores. A function like ``$hello_world`` becomes ``.URSL_func_hello__world``.

Any special characters in the name also get escaped using an underscore, a readable alphanumeric name, and another underscore. Currently that's only ``.`` which is escaped as ``_dot_``.

For local labels, there are two fields, the "func" and the "label". They can nicely be represented as ``$func:label``, and an example like ``$fibonacci:base_case`` will be mangled as ``.URSL_func_fibonacci_label_base__case``.

Most languages don't allow dots in their identifiers, so dots are useful for your own name mangling. Just use dots as the delimiter. For example, ``$example.module.func`` becomes ``.URSL_func_example_dot_module_dot_func``.

# Extern functions

URSL supports a declaration without a body like so:

```
extern "URCL++" func $example 2 -> 3;
```

This will use the name mangling scheme of the given calling convention. You can optionally specify the label it corresponds to as well:

```
extern "URCL++" func $example 2 -> 3 = .example;
```

Some calling conventions depend on additional stuff like parameters and may require you to specify a label. For example, hexagn depends on parameter types for the label mangling. URSL does not support any additional parameters to extern declarations, so labels for ``extern "Hexagn"`` are mandatory. I also don't think their name mangling is entirely stable, so it's just easier in general for me to pass the problem of mangling onto my users.

These calling conventions are supported as of now:

- URSL (native to URSL, currently equivalent to URCL++ but may change in the future. Consider it opaque)
- URCL++ (pass args in reverse order, ``CAL``, pop args, will be in ``$1..$n``)
- Hexagn (pass args in reverse order, ``CAL``, pop args, will be in ``$2``)'

(in all three calling conventions, the caller also saves the currently used regs, and restores them after the call. i've omitted that for the sake of brevity)

**Declaring functions of a different calling convention than the default is not supported.** Only calling them is supported. You can also call them with ``extern "convention" icall 0 -> 0`` with the stack behaviour specified inline.

When you're using extern functions, the compiler will assume the relevant labels exist and can be called. Obviously this is not true, you will need to manually do something to merge the output from two compilers if you wish to actually do interop (i.e. concatenating the URCL files.). When doing this, ``--no-main`` can be useful.

# Forward declaration

You can use a forward declaration to declare a function without a body. This is useful for hooks in libraries, like maybe allowing the application to set a custom allocator when depending on a standard library.

```
func $example 5 -> 5;
```

This ensures that ``$example`` will be declared later during compilation with the same signature. That function may be either implemented in URSL or defined as ``extern`` with some other calling convention. If it is never implemented, an error will be emitted.

# Custom instructions

Custom instructions are declared with the ``inst`` keyword, and they act like custom URSL instructions, meaning they don't have the ``$`` prefix and are inlined into the caller. They are not called with the ``call`` instruction, but just by naming them as any other instruction.

Custom instructions are not implemented as URSL, but with a syntax more similar to URCL. This functionality exists so that you can use new URCL instructions without needing me to add them to URSL, and for cases where you need lower level access to URCL. Also, most of the default instructions are defined in terms of custom instructions which allows URSL to have a simpler compiler that doesn't explicitly define how to emit boring instructions that don't have any semantic meaning relevant to URSL.

Custom instructions are not emitted exactly as the original code (some transformations do modify it, which i'll explain), but all URCL instructions in their bodies are mapped one-to-one in outputted code in the same order.

Custom instructions have the instruction/data label abstraction that URSL has, meaning that jumps look like ``JMP :label``, not ``JMP .label`` (as you may expect from URCL). Every instruction in a URCL function can be labeled, and they are converted into relatives for the output, since i didn't wanna deal with unique stateful mangled labels (i.e. different every time they are emitted). An instruction can have any number of labels. Data labels can only appear as source operands, and instruction labels can only appear as destination operands.

In custom instructions, there are 3 operand kinds: instruction labels, immediate values, and registers. Immediate values are the same as the arguments to the ``const`` instruction in URSL. Memory locations are immediate values, and unlike in URCL, they cannot start with ``M``. They must be prefixed with ``#``. Same with registers, those are always ``$``. This allows me to make a simpler parser, because if ``R`` was allowed, then the instructions cannot be arbitary (i.e. they are parsed with a regex like ``\w+``), since it would cause an ambiguity whether an instruction has 3 operands or if it has 2 operands and a new instruction starts. Unlike URCL, in URSL all whitespace is equivalent, including newlines and comments, and i did not want to make the grammar newline sensitive just because you want ``R``/``M`` prefixes.

Registers can also be prefixed with ``&``, in which case they will be *named registers*. I recommend using named registers, because it's more readable most of the time. Other than the different naming, they behave identically to indexed registers and just look slightly differently. The only exception is ``$0``, which of course, always maps to the actual zero register.

Almost all URCL instructions take the form of ``OPCODE Destination Source`` or ``OPCODE Destination Source1 Source2``. ``Destination`` is either a register, or an instruction label. This allows arbitrary branch instructions that may be added to URCL in the future. By design, the URCL opcode really doesn't give the compiler any info except for the 3 special cases, so therefore the destination is allowed to be a label so instructions like ``BRZ :some_other_place $0`` would work. This is also legal for non-branching instructions, because URSL does not deal with the semantics of individual URCL instructions. The source operands are either registers, or immediate values. Immediate values can include data labels.

There are three specially handled instructions:

- ``IN $0 %PORT`` The source operand can be a port, and when the source for ``IN`` is a port, the destination must be a register.
- ``OUT %PORT $0`` The destination operand can be a port and if so, the instruction must have exactly one source operand.
- ``JMP :label`` The destination can be a label, and this instruction can have zero source operands. This is the only legal instruction that has <2 operands.

These rules are carefully chosen to intentionally leave out this functionality:

- ``PSH $0`` & ``POP $0`` The stack in URSL is exclusively used as a callstack. Custom instructions may not break this invariant.
- ``CAL $function`` You may expect to be able to call functions from custom instructions (including URSL functions) but i did not want to bother implementing a way to do this safely. Where would you get your parameters and return values?
- ``CAL :function`` I do not want to encourage you to write complex algorithms in URSL via the URCL sub-syntax. You can use jumps, but not ``CAL``s.
- ``RET`` You might expect this to work like the URSL ``ret``, but that is very different from URCL's ``RET``. I did not want to allow any instructions that do not map one-to-one, and the URCL definition of ``RET`` violates the same rule as ``PSH`` and ``POP`` would. Instead, you should jump to ``:$``.
- ``NOP`` This instruction is never useful in URSL translations, and the only case i could think of that you'd need it is to jump to the end of a translation. In that case, you should jump to ``:$``, which will actually correspond to the instruction immediately after this translation, and not some NOP within it.
- ``HLT`` URSL has built-in support for the ``halt`` instruction, which translates to ``HLT``. In custom instructions, i did not think it is sufficiently useful to be able to halt within. And i did not want to deal with any kind of analysis within custom instructions.
- ``SP`` and ``PC`` These are entirely opaque to URSL code, including custom instructions.

The syntax of defining such an instruction is as so:

```ursl
inst example $1 $2 -> $3 $4 {
    // body
}
```

In that example, it takes two operands that are stored in ``$1`` and ``$2`` at the start. It then returns the values that are stored in ``$3`` and ``$4`` at the end.

Note that in the custom instruction syntax, the ``->`` is not written when you have zero return values.

You may overwrite those registers, and if you rename them to something like ``&a &b -> &c &d`` this will behave identically, just with different names.

However, if you do NOT plan to overwrite a register, you may use a little trick for optimization. You can put a register in angle brackets to tell the compiler that you never plan to write to this register.

```ursl
inst store <$1> <$2> {
    STR $1 $2
}
```

This enables optimizations such as implicit sharing. If this instruction was called immediately after ``dup``, then the compiler would know that ``$1`` and ``$2`` are the same register, and it would not need to spend another register. This is why you cannot write to them ever, because it may also overwrite other registers. Worse yet, if any "shared" register operand is an immediate value, it will not ever be loaded into a register. It will just be written in place. You obviously can't write to an immediate value, so you should really consider the shared registers as immediates.

The compiler cannot enforce that you do not write to these, because instructions like ``STR`` and ``CPY`` allow immediates in the "destination" operand, so i can't enforce them to be illegal in this spot.

But what if there are several different ways to express your custom instruction using different configurations of operands? You can define all of them after each other, and the compiler will emit whichever one is the shortest. Crucially, this relies on the fact that they have no observable difference in behaviour. Here is an example for the ``add`` instruction:

```
inst add &a <&b> -> &a {
    ADD &a &a &b
}

inst add <&a> &b -> &b {
    ADD &b &a &b
}

inst add <&a> <&b> -> &out {
    ADD &out &a &b
}
```

For instructions that are meant to return boolean values, and can be used as conditionals for the ``branch`` instruction, they must have a ``branch`` body.

```
branch example <$1> <$2> -> :dest {
    BRE :dest $1 $2
}
```

In this case, the "output" part is not optional, and must contain exactly one value which is a label, not a register. Simply use this label as a branch destination. ``branch`` is really a modifier for the previous instruction to emit its branch variant instead, and if the instruction does not have a ``branch`` body, it will not compile. This is so that relatively meaningless combinations like ``const 1 const 2 add branch :label`` will not compile.

You may only have one branch block per instruction. Use shared operands wherever possible.

# Custom permutations

"Custom permutations" are custom instructions that are declared using a permutation only, instead of URCL code. This allows you to give names to commonly used permutations (like ``nop``, ``dup``, ``pop``, etc)

Their syntax is just ``inst name [a b c] -> [a b c]`` where ``[a b c]`` is just any number of identifiers that are delimited by square brackets, and ``name`` is the instruction name. See the instrinsic instruction ``perm`` for more information. That fucky header name doesn't work well in links, so if you expected to click that name, nope. scroll down like less than a page, you lazy goose.

If you frequently use the same permutation, then a custom permutation declaration is usually more readable, but other than that it should behave identically to the anonymous permutation

# Intrinsic instructions

## ``height 0``

After an instruction that always diverges control flow (such as ``ret``, ``ret``, ``halt``) the stack height after it will be undefined. If instructions come after it, they must be jumped to, and since there is no stack height information already, ``height`` will specify the height that the code after should have. You can also use height directives to assert that the stack has a certain height at any given moment, and it may especially be useful in compiler outputs targeting URSL, because then the URSL compiler will refuse any code that does not match the expected height. This is the only instruction that is legal when the stack height is undefined

---

## ``label :example``

This instruction defines a label that can be jumped to. Internally, due to optimizations with shuffling registers, it may be necessary to "normalize" registers before the label. This is a process that always consists of only ``MOV``s, and it is always free after a ``height`` directive, after a ``branch`` instruction, or immediately after another ``label``.

---

## ``perm [a b c] -> [c a b]``

This instruction names the top elements of the operand stack in the left grouping (rightmost item is the top element), and in the right grouping it uses the same names to define the order these elements will be pushed back to the stack. ``perm`` is short for permute, or permutation, and the example given in the syntax above will rotate the top 3 elements, such that the previously-topmost element is the third from the top. By convention, the left side is usually named alphabetically, but they can be any identifier.

This is always free, because it is a compile-time operation.

---

## ``const 0`` 0 -> 1

This pushes a constant value (numeric/char literal like ``0``/``'\n'``, a macro value like ``@MAX``, a heap address like ``#0``, a data label like ``.label``, or a function pointer like ``$func``) onto the stack. This is equivalent to ``IMM``, but usually it is completely free because it will actually translate to an immediate operand in the URCL output if possible.

---

## ``ref 0`` 0 -> 1

This will load the address of the given argument or local at the zero-indexed position given by the immediate operand. You can read/write to it with ``load``/``store``. Once the current function returns, this pointer will be garbage, and it's undefined behaviour to write to it afterwards. Reading from it will just be garbage.

---

## ``get 0`` 0 -> 1

This will read the argument or local at the zero-indexed position given by the immediate operand. Arguments come first in the index, then locals.

---

## ``set 0`` 1 -> 0

This will write to the argument or local at the zero-indexed position given by the immediate operand. Arguments come first in the index, then locals.

---

## ``call $name``

The stack behaviour of this instruction is that of the function being called. Depending on the calling convention, it will be emitted completely differently from the others. See [extern functions](#extern-functions) for more information.

---

## ``icall args -> returns``

Calls a function from a function pointer on the stack, with the given signature. This allows for dynamic dispatch. This emits with URSL's native calling convention, and it compatible with functions defined in URSL.

---

## ``extern "convention" icall args -> returns``

This is the same as ``icall``, but it allows you to call a function from a different calling convention. See [extern functions](#extern-functions) for more information.

---

## ``ret``

Returns from a function. The return values are passed directly as registers, and really the parts after ``JMP`` in the translation from ``call`` are more part of handling the return values, but of course do not fit in the translation for ``ret`` at all. Because the stack area is just deallocated in bulk, it leaves garbage which needs to be cleaned up by overwriting it with new data or zero-initializing local variables, as seen in the ``call`` translation.

---

## ``halt``

Stops execution of the program, so the CPU or emulator needs to be manually reset. Code after this will require a ``height`` directive. This is equivalent to ``HLT``.

---

## ``in %port`` 0 -> 1

Reads from a port. This is equivalent to ``IN``.

---

## ``out %port`` 1 -> 0

Writes to a port. This is equivalent to ``OUT``.

---

## ``jump :dest``

This unconditionally transfers control to the label in the immediate operand. Unlike other instructions where the stack height must be at least the input height, here the stack height must be exactly that of the destination. After a jump, the stack height is undefined and requires a ``height`` directive. The compiler may need to normalize the registers before the jump, which is a process that always consists of only ``MOV``s.

---

## ``branch :dest`` 1 -> 0

This conditionally branches to the label in the immediate operand. Unlike other instructions where the stack height must be at least the input height, here the (output) stack height must be exactly that of the destination. You cannot branch on the output of a function, or from a constant value. You must use an instruction that has a branching variant, like ``eq`` or ``bool``. **In the core instructions above, there are no branching instructions**.

---

## Prelude instructions

The following instructions are not actually part of the core of the language, but are imported from [the prelude](src/prelude.ursl). You can turn this off with the ``--no-prelude`` parameter to the compiler.

---

## ``nop`` 0 -> 0

Does nothing. Equivalent to ``perm [] -> []``

---

## ``pop`` 1 -> 0

This will pop the top value off the stack, and discard it. Equivalent to ``perm [a] -> []``

---

## ``dup`` 1 -> 2

This will copy the value on top of the stack, such that it appears twice. Equivalent to ``perm [a] -> [a a]``

---

## ``swap`` 2 -> 2

Swaps the top 2 values on the stack. Equivalent to ``perm [a b] -> [b a]``

---

## ``over`` 2 -> 3

Copies the value *just below* the top of the stack and puts it over the top. Equivalent to ``perm [a b] -> [a b a]``

---

## ``load`` 1 -> 1

This will load the value at the given address from the operand stack, and push it onto the stack. Equivalent to ``LOD``.

---

## ``store`` 2 -> 0 (*A := B)

This will store the value at the top of the stack in the memory address below it. Equivalent to ``STR``.

---

## ``copy`` 2 -> 0 (*A := *B)

This will copy the value from the address at the top of the stack and store it in the address below it. Equivalent to ``CPY``.

---

If you expected ``LSTR`` and ``LLOD`` equivalents here, sorry, i just couldn't think of a name i loved for them. You can always just use ``add``; ``load`` and ``add``; (value); ``store``. I'll be sure to eventually implement optimizations that convert those to ``LLOD`` and ``LSTR``

---

## ``bool`` 1 -> 1 (A != 0)

This normalizes a boolean value on top of the stack. If it is zero (false), it will push zero. Otherwise (nonzero, true) it will push the one's complement of zero (@MAX) This allows bitwise operators to work as boolean logic (most notably ``not``, the rest are the same for the LSB representation). The results of this can also be interpreted as the two's complements of 0 and 1. Equivalent to ``SETNE`` or ``JNZ``.

---

## ``not`` 1 -> 1 (~A)

Equivalent to ``NOT``. Also works with branches and is equivalent to ``NOT`` then ``JNZ``. (not equivalent to ``JZ``!)

---

## ``xor`` 2 -> 1 (A ^ B)

Equivalent to ``XOR``.

---

## ``and`` 2 -> 1 (A & B)

Equivalent to ``AND``.

---

## ``or`` 2 -> 1 (A | B)

Equivalent to ``OR``.

---

## ``xnor`` 2 -> 1 (~(A ^ B))

Equivalent to ``XNOR``.

---

## ``nand`` 2 -> 1 (~(A & B))

Equivalent to ``NAND``.

---

## ``nor`` 2 -> 1 (~(A | B))

Equivalent to ``NOR``.

---

## ``carry`` 2 -> 1 (A + B > @MAX)

Carry out of ``add`` operation. This takes two operands, adds them together, discards the result, but keeps the carry bit and extends it to every bit (i.e. returns 0 or @MAX). Equivalent to ``SETC``.

---

## ``add`` 2 -> 1 (A + B)

Equivalent to ``ADD``.

---

## ``sub`` 2 -> 1 (A - B)

Equivalent to ``SUB``.

---

## ``inc`` 1 -> 1 (A + 1)

Equivalent to ``INC``.

---

## ``dec`` 1 -> 1 (A - 1)

Equivalent to ``DEC``.

---

### ``neg`` 1 -> 1 (-A)

Two's complement. Equivalent to ``NEG``.

---

## ``rsh`` 1 -> 1 (A >> 1)

(Logical) right shift. This shifts ``A`` to the right by one, placing 0 in the MSB. Equivalent to ``RSH``.

---

## ``ash`` 1 -> 1 (A >> 1)

Arithmetic (right) shift. This shifts ``A`` to the right by one, keeping the MSB unchanged. Equivalent to ``SRS``.

---

## ``lsh`` 1 -> 1 (A << 1)

(Logical) left shift. This shifts ``A`` to the left by one, placing 0 in the LSB. Equivalent to ``LSH``.

---

## ``brsh`` 2 -> 1 (A >> B)

Barrel (logical) right shift. This shifts ``A`` to the right by ``B``, placing 0 in the most significant bits. Equivalent to ``BSR``.

---

## ``bash`` 2 -> 1 (A << B)

Barrel arithmetic (right) shift. This shifts ``A`` to the right by ``B``, copying the MSB into the new slots. Equivalent to ``BSS``.

---

## ``blsh`` 2 -> 1 (A << B)

Barrel (logical) left shift. This shifts ``A`` to the left by ``B``, placing 0 in the least significant bits. Equivalent to ``BSL``.

---

## ``sgt`` 2 -> 1 (A > B)

Signed greater than. Equivalent to ``SSETG`` and ``SBRG``.

---

## ``sgte`` 2 -> 1 (A >= B)

Signed greater than or equals. Equivalent to ``SSETGE`` and ``SBGE``.

---

## ``slt`` 2 -> 1 (A < B)

Signed less than. Equivalent to ``SSETL`` and ``SBRL``.

---

## ``slte`` 2 -> 1 (A <= B)

Signed less than or equals. Equivalent to ``SSETLE`` and ``SBLE``.

---

## ``gt`` 2 -> 1 (A > B)

Unsigned greater than. Equivalent to ``SETG`` and ``BRG``.

---

## ``gte`` 2 -> 1 (A >= B)

Unsigned greater than or equals. Equivalent to ``SETGE`` and ``BGE``.

---

## ``lt`` 2 -> 1 (A < B)

Unsigned less than. Equivalent to ``SETL`` and ``BRL``.

---

## ``lte`` 2 -> 1 (A <= B)

Unsigned less than or equals. Equivalent to ``SETLE`` and ``BLE``.

---

## ``eq`` 2 -> 1 (A == B)

Equality. Equivalent to ``SETE`` and ``BRE``.

---

## ``ne`` 2 -> 1 (A != B)

Inequality. Equivalent to ``SETNE`` and ``BNE``.

---

## ``mult`` 2 -> 1 (A * B)

Multiplication. Equivalent to ``MLT``.

---

## ``sdiv`` 2 -> 1 (A / B)

Signed division. Equivalent to ``SDIV``.

---

## ``div`` 2 -> 1 (A / B)

Unsigned division. Equivalent to ``DIV``.

---

## ``smod`` 2 -> 1 (A mod B)

Signed modulo. Equivalent to ``SMOD``.

---

## ``mod`` 2 -> 1 (A mod B)

Unsigned modulo. Equivalent to ``MOD``.
