## Words

Polymorphic:  https://www.wikiwand.com/en/Polymorphism

Construtor: Type Constructor  -> take type parameters to produce types

Nonexhaustive: not exhaustive
    exhaustive: someone or something that covers every possible detail.

## Notes

### Every function takes exactly one argument

What we call multi-argument functions are just functions taking one tuple argument, implemented with a tuple pattern in the function binding

### Equality types
eg:
    ''a list * '' a -> bool
These are 'equality types' that arise from using the = operator
```
    (* ''a * ''a -> string *)
    fun same_thing(x,y) = 
        if x = y then "yes" else "no"
    
    (* int -> string *)
    fun is_three x =
        if x = 3 then "yes" else "no"
```
## Wrong Answerd

```
datatype ('a, 'b ) flower =
    Node of ('a,'b) flower * ('a,'b)flower
   | Leaf of 'a
   | Petal of 'b
```

? What is the best description of the following SML datatype definetion ?
<details>
  <summary> Answer </summary>

  > A binary tree that can hold no data on its internal nodes, but each leaf can hold one of two different types of data
</details>

? What happens if the following line of code is entered into the SML/NJ REPL?

```
val NONE = SOME 2;
```

<details>
    <summary> Anser </summary>
    A runtime exception
    > The Bind exception will be raised because, as we know, val bindings pattern match on execution. NONE does not match SOME 2, but there is no other pattern to try. Thus, a nonexhaustive binding failure occurs.
</details>
