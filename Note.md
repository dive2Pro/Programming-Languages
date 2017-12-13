## Words

Polymorphic:  https://www.wikiwand.com/en/Polymorphism

Construtor: Type Constructor  -> take type parameters to produce types


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
