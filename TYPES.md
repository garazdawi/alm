# Types

There are four different kinds of terms: number, cons, nil and frame. If these number, cons and nil are created by the programmer. frame is used to symbolize a stackframe on the stack. To encode the terms on the stack and heap a NaN encoded floating point is used. The following tag scheme is used:


    aaaaaaaa aaaaaaaa aaaaaaaa aaaaaaaa aaaaaaaa aaaaaaaa tttt0000 0000000S

    arity: 48, tag: 4, double: 11, signed: 1

    if t != 0 value is a number, apply XOR bitmask
    if t == 0 and a = 0, value is signed inf number, change 0s to 1s

      1111   Float NaN            | Change 0s to 1s
      0000   Frame Ptr            |
      0001   Cons ptr             | 
      0010   NIL
      0011   ??
      0100   ??
      0101   ??
      0110   ??
      0111   ??
      1000   ??
      1001   ??
      1010   ??
      1100   ??
      1101   ??
      1110   ??
      1111   ??

Inspiration from [1], [2] and [3]. Sooo many primary tags, do not know what to do with them all!

   [1]: https://www.erlang-factory.com/upload/presentations/569/Halfword_Erlang_Factory_SF_2012.pdf
   [2]: http://evilpie.github.com/sayrer-fatval-backup/cache.aspx.htm
   [3]: http://wingolog.org/archives/2011/05/18/value-representation-in-javascript-implementations