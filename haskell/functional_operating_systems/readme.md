# Functional Operating Systems
 > Its considered good programming practise to focus on compositionality: build software out of small well defined modules that combine to give rise to other modules with different behaviors. __This is simply too difficult to do in distributed systems. Why? - Marius Eriksen, Twitter

This is because of __complexity:__
- Applications are __deeply intertwined__ with system APIs, and so lack portibility,
- Modern operating systems offer __dynamic support__ for __many users__ to run __multiple applications__ simultaneously,

We build applications in a __safe and compositional style__ using functional programming and then surround it in __15 million lines of unsafe code__ to interact with the outside world. With such a powerful programming language, why hasn't the operating system disapeared from our stack?

## Video
- [Functional Operating System Design by Anil Madhavapeddy](https://www.youtube.com/watch?v=UEIHfXLMtwA)
- [Towards Functional Operating Systems by Anil Madhavapeddy](https://www.youtube.com/watch?v=DJuORegesyw)

## Papers
- [A Principled Approach to Operating System Construction in Haskell - Hallgren, Jones, Leslie & Tolmach](http://ogi.altocumulus.org/~hallgren/ICFP2005/house.pdf)