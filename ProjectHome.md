# Introduction #

The idea is simple: instead of writing actual programs, we write programs which write programs. But rather than trying to invent one general purpose meta-language, what we do is design a new programming language for each application. This allows us to abstract from all the tedious detail such as having to decide which particular programming language to use.

It also allows us to solve some hard problems such as the one Ken Thompson identifies in his Turing Award speech [On Trusting Trust.](http://www.ece.cmu.edu/~ganger/712.fall02/papers/p761-thompson.pdf) This is because a meta-programmed system does not actually have _any_ concrete representation in source code, so is not susceptible to any attack that is based on recognising the source code of the target system.

It also enables potentially massive code re-use. For example an operating system could share kernel code with drivers and applications. The actual object code that is needed for a given process is then all pre-loaded and all that is required is a very fast linking step which configures the existing code for the particular context from which it is called.

This sort of thing is impossible when code is written in an _ad hoc_ way, because even identically specified subroutines can be implemented in completely different ways, and identifying common code is therefore impossible. But if all the actual source code is generated automatically then this identification and re-use is almost trivial, amounting to little more than type checking.

See [Proofs and Representations](http://metaprogramming.googlecode.com/files/proofreps2.pdf) for more background than you probably want. But not more than there is because this a _very_ old project. If you don't believe me then see [John 1:1](http://en.wikipedia.org/wiki/John_1:1).

# The System #

For details see [Genesis](http://metaprogramming.googlecode.com/files/genesis.pdf) which is the project manifesto.

The theory is all in that strange book [Proofs and Types](http://www.paultaylor.eu/stable/prot.pdf). We can use system F modules as pluggable semantics. To get an idea of how this works, see the implementation of substitution in [Substitute.sml](http://code.google.com/p/metaprogramming/source/browse/Substitute.sml). It is essentially free of any concrete representation of the underlying objects which are the subject of the substitution. One can imagine implementing [Hindley-Milner type inference](http://prooftoys.org/ian-grant/hm/) similarly, and other common transforms such as CPS and the monad transforms described in Wadler's [The Essence of Functional Programming.](http://homepages.inf.ed.ac.uk/wadler/papers/essence/essence.ps) So defining a language is then simply a case of writing a grammar, and plugging in appropriate semantics by identifying what non-terminals in the grammar are supposed to mean in terms of being binding constructs, variable references, etc. So algorithms like unifcation and capture avoiding substitution need only be written and debugged once.

The system as it exists now is a few thousand lines of Standard ML code. This code implements a parser generator and pretty printer for arbitrary context-free grammars. We use a parser generator based on Ridge combinator parsers as described in http://www.tom-ridge.com/parsing.html There is a problem with this implementation which is that it takes exponential time to parse some left-recursive productions. I think this is easily fixed however, and a first stab at this is in the variants [Parser2.sml](http://code.google.com/p/metaprogramming/source/browse/Parser2.sml) and [SemanticParserModule2.sml](http://code.google.com/p/metaprogramming/source/browse/SemanticParserModule2.sml) which are as yet untested. Essentially the idea is to use the context to count down the number of available recursion steps, but still to allow the parsers access to the full token stream so that attempts to parse the input are cached and available for outer calls to re-use.

# What Next? #

We rewrite it, in itself. So we write a grammar with semantics described _using an arbitrary language,_ which grammar and semantics describe the parsing and printing of such grammars. What could be simpler? Then this can be re-targeted so that it works for C++, perl, python, javascript and all those lovely languages people love to write programs in. It'll even work for nasty languages like Standard ML and Scheme! So we recommend frequent forking of the project. The more thoroughly forked it is, the better it will be. All that needs to be agreed is the basic principle which is that we describe knowledge by inventing appropriate languages. The exact details of the syntax don't matter, as long it is _known by someone,_ and has been mechanically verified, because then it can always be automatically transformed.

Confused? There is a blog called [Live Logic](http://livelogic.blogspot.com/) where this can be discussed. Questions will be answered, problems solved. Not necessarily by _me,_ you understand, because this is a _collaboration,_ which is really just another word for a _conspiracy._ And it's not only the living, because lots of dead people are in on it. In fact, more dead people seem to know what's going on than living ones, but hopefully that will change soon!

# Red October #
Red October is a patch to the old Moscow ML 2.01 source. The patch adds some of the GNU libffi and lightning functions providing a very basic level of JIT compilation and run-time access to dynamic libraries, a Meta.exec system call for metaprogramming Standard ML, and it has a C parser which can analyse C function and data declarations in header files. Nothing is at a practically useful level yet, but it is a start.

The patch is described at length in [Red October](http://goo.gl/8jqq3w).

# Licencing #

I don't understand the notion of licencing knowledge. It is utterly incomprehensible to me, The reason is that knowledge of reading and knowledge of writing are actually the same thing: because if you know how to read, then you know how to write. So why should someone need permission to _write_ something?

All I can say is that this project is  _really_ open source: the source code does not _actually_ exist. Now it is true that some _representations_ of it ([UTF8.sig](http://code.google.com/p/metaprogramming/source/browse/UTF8.sig) and [UTF8.sml](http://code.google.com/p/metaprogramming/source/browse/UTF8.sml)) were taken directly from the HOL distribution, and there they are `licenced.' But if you downloaded that from here, or from the HOL distribution, then what is the difference?

Here is the HOL licence pertaining to those two files, I can't tell you what it means though. I _can_ tell you that it refers to rather neat, well-written code, which works, and that it is was a pleasure to discover it.

HOL COPYRIGHT NOTICE, LICENSE AND DISCLAIMER.

Copyright 1998, 1999 by Konrad Slind.
Copyright 2000--2009 by Michael Norrish and Konrad Slind.

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

  * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

  * The names of the copyright holders and contributors may not be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
DAMAGE.

---

Really, that it was `worth' someone's time to write  (or to read) _that_
reflects a _very_ low value of human life. And I mean no disrespect to the people named above.