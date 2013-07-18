# Prototc: a type checker for Proto

This is a proof of concept of a type checker for the [MIT Proto][proto] programming language.

[proto]: http://proto.bbn.com/

## HOW TO INSTALL

#### Install OCaml
Install Ocaml from the official download page: [http://ocaml.org/install.html][ocaml-install].

[ocaml-install]: http://ocaml.org/install.html

#### Get Prototc
Clone the project
	
	git clone https://github.com/hilbert-/prototc.git

or download an archive from [here][download-master].

[download-master]: https://github.com/hilbert-/prototc/archive/master.zip

#### Compile Prototc
Move to the prototc folder and compile it with

	make install

## HOW TO USE

	./bin/prototc -t type-string -e expr-string
	-t : the type that has to be checked for the given expression
	-e : the expression that has to be type-checked

## WHAT'S NEXT

### BUG FIXES
To report a BUG use the project's issue tracker: [https://github.com/hilbert-/prototc/issues][issue-traker]

[issue-traker]: https://github.com/hilbert-/prototc/issues

### IMPROVEMENTS

#### Performance
+ Every time the type-checker asks for an operator's type (calling the *ProtoOperators.get_type* function) it has to parse the string representation of the type. The overhead of the parsing operation can be reduced storing the inner representation of the type of the already parsed operators types.
+ Optimization of the build system.

#### Language functionalities coverage   
+ Handle operators with an unbounded number of arguments (like *+*, *\**, etc.) instead of requiring the application to 2 arguments at time. 
+ Handle *S* (scalar) instead of forcing those fields to be *N* (natural).
+ Implement the support to parametric field types, like *F(S)* (Field of Scalar).
+ Support the neo-compiler's in which constant values (like *inf*, etc..) are not functions.
