

@;x64 cannot distinguish between booleans and numbers: it has only bits.  We,
@;therefore, must come up with a way to distinguish them.
@;
@;A common approach to distinguishing data types is to steal a few bits off the
@;end of every machine words to represent a type tag.  For example, we might
@;decide that the last three bits of any machine word represent the type, and
@;if they are @code{101} then it's a number but if they are @code{100} then
@;it's a boolean.
@;
@;Designing a good type tag representation is tricky.  The more bits you can
@;steal, the more data types you can represent directly as register sized
@;values, resulting in fewer memory accesses and more efficient code.  However,
@;the more bits you steal, the smaller the size of the data type you can fit in
@;a single machine word.  For large instances of that data type, such as a
@;number that requires more than a 61 bits to represent, you require a
@;different representation that uses memory and will reduce performance.
