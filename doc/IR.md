# Intermediate Representation

Arwen generates Intermediate Representation (IR) from bound AST nodes. It then
either generates assembly or potentially QBE or LLVM intermediate code from
this IR.

Arwen IR is, like many other forms of this concept, an abstracted form of
assembly language, i.e. a list of operations to be executed sequentially.

## IR Types and pseudo types

| Type    | |
|---------|-|
| bool    | 1-bit logical true/false value |
| u8      | 8-bit (1 byte) unsigned value               |
| i8      | 8-bit (1 byte) signed value                 |
| u16     | 16-bit (2 byte) unsigned value              |
| i16     | 16-bit (2 byte) signed value                |
| u32     | 32-bit (4 byte) unsigned value              |
| i32     | 32-bit (4 byte) signed value                |
| u64     | 64-bit (8 byte) unsigned value              |
| i64     | 64-bit (8 byte) signed value                |
| Integer | One of u8, i8, u16, i16, u32, i32, u64, i64 |
| f32     | Single-precision float. (C `float`)         |
| f64     | Double-precision float. (C `double`)        |
| Float   | One of f32 or f64                           |
| Number  | Integer or float                            |
| Slice (`[]type`) | Typed slice                      |
| String (`[]u32`)    | Slice of UTF-32 characters      |
| Dynarr (`[*]type`) | Typed dynamic array            |
| Builder (`[*]u32`)   | String builder, dynamic array of UTF-32 characters |
| Array (`[n]type`) | Typed fixed-length array                          |
| Zero-terminated Array (`[0]type`) | Typed zero-terminated array.                      |
| CStr (`[0]u32`)       | Zero terminated string of UTF-32 characters ("CString") |
| CStr8 (`[0]u8`)       | Zero terminated string of unsigned characters  |

## IR operations

### AssignFromRef

Assign a variable from a memory reference.

#### Payload

The type of the variable.

#### Pops

| Entry | Type |
|-|-|
| Target variable | Variable Reference |
| Reference to the value to assign | Value Reference |

#### Pushes

| Entry | Type |
|-|-|
| Target variable | Variable Reference |

### AssignValue

Assign a value to a variable.

#### Payload

The type of the variable.

#### Pops

| Entry | Type |
|-|-|
| Value to assign | Value |
| Target variable | Variable Reference |

#### Pushes

| Entry | Type |
|-|-|
| Target variable | Variable Reference |

### BinaryOperator

Executes a binary operation.

#### Payload

The operator to execute.

#### Pops

| Entry | Type |
|-|-|
| Right-hand side operand | Value |
| Left-hand side operand | Value |

#### Pushes

| Entry | Type |
|-|-|
| Result | Value |

#### Notes

The following operations are supported. In the table below, `any' indicates
any type for which the type is implemented. It can be assumed that this includes
at least the Number, String, and Builder types.

|Operation|Left-hand type(s)|Right-hand type(s)|Result type|
|---------|-----------------|------------------|-----------|
|Add (+)       |Number           |LHS               |LHS        |
|              |String           |String            |Builder    |
|              |Builder|String|Builder (the LHS builder)|
|              |Builder|Builder|Builder (the LHS builder)|
|Binary And (&)|Integer|LHS|LHS|
|Binary Or (|)|Integer|LHS|LHS|
|Binary Xor (^)|Integer|LHS|LHS
|Cast|Any|Type|RHS (the type represented by RHS)|
|Divide ('/')|Number|LHS|LHS|
|Equals ('==')|Any|LHS|bool|
|Greater ('>')|Any|LHS|bool|
|Greater or equal ('>=')|Any|LHS|bool|
|Less ('<')|Any|LHS|Bool|
|Less or equal ('<=')|Any|LHS|bool|
|Logical And ('&&')|bool|bool|bool|
|Logical Or ('\|\|')|bool|bool|bool|
|Member Access ('.')|Struct|Field name|Type of the field in the struct|
|Modulo ('%', 'mod')|Number|LHS|LHS|
|Multiply ('\*')|Number|LHS|LHS|
|               |String|Integer|Builder|
|               |Builder|Integer|Builder (the LHS builder)|
|Not Equal ('!=')|Any|LHS|bool|
|Shift Left ('<<')|Integer|LHS|LHS|
|Shift Right ('<<')|Integer|LHS|LHS|
|Subscript|Array,DynArr,Slice|Integer|Type of the elements of the array
|Subtract ('-')|Number|LHS|LHS|

This operation should obviously be called `BinaryOperation`. This will happen
at some point.

### Break

Break out of a single or a number of nested blocks. This involves executing
the deferred operations encountered while executing the blocks.

#### Payload

```C++
struct BreakOp {
  uint64_t scope_end;   // Label of the statement to jump to to unwind the
                        // current block. This can be the first defer, or the
                        // actual end of the block if no defers have to be
                        // executed
  uint64_t depth;       // Number of blocks to break from
  uint64_t label;       // Label to eventually jump to, after all blocks and
                        // their associated defers are unwound. This label
                        // can be the exit point of a function, the first
                        // statement after a loop, or the first statement after
                        // a block
}
```

#### Pops

This operation does not use the stack

#### Pushes

This operation does not use the stack

#### Notes





    (Call, CallOp| |
    (DeclVar, IRVariableDeclaration| |
    (Dereference, pType| |
    (Discard, pType| |
    (Jump, uint64_t| |
    (JumpF, uint64_t| |
    (JumpT, uint64_t| |
    (Label, uint64_t| |
    (NativeCall, CallOp| |
    (Pop, pType| |
    (PushConstant, Value| |
    (PushValue, VarPath| |
    (PushVarAddress, VarPath| |
    (ScopeBegin, std::vector<IRVariableDeclaration>| |
    (ScopeEnd, ScopeEndOp| |
    (UnaryOperator, UnaryOp)


