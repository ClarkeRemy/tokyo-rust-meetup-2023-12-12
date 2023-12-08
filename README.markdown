# Presentation Notes

When you want to either make your code more explicit, or want to better understand the structure of the standard library, consider using :
```rust
#![no_implicit_prelude]
```

This will do 2 major things.
First is that `std::prelude::*` will it implicitly be imported into your project in all files (with `#![no_std]` it would be `core::prelude::*`).

Second is that Cargo will no longer implicitly add `extern crate` with each dependency in your `Cargo.toml` manifest 

There are 3 major crates that you can already use with the complier.
If you use them all it looks like this :
```rust
#![no_implicit_prelude]
extern crate core;  // fundamentals, lang items, intrinsics
extern crate alloc; // fundamental data structures that require a heap
extern crate std;   // re-exports `core` and `alloc`, 
                    //   but also extends them with OS specific things
```
One might be surprised by how much can be done with just `core` and `alloc`.

You then need to explicitly import anything you need. 
Give it a try! you'll get to see funny things like `alloc::alloc::alloc`.

# Labels
Labels were initially introduced for nested loops

Using the label we know explicitly where the value `break`s to :
```rust
let [mut i, mut j] = [5, 0];
let _loop_output = 'loop_label: loop {
  [i,j] = [i+j+1,j+1];
  if i > 20 {
    break 'loop_label j;
  }
  '_inner: loop {
    if i == 0 {
      continue 'loop_label;
    }
    i -= 1
  }
};
```

Labels could also be used for other loops,
I recommend naming the labels on what that are computing.
```rust
let mut i = 0..15;
'find_10: while let Some(val) = i.next() {
  if val == 10 {
    break 'find_10;
  }
}
'find_10: for each in 0..15 {
  if each == 10 {
    break 'find_10;
  }
}
```

Labels have also been added to blocks recently,
This is very powerful for "happy path programming".
```rust
let other = 5;
let ascii = b"martians";
let _out = 'check: {
  let [b'm', b'a', b'r', rest @ ..] = ascii else { break 'check None };
  let Some(thing) = rest.get(2) else { break 'check None };
  Some(*thing + other)
};
```

We can factor out the incrementally with `move` closures.
```rust
let check = move |ascii:&[u8]| 'check: {
  let [b'm', b'a', b'r', rest @ ..] = ascii else { break 'check None };
  let Some(thing) = rest.get(2) else { break 'check None };
  Some(*thing + other)
};
let _out = check(ascii);
```

We can change `break` to `return`
```rust
let check = move |ascii:&[u8]| {
  let [b'm', b'a', b'r', rest @ ..] = ascii else { return None };
  let Some(thing) = rest.get(2) else { return None };
  Some(*thing + other)
};
let _out = check(ascii);
```

it's now posible to turn into a function, and we learn that we need to pass in `other`
```rust
fn check_(ascii:&[u8], other: u8)-> Option<u8> {
  let [b'm', b'a', b'r', rest @ ..] = ascii else { return None };
  let Some(thing) = rest.get(2) else { return None };
  Some(*thing + other)
}
let _out = check_(ascii, other);
```
It's  now possible to extract the function.


Here is an example of a function that transforms a sexpr(prefix notation) into stack based bytecode (postfix). The arity is encodes as a number.

Note how the label `'dfs` says what process the loop is doing (depth first search).
```rust
#[derive(Debug, PartialEq)]
enum Atom { Plus, Mul, Num(i32) }

#[derive(PartialEq)]
enum Sexpr { Atom(Atom), List(Vec<Sexpr>) }

fn sexpr_to_postfix(sexpr: Sexpr) -> Result<Vec<Atom>, ()> {
  let mut postfix = Vec::new();
  let mut stack = Vec::new();
  stack.push(sexpr);
  'dfs: loop {
    match stack.pop() {
      None => break 'dfs Result::Ok(postfix),
      Some(val) => match val {
        Sexpr::Atom(atom) => postfix.push(atom),
        Sexpr::List(mut list) => {
          if let Some(Sexpr::Atom(Atom::Num(_)) | Sexpr::List(_)) = list.get(0) {
            break 'dfs Result::Err(());
          }

          let arity = (list.len() - 1) as i32;
          list.push(Sexpr::Atom(Atom::Num(arity)));
          let op = list.swap_remove(0);

          stack.push(op);
          stack.extend(list);
        }
      }
    }
  }
}
```


We expect this transformation :

(+ 2 4 (* 3 7) 4) => [4 7 3 2 * 4 2 4 +]

First some helper definitions
```rust
#[test]
#[cfg(test)]
fn use_sexpr_to_postfix() {
  // some helper definitions
  const PLUS: Sexpr = Sexpr::Atom(Atom::Plus);
  const MUL: Sexpr = Sexpr::Atom(Atom::Mul);
  const NUM: fn(i32) -> Sexpr = |n| Sexpr::Atom(Atom::Num(n));

  /// local declarative macro, talk about it later.
  #[cfg_attr(rustfmt, rustfmt_skip)]
  macro_rules! S {($($EXPR:expr)*) => { 
    Sexpr::List(alloc::vec![$($EXPR),*])  };
  }

  type A = Atom;

  /*
    the test code
  */
}
```

Now the tests
```rust
#[test]
#[cfg(test)]
fn use_sexpr_to_postfix() {
  /*
    the helper definitions
  */

  #[cfg_attr(rustfmt, rustfmt_skip)]
  let sexpr = 
    S!(PLUS NUM(2) 
            NUM(4) 
            S!(MUL NUM(3) 
                   NUM(7)) 
            NUM(4));

  #[cfg_attr(rustfmt, rustfmt_skip)]
  let expected = &[
    A::Num(4), 
      A::Num(7), 
      A::Num(3), 
      A::Num(2), A::Mul, 
    A::Num(4), 
    A::Num(2), 
    A::Num(4), A::Plus
  ];

  let Result::Ok(postfix) = sexpr_to_postfix(sexpr) else { core::panic!() };
  core::assert_eq!(&postfix[..], expected)
}
```


# Local Declarative Macros

Earlier we used a macro.
```rust
#[cfg_attr(rustfmt, rustfmt_skip)]
macro_rules! S {($($EXPR:expr)*) => { 
  Sexpr::List(alloc::vec![$($EXPR),*])  };
}
```
This macro exists to simplify the reading of the `Sexpr` test.
The intent is to make it as similar to a lisp S-expression.
```rust
#[cfg_attr(rustfmt, rustfmt_skip)]
let sexpr = 
  S!(PLUS NUM(2)         // (+ 2
          NUM(4)         //    4
          S!(MUL NUM(3)  //    (* 3
                 NUM(7)) //       7)
          NUM(4));       //    4)
```

The use of a macro raises questions. Why not just a function? Won't this complicate the API? Do I have to learn the DSL for this macro? Won't my coworkers complain?

Why not a function?
```rust
fn s<const LEN : usize>(list : [Sexpr; LEN]) -> Sexpr {
  let mut v = Vec::new();
  for each in list {
    v.push(each);
  }
  Sexpr::List(v)
}
/*
  ...
*/
#[cfg_attr(rustfmt, rustfmt_skip)]
let sexpr =
  s([PLUS, NUM(2),           // (+ 2
           NUM(4),           //    4
           s([MUL, NUM(3),   //    (* 3
                   NUM(7)]), //       7)
           NUM(4)]);         //    4)
```
Let's remember, we just wanted to have something for a test. To do so we had to define a type signature. If I used `&[Sexpr]` I would need to use `Clone`, forcing me to implement Clone. To avoid this I've used _`const` Generics_, increasing complexity.

- What about the API? 
  - Simple, it's used one locally. It's not exported, no dependent even has to know about it's existence.

- But it's a DSL! 
  - Yes, but it's also brain-dead simple, thread the arguments to `alloc::vec!`.

- Won't my coworkers complain? 
  - If they do, and can't be reasoned with, just `inline` it, and then see what they think.


## A more hairy example
Let's formulate a more hairy example.
We start by procuring an `async` executor using only the standard library.
```rust
pub(crate) fn blocks<T>(
  a: impl core::future::Future<Output = T>
) -> T {
  use core::task::*;
  fn make_raw_waker() -> RawWaker {
    RawWaker::new(
      core::ptr::null(), 
      &RawWakerVTable::new(
        |_| make_raw_waker(), 
        |_| (), 
        |_| (), 
        |_| (),
      ),
    )
  }
  let waker = unsafe { 
    core::task::Waker::from_raw(make_raw_waker()) 
  };
  let mut context = Context::from_waker(&waker);
  let mut pinned = core::pin::pin!(a);
  'execute: loop {
    match pinned.as_mut().poll(&mut context) {
      Poll::Ready(out) => break 'execute out,
      Poll::Pending => continue 'execute,
    }
  }
}
```
Our Slide-ware executor (don't think too hard about it ... ask me about it later).

let's now consider this type that gives limited access to it's inner value.

The important thing to note is the hairy type signature of `horror`, it only takes an exclusive reference to the inner value, meaning if you want to take arguments, you are going to need curry values into a closure. Because it returns a future, lifetimes could get real hairy.
```rust
use core::result::Result::{self, *};

struct Wrapped<T> {
  val: T,
}
impl<T> Wrapped<T> {
  pub fn inspect(self: &Self) -> &T {
    &self.val
  }
  pub async fn horror<F>(
    mut self: Self, 
    mut f: F
  ) -> Result<Self, ()>
  where
    F: core::ops::FnMut(&mut T) -> Result<(), ()>,
  {
    /*
      ...
    */
  }
}
```

The horror method `impl` for the curious uses a zero sized future.
```rust
  impl<T> Wrapped<T> {
    pub fn inspect(self: &Self) -> &T {
      &self.val
    }
    pub async fn horror<F>(mut self: Self, mut f: F) -> Result<Self, ()>
    where
      F: core::ops::FnMut(&mut T) -> Result<(), ()>,
    {
      /*
        `Stutter` future implementation  
      */


      Stutter.await;
      f(&mut self.val)?;
      Stutter.await;
      Ok(self)
    }
  }
```

What is `Stutter`? is a zero sized future! (lets not loiter though ...)
```rust
use core::{clone::Clone, sync::atomic, task::*};
/// spuriously yields
struct Stutter;
static WAIT: atomic::AtomicBool = atomic::AtomicBool::new(true);

impl core::future::Future for Stutter {
  type Output = ();
  fn poll(
    self: core::pin::Pin<&mut Self>, 
    cx: &mut Context<'_>,
  ) -> Poll<Self::Output> {

    let wait = WAIT.load(atomic::Ordering::Relaxed);
    WAIT.store(!wait, atomic::Ordering::Relaxed);

    cx.waker().clone().wake();

    if wait {
      Poll::Pending
    } else {
      Poll::Ready(())
    }
  }
}
```
Now that we have that set up, lets look at what we've been building to.

```rust
pub async fn use_mut() -> Result<(), ()> {
  const MAX_OPS: usize = 10;
  let mut num = Wrapped { val: 0_i32 };
  let operation_count = &mut 0_usize;

  let scope = 'operate: {
    #[cfg_attr(rustfmt, rustfmt_skip)]

    macro_rules! op {($($PUNCT:tt $NUM:literal)*) => {$(

      if *operation_count > MAX_OPS { 
        break 'operate *num.inspect() 
      }
      *operation_count+=1;
      num = num.horror({ 
        |x: &mut i32| { *x $PUNCT $NUM; Ok(()) }  
      }).await?;

    )*};}

    op! {+=2 -=1 *=7 +=1 /=2 +=4 +=1 +=5}
    *num.inspect()
  };

  std::println!("{}", scope);
  Ok(())
}
```

Lets break it down.

first we have the elements befor the macro in lexical scope
```rust
pub async fn use_mut() -> Result<(), ()> {
  const MAX_OPS: usize = 10;
  let mut num = Wrapped { val: 0_i32 };
  let operation_count = &mut 0_usize;

  let scope = 'operate: {
    /*
      note how op can refer to earlier symbols including :
      `operation_count`, `MAX_OPS`, `num`, and `'operate`
    
      we can shortcut localy using our `break 'operate` or
      return early on errors with `?`, and `.await` futures.
    */
  };

  std::println!("{}", scope);
  Ok(())
}
```

lets now look at the macro and it's use.

Try to imagine the function signature you would need, 
and how many arguments you would need to pass...
```rust
macro_rules! op {($($PUNCT:tt $NUM:literal)*) => {$(

  if *operation_count > MAX_OPS { 
    break 'operate *num.inspect() 
  }
  *operation_count+=1;
  num = num.horror({ 
    |x: &mut i32| { *x $PUNCT $NUM; Ok(()) }  
  }).await?;

)*};}

op! {+=2 -=1 *=7 +=1 /=2 +=4 +=1 +=5}
*num.inspect()
```
If it was a closure, imagine the lifetime issues you could have.
And you wouldn't have access to the labels in either case.

# First steps into unsafe code

Unsafe code has many forms, the the one that is most relavent has to do with memory allocation.

To explore this lets make the most basic data structure. A user defined box.

```rust
pub(crate) struct MyBox<T> {
  /// the field is private to ensure control of the allocation.
  ptr: *mut T,
}
```
This type has one job, own a value on the heap.
It holds a pointer to that value.

Let's consider the signatures and behavor of functions we want.
- ```rust 
  pub fn new(val: T) -> Self
  ```
  - allocates space and move `val : T` into the allocation
- ```rust
  pub fn unbox(self: Self) -> T
  ```
  - consume self and it's allocation, moving the inner `T` out

We will want to guarantee that when do not unbox it deallocates safely, for this we need an implementation of `core::ops::Drop`

We also want to make accessing the inner `T`, cheap. For that we will have `core::ops::Deref`, and `core::ops::DerefMut`.

We start by implementing `core::ops::Drop`.
The idea is that it should safely drop, even before it can even be made.
```rust
use alloc::alloc::Layout;
impl<T> MyBox<T> {
  /// Size of the struct in bytes and it's aligment
  const LAYOUT: Layout = Layout::new::<T>();
  /*
    ... other associated constants and methods
  */
}

// The core::ops::Drop::drop gets called first,
//   then each field of the struct will be dropped after.
impl<T> core::ops::Drop for MyBox<T> {
  // Note how we have exclusive access with &mut
  fn drop(self: &mut Self) {
    unsafe {
      self.ptr.drop_in_place();
      // SAFETY: The `self.ptr` does not have a 
      //         drop implementation, so we need to 
      //         drop what it points to it ourselves.
      alloc::alloc::dealloc(
        self.ptr as *mut u8, 
        Self::LAYOUT,
      );
    }
  }
}
```

We can now allocate with `new`. knowing it will get dropped.
```rust
pub fn new(val: T) -> Self {
  unsafe {
    let ptr = alloc::alloc::alloc(Self::LAYOUT) as *mut T;
    ptr.write(val);
    MyBox { ptr }
  }
}
```

We might feel confident about our code, but we should do some sanity checks. This can be done by running tests with the `miri` tool.

You can get details on installation here : `https://github.com/rust-lang/miri`

If you have the nightly `toolchain` with `rustup`, you need only run this.
```
rustup +nightly component add miri
```

With `miri` installed, we can now make tests with `miri`, to check for undefined  behavior.

We can first start with something we know will blow up. Let's dereference a null pointer!
```rust
  #[test]
  #[cfg(miri)]
  fn obviously_fails() {
    let null = core::ptr::null();
    let _x: i32 = unsafe { *null };
  }
```
And it fails!
```
test basic_box::obviously_fails ... error: Undefined Behavior: memory access failed: null pointer is a dangling pointer (it has no provenance)
```

We can now try with our functions.
```rust
  #[test]
  #[cfg(miri)]
  fn constructor_destructor() {
    let x = MyBox::<i32>::new(5); // constructor
    core::mem::drop(x) // destructor
  }
```
and ...
```
test basic_box::constructor_destructor ... ok
```
This passes!

But we should try the base case. A zero sized type.
```rust
  #[test]
  #[cfg(miri)]
  fn constructor_zst() {
    let x = MyBox::<()>::new(()); // constructor
    core::mem::drop(x) // destructor
  }
```
and ...
```
test basic_box::constructor_zst ... error: Undefined Behavior: creating allocation with size 0
```

We could make this work, but instead we are going to just define it out of existence.
```rust
use alloc::alloc::Layout;
impl<T> MyBox<T> {
  /// Size of the struct in bytes and it's aligment
  const LAYOUT: Layout = Layout::new::<T>();
  /// We don't support zero sized types
  const NOT_ZST: () = core::assert!(Self::LAYOUT.size() != 0);
  pub fn new(val: T) -> Self {
    // since all values must be created with 
    // `new` this constant will always be 
    // checked at compile time
    let _ = Self::NOT_ZST;
    unsafe {
      let ptr = alloc::alloc::alloc(Self::LAYOUT) as *mut T;
      ptr.write(val);
      MyBox { ptr }
    }
  }
  /*
    ... other associated methods
  */
}
```
We can now ignore the test, as it's unreachable
```rust
  #[test]
  #[cfg_attr(miri, ignore)]
  #[cfg(miri)]
  fn constructor_zst() { /* ... */ }
```

We can now work on `unbox`.

Since we deallocate the ptr, we don't want to drop `Self`, as it would "double free" by calling `alloc::alloc::dealloc` again. 
`core::mem::ManuallyDrop` stops the drop from happening
```rust
impl<T> MyBox<T> {
  const LAYOUT: alloc::alloc::Layout = /* ... */;
  const NOT_ZST: () = /* ... */;
  pub fn new(val: T) -> Self { /* ... */ }

  /// the standard library box is "magic" 
  /// when using `*` deref,
  /// we need this to pull out the value.
  pub fn unbox(self: Self) -> T {
    unsafe {
      let out = self.ptr.read();
      let this = core::mem::ManuallyDrop::new(self);
      // SAFETY: We have and owned `Self`, 
      //         the `self.ptr` is not shared.
      alloc::alloc::dealloc(
        this.ptr as *mut u8, 
        Self::LAYOUT,
      );

      out
    }
  }
}
```

We add another miri tests.
```rust
#[test]
#[cfg(miri)]
fn box_unbox() {
  let x = MyBox::<i32>::new(5); // box
  x.unbox(); // unbox
}
```
It passes.

Now we just need to do the same with `core::ops::Deref` and core::ops::DerefMut.
```rust
impl<T> core::ops::Deref for MyBox<T> {
  type Target = T;
  fn deref(self: &Self) -> &Self::Target {
    unsafe { &*self.ptr }
  }
}
impl<T> core::ops::DerefMut for MyBox<T> {
  fn deref_mut(self: &mut Self) -> &mut Self::Target {
    unsafe { &mut *self.ptr }
  }
}
```
And their tests.
```rust
#[test]
#[cfg(miri)]
fn deref() {
  let x = MyBox::<i32>::new(5);
  assert!(*x == 5);
}
#[test]
#[cfg(miri)]
fn deref_mut() {
  let mut x = MyBox::<i32>::new(5);
  *x = 25;
  assert!(*x == 25);
}
```

And we are done!

# Final thoughts

- Make use of code patterns that are effective locally.

- Consider the cost of encoding everything into the type system.
  - If one is not careful the cost can be very high.
  - The added complexity can be pernicious.

- Labels give access to powerful control flow.
  - consider the use of labels for fallible operations instead of making a fancy error type.

- Consider using macros as locally as possible.
  - lexical scope is powerful, it can reduces overall complexity. The deeper the macro, the more lexical scope you have access to.
  - It's okay if a macro is only ever called once.

- Unsafe code can be reasoned about.
  - most Rust programmers do not _need_ to write any. 
  - But all Rust developers _should_ experiment with it.
  - Use `miri`!
