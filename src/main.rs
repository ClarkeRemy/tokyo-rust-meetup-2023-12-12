#![no_implicit_prelude]
extern crate core;

fn main() -> core::result::Result<(), ()> {
  bad_async::blocks(local_macros::use_mut())
}

pub(crate) mod labels {
  extern crate alloc;
  extern crate core;
  use core::{
    iter::{Extend, Iterator},
    option::Option::{self, *},
    result::Result,
  };

  use alloc::vec::Vec;
  fn all_labels() {
    let [mut i, mut j] = [5, 0];
    let _loop_output = 'loop_label: loop {
      [i, j] = [i + j + 1, j + 1];
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

    let other = 5;
    let ascii = b"martians";
    let _out = 'check: {
      let [b'm', b'a', b'r', rest @ ..] = ascii else { break 'check None };
      let Some(thing) = rest.get(2) else { break 'check None };
      Some(*thing + other)
    };

    let check = move |ascii: &[u8]| 'check: {
      let [b'm', b'a', b'r', rest @ ..] = ascii else { break 'check None };
      let Some(thing) = rest.get(2) else { break 'check None };
      Some(*thing + other)
    };
    let _out = check(ascii);

    let check = move |ascii: &[u8]| {
      let [b'm', b'a', b'r', rest @ ..] = ascii else { return None };
      let Some(thing) = rest.get(2) else { return None };
      Some(*thing + other)
    };
    let _out = check(ascii);

    // it's now posible to turn into a function, and we learn that we need to pass in `other`
    fn check_(ascii: &[u8], other: u8) -> Option<u8> {
      let [b'm', b'a', b'r', rest @ ..] = ascii else { return None };
      let Some(thing) = rest.get(2) else { return None };
      Some(*thing + other)
    }
    let _out = check_(ascii, other);
    // it's  now possible to extract the function.
  }

  #[allow(unused)]
  #[derive(Debug, PartialEq)]
  enum Atom {
    Plus,
    Mul,
    Num(i32),
  }

  #[allow(unused)]
  #[derive(PartialEq)]
  enum Sexpr {
    Atom(Atom),
    List(Vec<Sexpr>),
  }

  #[allow(unused)]
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
        },
      }
    }
  }

  #[test]
  #[cfg(test)]
  fn use_sexpr_to_postfix() {
    const PLUS: Sexpr = Sexpr::Atom(Atom::Plus);
    const MUL: Sexpr = Sexpr::Atom(Atom::Mul);
    const NUM: fn(i32) -> Sexpr = |n| Sexpr::Atom(Atom::Num(n));
    #[cfg_attr(rustfmt, rustfmt_skip)]
    macro_rules! S {($($EXPR:expr)*) => { Sexpr::List(alloc::vec![$($EXPR),*])  };}
    type A = Atom;

    // (+ 2 4 (* 3 7) 4) => [4 7 3 2 * 4 2 4 +]
    let sexpr = S!(PLUS NUM(2) NUM(4) S!(MUL NUM(3) NUM(7)) NUM(4));
    let expected = &[A::Num(4), A::Num(7), A::Num(3), A::Num(2), A::Mul, A::Num(4), A::Num(2), A::Num(4), A::Plus];

    let Result::Ok(postfix) = sexpr_to_postfix(sexpr) else { core::panic!() };
    core::assert_eq!(&postfix[..], expected)
  }
}

pub(crate) mod bad_async {
  pub(crate) extern crate core;

  pub(crate) fn blocks<T>(a: impl core::future::Future<Output = T>) -> T {
    use core::task::*;
    fn make_raw_waker() -> RawWaker {
      RawWaker::new(core::ptr::null(), &RawWakerVTable::new(|_| make_raw_waker(), |_| (), |_| (), |_| ()))
    }
    let waker = unsafe { core::task::Waker::from_raw(make_raw_waker()) };
    let mut context = Context::from_waker(&waker);
    let mut pinned = core::pin::pin!(a);
    'execute: loop {
      match pinned.as_mut().poll(&mut context) {
        Poll::Ready(out) => break 'execute out,
        Poll::Pending => continue 'execute,
      }
    }
  }
}

pub(crate) mod local_macros {
  extern crate core;
  extern crate std;
  use core::result::Result::{self, *};

  struct Wrapped<T> {
    val: T,
  }
  impl<T> Wrapped<T> {
    pub fn inspect(self: &Self) -> &T {
      &self.val
    }
    pub async fn horror<F>(mut self: Self, mut f: F) -> Result<Self, ()>
    where
      F: core::ops::FnMut(&mut T) -> Result<(), ()>,
    {
      use core::{clone::Clone, sync::atomic, task::*};

      /// spuriously yields
      struct Stutter;
      static WAIT: atomic::AtomicBool = atomic::AtomicBool::new(true);

      impl core::future::Future for Stutter {
        type Output = ();
        fn poll(self: core::pin::Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
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

      Stutter.await;
      f(&mut self.val)?;
      Stutter.await;
      Ok(self)
    }
  }

  pub async fn use_mut() -> Result<(), ()> {
    const MAX_OPS: usize = 10;
    let mut num = Wrapped { val: 0_i32 };
    let operation_count = &mut 0_usize;

    let scope = 'operate: {
      //without this attribute, the indentation would be intense
      #[cfg_attr(rustfmt, rustfmt_skip)]
      macro_rules! op {($($PUNCT:tt $NUM:literal)*) => {$(

        if *operation_count > MAX_OPS { break 'operate *num.inspect() }
        *operation_count+=1;
        num = num.horror({  |x: &mut i32| { *x $PUNCT $NUM; Ok(()) }  }).await?;

      )*};}

      op! {+=2 -=1 *=7 +=1 /=2 +=4 +=1 +=5}
      *num.inspect()
    };

    std::println!("{}", scope);
    Ok(())
  }
}

pub(crate) mod basic_box {
  extern crate alloc;
  extern crate core;

  pub(crate) struct MyBox<T> {
    /// the field is private to ensure control of the allocation.
    ptr: *mut T,
  }

  impl<T> MyBox<T> {
    const LAYOUT: alloc::alloc::Layout = alloc::alloc::Layout::new::<T>();
    /// We don't support zero sized types
    const NOT_ZST: () = core::assert!(Self::LAYOUT.size() != 0);
    /// This is the only function that will allocate memory.
    pub fn new(val: T) -> Self {
      let _ = Self::NOT_ZST;
      unsafe {
        let ptr = alloc::alloc::alloc(Self::LAYOUT) as *mut T;
        ptr.write(val);
        MyBox { ptr }
      }
    }
    /// the standard library box is "magic" when using `*` deref,
    ///   we need this to pull out the value.
    pub fn unbox(self: Self) -> T {
      unsafe {
        let out = self.ptr.read();
        // SAFETY: We have and owned `Self`, the `self.ptr` is not shared.
        let this = core::mem::ManuallyDrop::new(self);
        alloc::alloc::dealloc(this.ptr as *mut u8, Self::LAYOUT);
        
        out
      }
    }
  }

  impl<T> core::ops::Drop for MyBox<T> {
    fn drop(self: &mut Self) {
      unsafe {
        self.ptr.drop_in_place();
        // SAFETY: The `self.ptr` does not have a drop implementation,
        //         so we need to drop what it points to it ourselves.
        alloc::alloc::dealloc(self.ptr as *mut u8, Self::LAYOUT);
      }
    }
  }

  // The [`MyBox`] is a "smart pointer", so we add smart pointer traits `Deref` and `DerefMut`
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
  // some other trait implementations would be straightforward to add.

  #[test]
  #[cfg_attr(miri, ignore)]
  #[cfg(miri)]
  fn obviously_fails() {
    let null = core::ptr::null();
    let _x: i32 = unsafe { *null };
  }
  #[test]
  // #[cfg_attr(miri, ignore)]
  #[cfg(miri)]
  fn constructor_zst() {
    let x = MyBox::<()>::new(()); // constructor
    core::mem::drop(x) // destructor
  }
  #[test]
  #[cfg(miri)]
  fn constructor_destructor() {
    let x = MyBox::<i32>::new(5); // constructor
    core::mem::drop(x) // destructor
  }
  #[test]
  #[cfg(miri)]
  fn box_unbox() {
    let x = MyBox::<i32>::new(5); // box
    x.unbox(); // unbox
  }
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
}

