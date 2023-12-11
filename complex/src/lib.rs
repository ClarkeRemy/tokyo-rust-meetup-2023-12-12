//! the original of mine gist can be found here ( https://gist.github.com/ClarkeRemy/b86fac831a72d3808713ba92c8784fcb )

use std::{ops::*,cmp::PartialEq};
#[derive(Debug,Clone,Copy,PartialEq)] pub struct Complex(f64,f64);

type C=Complex;

impl Add for C{ type Output = C; fn add(self, rhs: Self) -> Self::Output { Complex(self.0+rhs.0,self.1+rhs.1)}}
impl Sub for C{ type Output = C; fn sub(self, rhs: Self) -> Self::Output { Complex(self.0-rhs.0,self.1-rhs.1)}}
impl Neg for C{ type Output = C; fn neg(self           ) -> Self::Output {	Complex(-self.0,-self.1)}}
impl Mul for C{ type Output = C; fn mul(self, rhs: Self) -> Self::Output { Complex((self.0*rhs.0)-(self.1*rhs.1), (self.0*rhs.1)+(self.1*rhs.0))}}
impl Div for C{ type Output = C; fn div(self, rhs: Self) -> Self::Output {
  let div = rhs.0.powi(2)+rhs.1.powi(2);  Complex((self.0*rhs.0 + self.1*rhs.1)/div, (self.1*rhs.0 - self.0*rhs.1)/div)}}
impl AddAssign for C{fn add_assign(&mut self, rhs: Self) {*self = self.add(rhs)}}
impl SubAssign for C{fn sub_assign(&mut self, rhs: Self) {*self = self.sub(rhs)}}
impl MulAssign for C{fn mul_assign(&mut self, rhs: Self) {*self = self.mul(rhs)}}
impl DivAssign for C{fn div_assign(&mut self, rhs: Self) {*self = self.div(rhs)}}


// Complex Overloads

macro_rules! c_ol_partial_eq {( $TYPE:ty	) => {
  impl PartialEq<$TYPE> for C     {
    fn eq(&self, rhs: &$TYPE ) -> bool {match self {Complex(r,i) if *r == *rhs  as f64 && (*i ==0.0) =>true,_=>false}}
    fn ne(&self, rhs: &$TYPE ) -> bool {!(self==rhs)}}
  impl PartialEq<C>     for $TYPE {
    fn eq(&self, rhs: &C     ) -> bool {match rhs  {Complex(r,i) if *r == *self as f64 && (*i ==0.0) =>true,_=>false}}
    fn ne(&self, rhs: &C     ) -> bool {!(self==rhs)}}
};}

macro_rules! c_ol_trait {( $TRAIT:ident $METHOD:ident => $TYPE:ty	) => {
  impl $TRAIT<$TYPE> for C     {type Output = C; fn $METHOD(self, rhs: $TYPE ) -> Self::Output {Complex(self.0.$METHOD(rhs as f64),self.1)}}
  impl $TRAIT<C>     for $TYPE {type Output = C; fn $METHOD(self, rhs: C     ) -> Self::Output {Complex((self as f64).$METHOD(rhs.0),rhs.1)}}
};}
macro_rules! c_ol_assign {($TRAIT:ident $METHOD:ident => $TYPE:ty) => {
  impl $TRAIT<$TYPE> for C {fn $METHOD(&mut self, rhs: $TYPE) {self.0.$METHOD(rhs as f64)}}  };}
macro_rules! c_ol_all_traits  {($($TRAIT:ident $METHOD:ident;)* =>$TYPE:ty) => {$( c_ol_trait!{ $TRAIT $METHOD=>$TYPE} )*};}
macro_rules! c_ol_all_assigns {($($TRAIT:ident $METHOD:ident;)* =>$TYPE:ty) => {$( c_ol_assign!{$TRAIT $METHOD=>$TYPE} )*};}


macro_rules! complex_overload_all {($($TYPE:ty)*) => { $(
  c_ol_all_traits! {Add add; Sub sub; Mul mul; Div div; => $TYPE}
  c_ol_all_assigns!{AddAssign add_assign; SubAssign sub_assign; MulAssign mul_assign; DivAssign div_assign; => $TYPE}
  c_ol_partial_eq! {$TYPE}
)*

};}

complex_overload_all!{isize i8 i16 i32 i64 i128 usize u8 u16 u32 u64 u128 f32 f64}