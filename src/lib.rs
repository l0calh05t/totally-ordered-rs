//! This crate adds the `TotallyOrderable` trait for `f32` and `f64` values as well as the ABI-transparent `TotallyOrdered` type which adds `Ord + Eq + Hash` to wrapped floating point values.
//! Main use case: sorting of floating-point arrays which may or may not contain not-a-numbers, infinities, and positive or negative zeros.
//!
//! ```
//! use totally_ordered::TotallyOrdered;
//! let mut values : [f64; 4] = [-0.0, 0.0, -1.0, 1.0];
//! TotallyOrdered::new_slice_mut(&mut values).sort();
//! # assert_eq!(values[0], -1.0);
//! # assert_eq!(values[1].to_bits(), (-0.0f64).to_bits());
//! # assert_eq!(values[2].to_bits(), (0.0f64).to_bits());
//! # assert_eq!(values[3], 1.0);
//! ```

#![cfg_attr(not(feature = "std"), no_std)]

use core::cmp::Ordering;
use core::hash::{Hash, Hasher};
use core::slice::{from_raw_parts, from_raw_parts_mut};

/// Implement this for types that are not directly `Ord + Eq`, but
/// can be at a slightly higher runtime cost. Implemented for `f32`
/// and `f64`.
pub trait TotallyOrderable {
	/// A true equality comparison. Can be more expensive than standard
	/// `PartialEq`.
	fn total_eq(&self, other: &Self) -> bool;
	/// A totally ordered comparison. Can be more expensive than standard
	/// `PartialOrd`.
	fn total_cmp(&self, other: &Self) -> Ordering;
	/// A hashing function that matches `total_eq`. As the wrapped type
	/// doesn't implement `Eq`, it can't be `Hash` directly.
	fn total_hash<H: Hasher>(&self, state: &mut H);

	type TotalOrdKey: Ord + Hash + Sized;
	fn total_ord_key(&self) -> Self::TotalOrdKey;
}

/// Implement this for types that are not directly `Ord + Eq`, but
/// can be transformed *in place* in such a way that the associated
/// type `Transformed` is. Implemented for `f32` and `f64`.
pub trait InPlaceTotallyOrderable: TotallyOrderable + Sized {
	/// Associated type that is `Ord`, must be the same size as
	/// `Self`
	type Transformed: Ord + Hash + Sized;
	/// Transform `Self` into `Transformed` in place.
	/// # Safety
	/// The resulting value must no longer be treated as a
	/// `Self`, but as a `Transformed`, until the inverse
	/// transform `total_order_inverse_transform` is called.
	/// `Transformed` shall only be used for comparisons,
	/// hashing and swapping / moves.
	unsafe fn total_order_transform(&mut self);
	/// Apply the inverse transformation of
	/// `total_order_transform`
	/// # Safety
	/// This function may only be called on values previously
	/// transformed via `total_order_transform`.
	unsafe fn total_order_inverse_transform(&mut self);
}

#[cfg(feature = "std")]
/// (Potentially) faster alternative to `TotallyOrdered::new_slice_mut` followed by
/// `sort` for values that can be transformed in place.
///
/// ```
/// # use totally_ordered::*;
/// let mut values = vec![3.0, 2.0, 1.0];
/// total_sort(&mut values);
/// # assert_eq!(values[0], 1.0);
/// # assert_eq!(values[1], 2.0);
/// # assert_eq!(values[2], 3.0);
/// ```
pub fn total_sort<E: InPlaceTotallyOrderable, T: AsMut<[E]>>(container: &mut T) {
	fn total_sort_impl<E: InPlaceTotallyOrderable>(s: &mut [E]) {
		for e in s.iter_mut() {
			unsafe {
				e.total_order_transform();
			}
		}
		{
			let st = unsafe { from_raw_parts_mut(s.as_ptr() as *mut E::Transformed, s.len()) };
			// as the transformed values are totally orderable, unstable == stable!
			st.sort_unstable();
		}
		for e in s.iter_mut() {
			unsafe {
				e.total_order_inverse_transform();
			}
		}
	}
	total_sort_impl(container.as_mut());
}

macro_rules! implement_float_order {
	($F:ident, $U:ident, $I:ident, $N:literal) => {
		/// Implements the IEEE 754-2008 binary32/binary64 total ordering predicate.
		impl TotallyOrderable for $F {
			fn total_eq(&self, other: &Self) -> bool {
				self.to_bits() == other.to_bits()
			}

			fn total_cmp(&self, other: &Self) -> Ordering {
				let mut a = self.to_bits() as $I;
				let mut b = other.to_bits() as $I;
				a ^= (((a >> ($N - 1)) as $U) >> 1) as $I;
				b ^= (((b >> ($N - 1)) as $U) >> 1) as $I;
				a.cmp(&b)
			}

			fn total_hash<H: Hasher>(&self, state: &mut H) {
				self.to_bits().hash(state);
			}

			type TotalOrdKey = $I;
			fn total_ord_key(&self) -> Self::TotalOrdKey {
				let a = self.to_bits() as $I;
				a ^ (((a >> ($N - 1)) as $U) >> 1) as $I
			}
		}

		impl InPlaceTotallyOrderable for $F {
			type Transformed = $I;

			unsafe fn total_order_transform(&mut self) {
				let mut bits = self.to_bits();
				bits ^= ((((bits as $I) >> ($N - 1)) as $U) >> 1);
				*self = $F::from_bits(bits);
			}

			unsafe fn total_order_inverse_transform(&mut self) {
				// forward and inverse transforms are identical for f32/f64!
				self.total_order_transform();
			}
		}
	};
}

implement_float_order!(f32, u32, i32, 32);
implement_float_order!(f64, u64, i64, 64);

/// An ABI-transparent newtype wrapper around types that
/// adds Ord + Eq to TotallyOrderable types (f32 and f64).
#[derive(Copy, Clone, Debug, Default)]
#[repr(transparent)]
pub struct TotallyOrdered<F: TotallyOrderable>(pub F);

impl<F: TotallyOrderable> TotallyOrdered<F> {
	/// Creates a wrapped value from an inner value
	pub fn new(v: F) -> Self {
		Self(v)
	}

	/// Creates a wrapped slice without copying data.
	/// Not implemented as `From<&[F]> for &[Self]` as
	/// slices are always foreign.
	///
	/// Follows the usual borrowing rules:
	/// ```compile_fail
	/// # use totally_ordered::*;
	/// let mut values = [1.0, 2.0, 3.0];
	/// let values_to = TotallyOrdered::new_slice(&values);
	/// values[2] = 4.0; // error, can't mutate while borrowed
	/// assert_eq!(values_to[2], TotallyOrdered::new(3.0));
	/// ```
	pub fn new_slice(s: &[F]) -> &[Self] {
		// TotallyOrdered is repr(transparent)
		unsafe { from_raw_parts(s.as_ptr() as *const _, s.len()) }
	}

	/// Creates a mutable wrapped slice without copying data.
	/// Not implemented as `From<&mut [F]> for &mut [Self]` as
	/// slices are always foreign.
	///
	/// Follows the usual borrowing rules:
	/// ```compile_fail
	/// # use totally_ordered::*;
	/// let mut values = [3.0, 2.0, 1.0];
	/// let values_to = TotallyOrdered::new_slice_mut(&mut values);
	/// assert_eq!(values[2], 1.0); // error, can't borrow while mutably borrowed
	/// values_to.sort();
	/// ```
	pub fn new_slice_mut(s: &mut [F]) -> &mut [Self] {
		// TotallyOrdered is repr(transparent)
		unsafe { from_raw_parts_mut(s.as_ptr() as *mut _, s.len()) }
	}
}

impl<F: TotallyOrderable> PartialEq for TotallyOrdered<F> {
	fn eq(&self, other: &Self) -> bool {
		self.0.total_eq(&other.0)
	}
}

impl<F: TotallyOrderable> Eq for TotallyOrdered<F> {}

impl<F: TotallyOrderable> PartialOrd for TotallyOrdered<F> {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.0.total_cmp(&other.0))
	}
}

impl<F: TotallyOrderable> Ord for TotallyOrdered<F> {
	fn cmp(&self, other: &Self) -> Ordering {
		self.0.total_cmp(&other.0)
	}
}

impl<F: TotallyOrderable> Hash for TotallyOrdered<F> {
	fn hash<H: Hasher>(&self, state: &mut H) {
		self.0.total_hash(state);
	}
}

impl<F: TotallyOrderable> From<F> for TotallyOrdered<F> {
	fn from(v: F) -> Self {
		Self(v)
	}
}

impl<'a, F: TotallyOrderable> From<&'a F> for &'a TotallyOrdered<F> {
	/// The explicit lifetime bound ensures that both From
	/// ```compile_fail
	/// # use totally_ordered::*;
	/// let mut value : f32 = 1.0;
	/// let value_to : &TotallyOrdered<_> = From::from(&value);
	/// value = 2.0; // error, can't mutate while borrowed
	/// assert_eq!(*value_to, TotallyOrdered::new(1.0));
	/// ```
	/// and Into
	/// ```compile_fail
	/// # use totally_ordered::*;
	/// let mut value : f32 = 1.0;
	/// let value_to : &TotallyOrdered<_> = (&value).into();
	/// value = 2.0; // error, can't mutate while borrowed
	/// assert_eq!(*value_to, TotallyOrdered::new(1.0));
	/// ```
	/// respect the lifetime of the borrow on the original value.
	fn from(v: &F) -> Self {
		// TotallyOrdered is repr(transparent)
		unsafe { &*(v as *const _ as *const _) }
	}
}

impl<'a, F: TotallyOrderable> From<&'a mut F> for &'a mut TotallyOrdered<F> {
	/// The explicit lifetime bound ensures that both From
	/// ```compile_fail
	/// # use totally_ordered::*;
	/// let mut value : f32 = 1.0;
	/// let value_to : &mut TotallyOrdered<_> = From::from(&mut value);
	/// assert_eq!(value, 1.0); // error, can't borrow while mutably borrowed
	/// *value_to = TotallyOrdered::new(2.0);
	/// ```
	/// and Into
	/// ```compile_fail
	/// # use totally_ordered::*;
	/// let mut value : f32 = 1.0;
	/// let value_to : &mut TotallyOrdered<_> = (&mut value).into();
	/// assert_eq!(value, 1.0); // error, can't borrow while mutably borrowed
	/// *value_to = TotallyOrdered::new(2.0);
	/// ```
	/// respect the lifetime of the mutable borrow on the original value.
	fn from(v: &mut F) -> Self {
		// TotallyOrdered is repr(transparent)
		unsafe { &mut *(v as *mut _ as *mut _) }
	}
}

#[cfg(test)]
mod test {
	use crate::TotallyOrdered;
	use core::{f32, f64};

	#[test]
	fn test_total_order_f32() {
		let mut values = [
			-0.0,
			0.0,
			-1.0,
			1.0,
			f32::NEG_INFINITY,
			f32::INFINITY,
			-f32::NAN,
			f32::NAN,
			-f32::MIN_POSITIVE,
			f32::MIN_POSITIVE,
			f32::MIN,
			f32::MAX,
		];

		TotallyOrdered::new_slice_mut(&mut values).sort();
		let values_total = TotallyOrdered::new_slice(&values);

		assert_eq!(values_total[0], (-f32::NAN).into());
		assert_eq!(values_total[1], f32::NEG_INFINITY.into());
		assert_eq!(values_total[2], f32::MIN.into());
		assert_eq!(values_total[3], (-1.0).into());
		assert_eq!(values_total[4], (-f32::MIN_POSITIVE).into());
		assert_eq!(values_total[5], (-0.0).into());
		assert_eq!(values_total[6], 0.0.into());
		assert_eq!(values_total[7], f32::MIN_POSITIVE.into());
		assert_eq!(values_total[8], 1.0.into());
		assert_eq!(values_total[9], f32::MAX.into());
		assert_eq!(values_total[10], f32::INFINITY.into());
		assert_eq!(values_total[11], f32::NAN.into());

		assert_ne!(
			TotallyOrdered::new(-f32::NAN),
			TotallyOrdered::new(f32::NAN)
		);
		assert_ne!(TotallyOrdered::new(-0.0f32), TotallyOrdered::new(0.0f32));
	}

	#[test]
	fn test_total_order_f64() {
		let mut values = [
			-0.0,
			0.0,
			-1.0,
			1.0,
			f64::NEG_INFINITY,
			f64::INFINITY,
			-f64::NAN,
			f64::NAN,
			-f64::MIN_POSITIVE,
			f64::MIN_POSITIVE,
			f64::MIN,
			f64::MAX,
		];

		TotallyOrdered::new_slice_mut(&mut values).sort();
		let values_total = TotallyOrdered::new_slice(&values);

		assert_eq!(values_total[0], (-f64::NAN).into());
		assert_eq!(values_total[1], f64::NEG_INFINITY.into());
		assert_eq!(values_total[2], f64::MIN.into());
		assert_eq!(values_total[3], (-1.0).into());
		assert_eq!(values_total[4], (-f64::MIN_POSITIVE).into());
		assert_eq!(values_total[5], (-0.0).into());
		assert_eq!(values_total[6], 0.0.into());
		assert_eq!(values_total[7], f64::MIN_POSITIVE.into());
		assert_eq!(values_total[8], 1.0.into());
		assert_eq!(values_total[9], f64::MAX.into());
		assert_eq!(values_total[10], f64::INFINITY.into());
		assert_eq!(values_total[11], f64::NAN.into());

		assert_ne!(
			TotallyOrdered::new(-f64::NAN),
			TotallyOrdered::new(f64::NAN)
		);
		assert_ne!(TotallyOrdered::new(-0.0f64), TotallyOrdered::new(0.0f64));
	}
}
