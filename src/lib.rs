#![no_std]

use core::cmp::Ordering;
use core::hash::{Hash, Hasher};

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
	pub fn new_slice(s: &[F]) -> &[Self] {
		use core::slice::from_raw_parts;
		// TotallyOrdered is repr(transparent)
		unsafe { from_raw_parts(s.as_ptr() as *const _, s.len()) }
	}

	/// Creates a mutable wrapped slice without copying data.
	/// Not implemented as `From<&mut [F]> for &mut [Self]` as
	/// slices are always foreign.
	pub fn new_slice_mut(s: &mut [F]) -> &mut [Self] {
		use core::slice::from_raw_parts_mut;
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
	fn from(v: &F) -> Self {
		// TotallyOrdered is repr(transparent)
		unsafe { &*(v as *const _ as *const _) }
	}
}

impl<'a, F: TotallyOrderable> From<&'a mut F> for &'a mut TotallyOrdered<F> {
	fn from(v: &mut F) -> Self {
		// TotallyOrdered is repr(transparent)
		unsafe { &mut *(v as *mut _ as *mut _) }
	}
}

#[cfg(test)]
mod test {
	#[test]
	fn test_total_order_f32() {
		use crate::TotallyOrdered;
		use core::f32;

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
		assert_ne!(TotallyOrdered::new(-0.0), TotallyOrdered::new(0.0));
	}

	#[test]
	fn test_total_order_f64() {
		use crate::TotallyOrdered;
		use core::f64;

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
		assert_ne!(TotallyOrdered::new(-0.0), TotallyOrdered::new(0.0));
	}
}
