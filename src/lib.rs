#![no_std]

use core::cmp::Ordering;
use core::hash::{Hash, Hasher};

pub trait TotallyOrderable {
	fn total_eq(&self, other: &Self) -> bool;
	fn total_cmp(&self, other: &Self) -> Ordering;
	fn total_hash<H: Hasher>(&self, state: &mut H);
}

macro_rules! implement_float_order {
	($F:ident, $U:ident, $I:ident, $N:literal) => {
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

#[derive(Copy, Clone, Debug, Default)]
#[repr(transparent)]
pub struct TotallyOrdered<F: TotallyOrderable>(pub F);

impl<F: TotallyOrderable> TotallyOrdered<F> {
	pub fn new(v: F) -> Self {
		Self(v)
	}

	pub fn new_slice(s: &[F]) -> &[Self] {
		use core::slice::from_raw_parts;
		// TotallyOrdered is repr(transparent)
		unsafe { from_raw_parts(s.as_ptr() as *const _, s.len()) }
	}

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

impl<F: TotallyOrderable> From<&F> for &TotallyOrdered<F> {
	fn from(v: &F) -> Self {
		// TotallyOrdered is repr(transparent)
		unsafe { &*(v as *const _ as *const _) }
	}
}

impl<F: TotallyOrderable> From<&mut F> for &mut TotallyOrdered<F> {
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
