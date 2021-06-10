use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use totally_ordered::*;

fn sort_slow<T: TotallyOrderable>(values: &mut Vec<T>) {
	let slice = TotallyOrdered::new_slice_mut(values.as_mut_slice());
	slice.sort_unstable();
}

fn sort_fast<T: InPlaceTotallyOrderable>(values: &mut Vec<T>) {
	total_sort(values);
}

fn sort_partial<T: PartialOrd>(values: &mut Vec<T>) {
	values.sort_unstable_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
}

fn sort_cached<T: TotallyOrderable>(values: &mut Vec<T>) {
	values.sort_by_cached_key(TotallyOrderable::total_ord_key);
}

fn random_vec<T>(n: usize) -> Vec<T>
where
	rand::distributions::Standard: rand::distributions::Distribution<T>,
{
	use rand::prelude::*;
	let mut rng = thread_rng();
	(0..n).map(|_| rng.gen()).collect()
}

fn bench_sorts<T>(c: &mut Criterion)
where
	T: PartialOrd + InPlaceTotallyOrderable + Clone,
	rand::distributions::Standard: rand::distributions::Distribution<T>,
{
	let mut group = c.benchmark_group(format!("Sort {}", std::any::type_name::<T>()));
	for p in 8..=24 {
		if p % 8 != 0 {
			continue;
		}
		let n = 1 << p;
		let input = random_vec::<T>(n);

		group.bench_with_input(BenchmarkId::new("Partial", n), &input, |b, i| {
			b.iter_batched_ref(
				|| i.clone(),
				|i| sort_partial(i),
				criterion::BatchSize::LargeInput,
			);
		});
		group.bench_with_input(BenchmarkId::new("Total", n), &input, |b, i| {
			b.iter_batched_ref(
				|| i.clone(),
				|i| sort_slow(i),
				criterion::BatchSize::LargeInput,
			);
		});
		group.bench_with_input(BenchmarkId::new("Cached key", n), &input, |b, i| {
			b.iter_batched_ref(
				|| i.clone(),
				|i| sort_cached(i),
				criterion::BatchSize::LargeInput,
			);
		});
		group.bench_with_input(BenchmarkId::new("In place key", n), &input, |b, i| {
			b.iter_batched_ref(
				|| i.clone(),
				|i| sort_fast(i),
				criterion::BatchSize::LargeInput,
			);
		});
	}
	group.finish();
}

criterion_group!(benches, bench_sorts::<f32>, bench_sorts::<f64>);
criterion_main!(benches);
