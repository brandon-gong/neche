/*
Copyright (c) 2022 Brandon Gong

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

use std::fs;
use std::fs::OpenOptions;
use std::io::Write;
use std::io::Read;
use std::str::FromStr;
use std::cmp::Ordering;
use rand::Rng;
use rand::prelude::SliceRandom;
use rayon::prelude::*;
use crate::evaluation::genome_to_evaluator;
use crate::bitboard::*;
use crate::search::*;
use crate::genome::{gen_offspring, gen_f0_genome};

/**
 * evolution.rs
 * 
 * Evolves generations of networks, with the strongest networks having the
 * greatest probability of passing their genes on to the next generation.
 * Documents everything by writing to files.
 */

/// Simulates a game between two networks given their genomes.
/// Returns Ordering::Less if white loses, Ordering::Equal if the game result
/// is a draw, and Ordering::Greater if black loses.
pub fn simulate_game((white_genome, black_genome): (&String, &String),
	depth: u32) -> Ordering {
	// Standard starting position. Could be interesting to simulate from various
	// initial positions to improve various aspects of networks, i.e. defense,
	// endgames, tactical sharpness
	let mut position = string_to_board("\
			.b.b.b.b\n\
			b.b.b.b.\n\
			.b.b.b.b\n \
			 . . . .\n\
			. . . . \n\
			w.w.w.w.\n\
			.w.w.w.w\n\
			w.w.w.w.\n");

	let white_evaluator = genome_to_evaluator(white_genome);
	let black_evaluator = genome_to_evaluator(black_genome);

	// Loop indefinitely while the game is still going on.
	loop {

		// If we have exceeded the HALF_MOVES_TO_DRAW limit, the game ends in a draw
		if position.moves_since_action > HALF_MOVES_TO_DRAW {
			break Ordering::Equal;
		}

		// White moves first.
		match make_move(&position, depth, &white_evaluator) {
			// If white returns None, it means white had no moves. By checkers rules,
			// this is a loss for white.
			None => break Ordering::Less,

			// Otherwise, white plays a move and flips the board for black.
			Some((new_pos, _)) => position = flip_board(&new_pos)
		};

		// Now black moves.
		match make_move(&position, depth, &black_evaluator) {
			// Likewise, if black returns None, black lost.
			None => break Ordering::Greater,

			// Otherwise, black plays a move and flips the board black for white.
			Some((new_pos, _)) => position = flip_board(&new_pos)
		};
	}
}

/// Given a list of scores, rescale them into a probability distribution
/// in which higher scores are given a higher probability to be picked
fn rescale_probabilities(scores: &Vec<f64>, low_score: f64) -> Vec<f64> {

	// Shift all the scores up so that they are positive numbers (nonzero)
	let offset: Vec<f64> = scores
		.into_iter()
		.map(|x| x - low_score + 0.5)
		.collect();
	
	// Take sum and normalize by this sum.
	let sum: f64 = offset.iter().sum();
	offset.into_iter().map(|x| x / sum).collect()
}

/// Given a probability distribution, randomly return a number between 0 and
/// the length of the distribution.
fn rand_with_dist(dist: &Vec<f64>) -> usize {
	let mut sum = 0.0;
	let v: f64 = rand::thread_rng().gen();
	for i in 0..dist.len() {
		sum += dist[i];
		if sum > v {
			return i;
		}
	}
	dist.len() - 1
}

/// Continues evolution from a given generation. Loops infinitely and does
/// not return.
fn evolve_from_generation(mut latest: u32, mut generation: Vec<String>,
	depth: u32) -> ! {

	let generation_size = generation.len();

	print!("Opening best_networks.txt for writing... ");
	let mut best_networks = OpenOptions::new()
		.write(true)
		.create(true)
		.append(true)
		.open("./nechedbg/best_networks.txt")
		.unwrap();
	println!("done.");

	loop {
		println!("\n*** GENERATION {} ***", latest);

		print!("Initializing score table... ");
		let mut totalpoints = vec![0; generation_size];
		println!("done.");

		// Arbitrarily, do three rounds. I think more than one is necessary,
		// because there is randomness involved in the results, but too many would
		// be infeasible to compute in a reasonable time.
		for i in 0..3 {
			println!("Simulating round {}.", i + 1);
			let mut ranking: Vec<usize> = (0..generation_size).collect();
			// Shuffle the original list to randomize rankings in case of ties.
			ranking.shuffle(&mut rand::thread_rng());
			println!("Doing par_sort.");
			// Quicksort-ish sorting algorithm, reminiscent of Swiss-style tournament
			// format.
			ranking.par_sort_unstable_by(|&a, &b|
				simulate_game((&generation[a], &generation[b]), depth));
			
			// Now the number at the 0th index of the sorted ranking list will be
			// the index corresponding to the weakest network.
			for (pts, idx) in ranking.into_iter().enumerate() {
				totalpoints[idx] += pts;
			}
		}

		// Get highest performing network to write to best_networks file.
		let mut high_idx = 0;
		let mut high_score = 0;
		for i in 0..generation_size {
			if high_score < totalpoints[i] {
				high_score = totalpoints[i];
				high_idx = i;
			}
		}

		println!("Writing best network to file... ");
		best_networks.write_all((generation[high_idx].clone() + "\n").as_bytes())
			.expect("failed to write");
		println!("done.");

		print!("Rescaling scores to probabilities... ");
		let rescale = rescale_probabilities(&totalpoints
			.into_iter()
			.map(|x| x as f64)
			.collect(), 0.0);
		println!("done.");

		print!("Populating next generation... ");
		// increment generation number
		latest += 1;
		let mut new_generation = Vec::new();
		for _ in 0..generation_size {
			// We randomly select a parent with a non-uniform distribution such that
			// stronger networks are more likely to become parents
			new_generation.push(gen_offspring(
				&generation[rand_with_dist(&rescale)],
				&generation[rand_with_dist(&rescale)],
				0.03, // point mutate rate
				0.1 // translocate mutate rate
			));
		}
		println!("done.");

		generation = new_generation;
		print!("Writing new generation to file... ");
		let mut gen_file = OpenOptions::new()
			.write(true)
			.create(true)
			.truncate(true)
			.open(format!("./nechedbg/{}_nets.txt", latest))
			.unwrap();
		for network in generation.iter() {
			gen_file.write_all(format!("{}\n",
				network).as_bytes()).expect("failed to write");
		}
		println!("done.");
	}
}

/// Begin evolution from scratch.
pub fn evolve_zero(genome_size: u32, generation_size: u32, depth: u32) -> ! {
	println!("Beginning evolution from scratch.");

	print!("Generating F0 (parental) generation...");
	let generation: Vec<String> = (0..generation_size)
		.map(|_| gen_f0_genome(genome_size)).collect();
	println!("done.");

	print!("Writing F0 generation to file... ");
	let mut gen_file = OpenOptions::new()
		.write(true)
		.create(true)
		.truncate(true)
		.open("./nechedbg/0_nets.txt")
		.unwrap();
	for network in generation.iter() {
		gen_file.write_all(format!("{}\n",
			network).as_bytes()).expect("failed to write");
	}
	println!("done.");

	println!("Initialization completed.");
	evolve_from_generation(0, generation, depth)
}

/// Resume evolution from the latest generation.
pub fn evolve_resume(genome_size: u32, generation_size: u32, depth: u32) -> ! {
	println!("Checking for previous stored generations.");
	let paths = fs::read_dir("./nechedbg/").unwrap();
	let mut highest_gen = -1;

	for path in paths {
		// We know our network files are formatted like <number>_nets.txt
		let path_string = path.unwrap().file_name().into_string().unwrap();
		let name: Vec<&str> = path_string.split("_").collect();
		if name.len() == 2 && name[1] == "nets.txt" {
			// Try to parse it to a number, if not return a trash value
			let gen_num = i32::from_str(name[0]).unwrap_or(-2);
			if highest_gen < gen_num {
				highest_gen = gen_num;
			}
		}
	}

	if highest_gen == -1 {
		println!("No generations found. Evolving from zero.");
		evolve_zero(genome_size, generation_size, depth);
	} else {
		println!("Resuming evolution from generation {}.", highest_gen);
		let mut networks_file = OpenOptions::new()
			.read(true)
			.open(format!("./nechedbg/{}_nets.txt", highest_gen))
			.unwrap();

		let mut buffer = String::new();
		networks_file.read_to_string(&mut buffer).unwrap();
		// Split file input on newlines
		let mut networks: Vec<String> = buffer
			.split("\n")
			.map(|x| x.to_string())
			.collect();
		
		// Cut off any excess empty lines. This is important, the program
		// WILL PANIC if we neglect to do this
		while networks.len() > 100 { networks.pop(); }

		// Pass control over to evolve_from_generation
		evolve_from_generation(highest_gen.try_into().unwrap(), networks, depth);
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn g_idx_pairs_tests() {
		assert_eq!(generate_idx_pairs(3),
			vec![(0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1)]);
		assert_eq!(generate_idx_pairs(1), vec![]);
		assert_eq!(generate_idx_pairs(0), vec![]);
		assert_eq!(generate_idx_pairs(100).len(), 9900);
	}
}