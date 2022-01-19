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

use crate::bitboard::*;
use crate::evaluation::evaluate;
use rustc_hash::FxHashMap;
use rand::seq::SliceRandom;

/**
 * search.rs
 *
 * Searches through trees of variations to find the best continuation in a
 * position. Speedups through transposition tables (memoization) and
 * alpha-beta pruning.
 */

// Simple max and min functions so we don't have to keep writing the same
// ternary statement over and over
#[inline(always)]
fn max(a: f64, b: f64) -> f64 {
	if a > b { a } else { b }
}
#[inline(always)]
fn min(a: f64, b: f64) -> f64 {
	if a < b { a } else { b }
}

// For the purposes of training, I'm keeping this at a very tight 10 move rule
// to draw to try to push networks to fight for wins instead of repeating.
// However in an actual game it would be better to have a more proper 40 move
// rule or something.
pub const HALF_MOVES_TO_DRAW: u8 = 20;

/// From the current position, performs a quiescent search, searching with
/// unlimited depth until we reach a position where no imminent captures are
/// available.
fn q_minimax(
	current_pos: &Board,
	alpha: f64,
	beta: f64,
	is_maximizing: bool,
	evaluator: &Vec<Vec<f64>>) -> f64 {

	// note we only generate captures here, not sliding moves.
	let moves = gen_captures(current_pos);

	// rather than check for depth, we just check if there are any more captures
	// to be made, in which case we just return the static evaluation.
	if moves.len() == 0 {
		let eval = if is_maximizing {
			// If we are white, just evaluate the board without doing anything
			evaluate(current_pos, evaluator)
		} else {
			// else flip the board and evaluate from black's POV
			evaluate(&flip_board(current_pos), evaluator)
		};
		return eval;
	}

	// same minimax algorithm as regular minimax function below, except we aren't
	// memoizing for simplicity (and also its much rarer to transpose by captures)
	if is_maximizing {
		let mut max_eval = f64::MIN;
		for m in moves.iter() {
			let eval = q_minimax(&flip_board(m), alpha, beta, false, evaluator);
			max_eval = max(eval, max_eval);
			let new_alpha = max(alpha, eval);
			if beta <= new_alpha {
				break;
			}
		}
		max_eval
	} else {
		let mut min_eval = f64::MAX;
		for m in moves.iter() {
			let eval = q_minimax(&flip_board(m), alpha, beta, true, evaluator);
			min_eval = min(eval, min_eval);
			let new_beta = min(beta, eval);
			if new_beta <= alpha {
				break;
			}
		}
		min_eval
	}
}

/// From the current position, search _at least_ as deep as the given depth;
/// may search deeper with q_minimax if captures are available at the end of
/// the variation. Speedups through alpha beta pruning and memoizing.
fn minimax(
	current_pos: &Board,
	depth_remaining: u32,
	alpha: f64,
	beta: f64,
	is_maximizing: bool,
	evaluator: &Vec<Vec<f64>>,
	memo: &mut FxHashMap<Board, (u32, f64)>) -> f64 {

	// pretty instant check, no need to waste memory adding trivial positions
	// to memo table
	if current_pos.moves_since_action > HALF_MOVES_TO_DRAW {
		return 0.0;
	} 
	
	// check transposition table for already computed result
	if let Some(&(depth_after, eval_score)) = memo.get(current_pos) {
		if depth_after >= depth_remaining {
			return eval_score;
		}
	}

	// reached desired depth, now just do a quiescent search on current position
	// to get final evaluation
	if depth_remaining == 0 {
		let eval = q_minimax(current_pos, alpha, beta, is_maximizing, evaluator);

		// add evaluation to transposition table
		memo.insert(*current_pos, (0, eval));
		return eval;
	}

	// Generate ALL legal moves, not just captures
	let moves = gen_moves(current_pos);

	// This is pretty much a textbook minimax algorithm, so I won't annotate much.
	if is_maximizing {
		let mut max_eval = f64::MIN;
		for m in moves.iter() {
			// Flip board as we recur so we always evaluate from white perspective
			let eval = minimax(&flip_board(m), depth_remaining - 1,
				alpha, beta, false, evaluator, memo);
			max_eval = max(eval, max_eval);
			let new_alpha = max(alpha, eval);
			if beta <= new_alpha {
				break;
			}
		}
		// Insert the evaluation into transposition table and return.
		memo.insert(*current_pos, (depth_remaining, max_eval));
		max_eval
	} else {
		let mut min_eval = f64::MAX;
		for m in moves.iter() {
			let eval = minimax(&flip_board(m), depth_remaining - 1,
				alpha, beta, true, evaluator, memo);
			min_eval = min(eval, min_eval);
			let new_beta = min(beta, eval);
			if new_beta <= alpha {
				break;
			}
		}
		memo.insert(*current_pos, (depth_remaining, min_eval));
		min_eval
	}
}

/// Compute the best move for a given position after searching to the given
/// depth, and return a tuple containing the position after the best move
/// has been made as well as the evaluation for the given position.
pub fn make_move(board: &Board, depth: u32, evaluator: &Vec<Vec<f64>>)
	-> Option<(Board, f64)> {

	// We search every possible move, and return the one that yields the best
	// evaluation.
	let moves = gen_moves(board);

	// No moves to make. This should be treated as a loss.
	if moves.len() == 0 {
		return None;
	}

	// We can thread the memoization table throughout each search by passing it
	// as a mutable reference.
	let mut memo: FxHashMap<Board, (u32, f64)> = FxHashMap::default();
	let mut evals = Vec::new();
	let mut best_eval = f64::MIN;

	// For each move, get its corresponding evaluation. At the same time, we
	// keep track of the best evaluation we've seen so far.
	for m in moves.iter() {
		let eval = minimax(&flip_board(m), depth - 1, f64::MIN, f64::MAX, false,
			evaluator, &mut memo);
		best_eval = max(eval, best_eval);
		evals.push(eval);
	}

	// Match the evaluations up with the corresponding moves, and filter out
	// only the moves that led to the highest evaluation.
	let best_moves: Vec<(Board, f64)> = moves
		.into_iter()
		.zip(evals.into_iter())
		.filter(|(_,e)| *e == best_eval)
		.collect();

	// Pick a random move out of the equivalently best moves.
	Some(*(best_moves.choose(&mut rand::thread_rng()).unwrap()))
}


#[cfg(test)]
mod tests {
	use super::*;
	use crate::evaluation::genome_to_evaluator;

	#[test]
	fn minimax_test() {
		let m_e = genome_to_evaluator(&String::from("AAATTTGAAGCTACAAAACTTTGAAGCTACAAAAGTTTGAAGCTACAAAATTTTGAAGCTACAAACATTTGAAGCTACAAACCTTTGAAGCTACAAACGTTTGAAGCTACAAACTTTTGAAGCTACAAAGATTTGAAGCTACAAAGCTTTGAAGCTACAAAGGTTTGAAGCTACAAAGTTTTGAAGCTACAAATATTTGAAGCTACAAATCTTTGAAGCTACAAATGTTTGAAGCTACAAATTTTTGAAGCTACAACAATTTGAAGCTACAACACTTTGAAGCTACAACAGTTTGAAGCTACAACATTTTGAAGCTACAACCATTTGAAGCTACAACCCTTTGAAGCTACAACCGTTTGAAGCTACAACCTTTTGAAGCTACAACGATTTGAAGCTACAACGCTTTGAAGCTACAACGGTTTGAAGCTACAACGTTTTGAAGCTACAACTATTTGAAGCTACAACTCTTTGAAGCTACAACTGTTTGAAGCTACAACTTTTTGAAGCTACAA"));
		let mut map: FxHashMap<Board, (u32, f64)> = FxHashMap::default();

		assert_eq!(minimax(
			&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), 0, f64::MIN, f64::MAX, true, &m_e, &mut map), 0.0);
		assert_eq!(minimax(
			&string_to_board("\
			. . . .b\n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n\
			w. . . .\n"), 2, f64::MIN, f64::MAX, true, &m_e, &mut map), 0.0);
		assert_eq!(minimax(
			&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. .b. . \n \
			 . . . .\n\
			. .w. . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), 4, f64::MIN, f64::MAX, true, &m_e, &mut map), f64::MIN);
		assert_eq!(minimax(
			&string_to_board("\
			. . . . \n \
			 . .b. .\n\
			. . . . \n \
			 . . . .\n\
			. .w. . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), 4, f64::MIN, f64::MAX, true, &m_e, &mut map), f64::MAX);
		assert_eq!(minimax(
			&string_to_board("\
			. . .b. \n \
			 . . . .\n\
			.w. . .W\n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), 4, f64::MIN, f64::MAX, true, &m_e, &mut map), 3.0);
		assert_eq!(minimax(
			&string_to_board("\
			.b.b.b. \n \
			 . . . .\n\
			.w. . .W\n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), 4, f64::MIN, f64::MAX, true, &m_e, &mut map), -1.0);
		assert_eq!(minimax(
			&string_to_board("\
			. . . . \n \
			 . .b. .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . .w. .\n\
			. . . . \n \
			 . . . .\n"), 6, f64::MIN, f64::MAX, true, &m_e, &mut map), f64::MIN);
	}
}