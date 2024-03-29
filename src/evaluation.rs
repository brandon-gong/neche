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

use crate::genome;
use crate::bitboard;

/**
 * evaluation.rs
 *
 * Implements static evaluation of positions via neural networks.
 */

/// Parse a genome (String of basepairs) into a vector of row matrices
/// that can be used for evaluation.
pub fn genome_to_evaluator(genome: &String) -> Vec<Vec<f64>> {
	// Our resulting matrix. Each row has 64 elements, and there are 63 rows
	// total.
	let mut evaluator = vec![vec![0.0; 64]; 63];

	// a is the "from" node, b is the "to" node, and w is the raw weight value.
	for (a, b, w) in genome::parse_genome(genome).into_iter() {
		// Set the bth element in row a to be the actual weight of the connection.
		evaluator[a as usize][b as usize] = genome::raw_to_weight(w);
	}

	// Done, return matrix.
	evaluator
}

/// Rectified linear unit activation function for inner neurons.
/// ReLU is simply a truncated linear function that acts as identity function
/// for non-negative numbers, and zeros out negative numbers.
fn relu(n: f64) -> f64 {
	if n < 0. { 0. } else { n }
}

/// Given a legal board position, compute the static evaluation using the
/// given evaluator.
pub fn evaluate(board: &bitboard::Board, evaluator: &Vec<Vec<f64>>) -> f64 {

	// For each of our 64 neurons, they all have an activation value. This
	// vector will hold all of those values.
	// Note that this is the same size as each row of the evaluator matrix.
	// For each row vector r that holds the weight of a neurons outbound
	// connections, we are going to do activations += value * r
	let mut activations = vec! [0.0; 64];

	// Neurons 0 through 31 are correlated with board squares.
	// -2 for B, -1 for b, 0 for empty, 1 for w, 2 for W.
	for i in 0..32 {
		let square = 1 << i;

		// This is probably not faster, but it is branchless, so hopefully it is not
		// slower?
		let scalar = ((1.0 * ((board.white & square) >> i) as f64)
			+ (-1.0 * ((board.black & square) >> i) as f64))
			* ((1 << ((board.kings & square) >> i)) as f64);
		
		// Add the scalar multiplied by the corresponding weight for each neuron.
		// Note that disconnected neurons are implicitly connected, with a weight of
		// 0.
		for j in 39..64 {
			activations[j] += scalar * evaluator[i][j];
		}
	}

	// bias neurons always output a 1, so we just add on the weight directly
	for i in 32..39 {
		for j in 39..64 {
			activations[j] += evaluator[i][j];
		}
	}

	// This is our first layer of internal neurons. We apply relu to the input
	// value and add on its activation multiplied by the weight of the connection
	for i in 39..55 {
		let val = relu(activations[i]);
		for j in 55..64 {
			activations[j] += val * evaluator[i][j];
		}
	}

	// Same process for layer 2 internal neurons.
	for i in 55..63 {
		let val = relu(activations[i]);
		activations[63] += val * evaluator[i][63];
	}

	// Return the final output activation
	activations[63]
}



#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn evaluation_tests() {
		assert_eq!(evaluate(
			&bitboard::string_to_board("\
			.w.W.b. \n\
			B. .b.b.\n\
			.W.W.w.w\n \
			 . .w.B.\n\
			. .b.B.w\n\
			w.B.W.b.\n\
			. .b. .W\n\
			B. . .B.\n"),
			&genome_to_evaluator(&String::from("AAATTTGAAGCTACAAAACTTTGAAGCTACAAAAGTTTGAAGCTACAAAATTTTGAAGCTACAAACATTTGAAGCTACAAACCTTTGAAGCTACAAACGTTTGAAGCTACAAACTTTTGAAGCTACAAAGATTTGAAGCTACAAAGCTTTGAAGCTACAAAGGTTTGAAGCTACAAAGTTTTGAAGCTACAAATATTTGAAGCTACAAATCTTTGAAGCTACAAATGTTTGAAGCTACAAATTTTTGAAGCTACAACAATTTGAAGCTACAACACTTTGAAGCTACAACAGTTTGAAGCTACAACATTTTGAAGCTACAACCATTTGAAGCTACAACCCTTTGAAGCTACAACCGTTTGAAGCTACAACCTTTTGAAGCTACAACGATTTGAAGCTACAACGCTTTGAAGCTACAACGGTTTGAAGCTACAACGTTTTGAAGCTACAACTATTTGAAGCTACAACTCTTTGAAGCTACAACTGTTTGAAGCTACAACTTTTTGAAGCTACAA"))
		), -2.0);
		assert_eq!(evaluate(
			&bitboard::string_to_board("\
			.w. . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"),
			&genome_to_evaluator(&String::from("AAATTTGAAGCTACAAAACTTTGAAGCTACAAAAGTTTGAAGCTACAAAATTTTGAAGCTACAAACATTTGAAGCTACAAACCTTTGAAGCTACAAACGTTTGAAGCTACAAACTTTTGAAGCTACAAAGATTTGAAGCTACAAAGCTTTGAAGCTACAAAGGTTTGAAGCTACAAAGTTTTGAAGCTACAAATATTTGAAGCTACAAATCTTTGAAGCTACAAATGTTTGAAGCTACAAATTTTTGAAGCTACAACAATTTGAAGCTACAACACTTTGAAGCTACAACAGTTTGAAGCTACAACATTTTGAAGCTACAACCATTTGAAGCTACAACCCTTTGAAGCTACAACCGTTTGAAGCTACAACCTTTTGAAGCTACAACGATTTGAAGCTACAACGCTTTGAAGCTACAACGGTTTGAAGCTACAACGTTTTGAAGCTACAACTATTTGAAGCTACAACTCTTTGAAGCTACAACTGTTTGAAGCTACAACTTTTTGAAGCTACAA"))
		), 1.0);
		assert_eq!(evaluate(
			&bitboard::string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . .B. .\n\
			. . . . \n \
			 . .B. .\n\
			. . . . \n \
			 . . . .\n"),
			&genome_to_evaluator(&String::from("AAATTTGAAGCTACAAAACTTTGAAGCTACAAAAGTTTGAAGCTACAAAATTTTGAAGCTACAAACATTTGAAGCTACAAACCTTTGAAGCTACAAACGTTTGAAGCTACAAACTTTTGAAGCTACAAAGATTTGAAGCTACAAAGCTTTGAAGCTACAAAGGTTTGAAGCTACAAAGTTTTGAAGCTACAAATATTTGAAGCTACAAATCTTTGAAGCTACAAATGTTTGAAGCTACAAATTTTTGAAGCTACAACAATTTGAAGCTACAACACTTTGAAGCTACAACAGTTTGAAGCTACAACATTTTGAAGCTACAACCATTTGAAGCTACAACCCTTTGAAGCTACAACCGTTTGAAGCTACAACCTTTTGAAGCTACAACGATTTGAAGCTACAACGCTTTGAAGCTACAACGGTTTGAAGCTACAACGTTTTGAAGCTACAACTATTTGAAGCTACAACTCTTTGAAGCTACAACTGTTTGAAGCTACAACTTTTTGAAGCTACAA"))
		), -4.0);
		assert_eq!(evaluate(
			&bitboard::string_to_board("\
			.w.W.b. \n\
			B. .b.b.\n\
			.W.W.w.w\n \
			 . .w.B.\n\
			. .b.B.w\n\
			w.B.W.b.\n\
			. .b. .W\n\
			B. . .B.\n"),
			&genome_to_evaluator(&String::from("CATTGCGCTCCATAAATGCGAAATCCGGTAACGAATTTGAAGCTACAATGCTTTCTTTCGATTA"))
		), -2.0);
		assert_eq!(evaluate(
			&bitboard::string_to_board("\
			.w.W.b. \n\
			B. .b.b.\n\
			.W.W.w.w\n \
			 . .w.B.\n\
			. .b.B.W\n\
			w.B.W.b.\n\
			. .b. .W\n\
			B. . .B.\n"),
			&genome_to_evaluator(&String::from("CATTGCGCTCCATAAATGCGAAATCCGGTAACGAATTTGAAGCTACAATGCTTTCTTTCGATTA"))
		), -5.0);
		assert_eq!(evaluate(
			&bitboard::string_to_board("\
			.w.W.b. \n\
			B. .b.b.\n\
			.W.W.w.w\n \
			 . .w.B.\n\
			. .b.B.b\n\
			w.B.W.b.\n\
			. .b. .W\n\
			B. . .B.\n"),
			&genome_to_evaluator(&String::from("CATTGCGCTCCATAAATGCGAAATCCGGTAACGAATTTGAAGCTACAATGCTTTCTTTCGATTA"))
		), 1.0);
		assert_eq!(evaluate(
			&bitboard::string_to_board("\
			.w.W.b. \n\
			B. .b.b.\n\
			.W.W.w.w\n \
			 . .w.B.\n\
			. .b.B.B\n\
			w.B.W.b.\n\
			. .b. .W\n\
			B. . .B.\n"),
			&genome_to_evaluator(&String::from("CATTGCGCTCCATAAATGCGAAATCCGGTAACGAATTTGAAGCTACAATGCTTTCTTTCGATTA"))
		), 1.0);
		assert_eq!(evaluate(
			&bitboard::string_to_board("\
			.w.W.b. \n\
			B. .b.b.\n\
			.W.W.w.w\n \
			 . .w.B.\n\
			. .b.B.B\n\
			w.B.W.b.\n\
			. .b. .W\n\
			B. . .B.\n"),
			&genome_to_evaluator(&String::from("GGTTTTGAAACTTCAACACGGTGACATGAGAAACATCTTGGACACCCGCCTGGTCTTCGATTAAGAGGGTCTGAGGTCAAGGGACGAAAAAAAAAC"))
		), 0.0);
		assert!((evaluate(
			&bitboard::string_to_board("\
			. .B. . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . .W. \n \
			 . . . .\n"),
			&genome_to_evaluator(&String::from("GGTTTTGAAACTTCAACACGGTGACATGAGAAACATCTTGGACACCCGCCTGGTCTTCGATTAAGAGGGTCTGAGGTCAAGGGACGAAAAAAAAAC"))
		) - 0.6).abs() < 0.000000001);
		assert!((evaluate(
			&bitboard::string_to_board("\
			. .B. . \n \
			 . . . .\n\
			. . . . \n \
			 . .W. .\n\
			. . . . \n \
			 . . . .\n\
			. . .W. \n \
			 . . . .\n"),
			&genome_to_evaluator(&String::from("GGTTTTGAAACTTCAACACGGTGACATGAGAAACATCTTGGACACCCGCCTGGTCTTCGATTAAGAGGGTCTGAGGTCAAGGGACGAAAAAAAAAC"))
		) - 0.6).abs() < 0.000000001);
		assert_eq!(evaluate(
			&bitboard::string_to_board("\
			. .w. . \n \
			 . . . .\n\
			. . . . \n \
			 . .W. .\n\
			. . . . \n \
			 . . . .\n\
			. . .w. \n \
			 . . . .\n"),
			&genome_to_evaluator(&String::from("GGTTTTGAAACTTCAACACGGTGACATGAGAAACATCTTGGACACCCGCCTGGTCTTCGATTAAGAGGGTCTGAGGTCAAGGGACGAAAAAAAAAC"))
		), 0.0);
	}
}
