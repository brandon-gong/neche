/*
Copyright (c) 2021 Brandon Gong

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


/**
 * bitboard.rs
 *
 * Defines functions for creating/pretty-printing boards, flipping orientation,
 * and generating captures/promotions/sliding moves.
 */


/// We use the following bitboard layout because it offers a nice, clean way
/// to generate moves (moves to the left is a rotate by 9, moves to the right
/// is a rotate by 1).
const BOARD_REPR: [i32; 32] = [
		 31,   23,   15,   7,
  30,   22,   14,   6,
     21,   13,   5,   29,
  20,   12,   4,   28,
     11,   3,   27,   19,
  10,   2,   26,   18,
     1,   25,   17,   9,
  0,   24,   16,   8,
];

const MASK_TOP_ROW: u32 = 0x80808080;
const MASK_LEFT_COL: u32 = 0x40100401;
const MASK_RIGHT_COL: u32 = 0x20080280;
const MASK_BOTTOM_ROW: u32 = 0x01010101;

const MASK_ULJUMP: u32 = !(MASK_RIGHT_COL | (MASK_RIGHT_COL >> 1)
	| MASK_BOTTOM_ROW | (MASK_BOTTOM_ROW << 1));
const MASK_URJUMP: u32 = !(MASK_LEFT_COL | (MASK_LEFT_COL << 1)
	| MASK_BOTTOM_ROW | (MASK_BOTTOM_ROW << 1));
const MASK_DLJUMP: u32 = !(MASK_RIGHT_COL | (MASK_RIGHT_COL >> 1)
	| MASK_TOP_ROW | (MASK_TOP_ROW >> 1));
const MASK_DRJUMP: u32 = !(MASK_LEFT_COL | (MASK_LEFT_COL << 1)
	| MASK_TOP_ROW | (MASK_TOP_ROW >> 1));

/// A board consists of white pieces (both men and kings), black pieces,
/// a general-purpose kings bitboard storing locations of both white and
/// black kings, and a moves_since_action which can be used to implement
/// 50-move rules.
#[derive(Eq, PartialEq, Debug, Hash, Copy, Clone)]
pub struct Board {
	pub white: u32,
	pub black: u32,
	pub kings: u32,
	pub moves_since_action: u8
}

////////////////////////////////////////////////////////////////////////////

/// Given a string that may contain extra formatting characters, convert it
/// to the internal bitboard representation.
pub fn string_to_board(string: &str) -> Board {
	let (mut w, mut b, mut k): (u32, u32, u32) = (0, 0, 0);
	let mut indices = BOARD_REPR.iter();
	for c in string.chars() {
		match c {
			'w' => match indices.next() {
				Some(v) => w |= 1 << v,
				None => ()
			},
			'b' => match indices.next() {
				Some(v) => b |= 1 << v,
				None => ()
			},
			'W' => match indices.next() {
				Some(v) => { w |= 1 << v; k |= 1 << v; },
				None => ()
			},
			'B' => match indices.next() {
				Some(v) => { b |= 1 << v; k |= 1 << v; },
				None => ()
			},
			' ' => {indices.next();},
			_ => ()
		}
	}
	Board { white: w, black: b, kings: k, moves_since_action: 0 }
}

/// Convert the internal bitboard representation to a human-readable, printable
/// string.
pub fn board_to_string(board: &Board) -> String {
	let mut result = String::new();
	let decoration = (0..32).map(|x| match x {
		4 | 12 | 20 | 28 => "\n",
		8 | 16 | 24 => ".\n.",
		_ => "."
	});
	for (d, i) in decoration.zip(BOARD_REPR) {
		result.push_str(d);
		let mask = 1 << i;
		result.push(if board.white & board.kings & mask != 0 {
			'W'
		} else if board.black & board.kings & mask != 0 {
			'B'
		} else if board.white & mask != 0 {
			'w'
		} else if board.black & mask != 0 {
			'b'
		} else if !(board.white | board.black) & board.kings & mask != 0 {
			panic!("corrupted board: king not associated with piece");
		} else if board.white & board.black & mask != 0 {
			panic!("corrupted board: doubly occupied square");
		} else {
			' '
		});
	}
	result.push_str(".\n");
	result
}

/////////////////////////////////////////////////////////////////////////////

/// "Flips" a board, rotating it by 180deg and switching white and black colors.
pub fn flip_board(board: &Board) -> Board {
	Board {
		white: board.black.reverse_bits().rotate_left(8),
		black: board.white.reverse_bits().rotate_left(8),
		kings: board.kings.reverse_bits().rotate_left(8),
		moves_since_action: board.moves_since_action
	}
}

/////////////////////////////////////////////////////////////////////////////

/// Given a board and a vector of potentially already-computed moves, push
/// moves that are legal sliding promotions onto the vector.
pub fn gen_sliding_promotions(board: &Board, mut moves: Vec<Board>)
	-> Vec<Board> {
	let open_squares = MASK_TOP_ROW & !(board.white | board.black);
	let white_men = board.white & !board.kings;
	let mut add_moves = |mut movers: u32, offset| {
		while movers != 0 {
			let square = 1 << movers.trailing_zeros();
			let inv_mask = !square;
			moves.push(Board {
				white: board.white & inv_mask | (square << offset),
				black: board.black,
				kings: board.kings | (square << offset),
				moves_since_action: 0
			});
			movers &= inv_mask;
		}
	};
	add_moves((open_squares >> 1) & white_men, 1);
	add_moves(((open_squares & !MASK_RIGHT_COL) >> 9) & white_men, 9);
	moves
}

/// Generate all legal sliding men moves, NOT promotions or king slides.
pub fn gen_sliding_men(board: &Board, mut moves: Vec<Board>) -> Vec<Board> {
	let open_squares = !MASK_TOP_ROW & !(board.white | board.black);
	let white_men = board.white & !board.kings;
	let mut add_moves = |mut movers: u32, offset| {
		while movers != 0 {
			let square = 1 << movers.trailing_zeros();
			let inv_mask = !square;
			moves.push(Board {
				white: board.white & inv_mask | square.rotate_left(offset),
				black: board.black,
				kings: board.kings,
				moves_since_action: board.moves_since_action + 1
			});
			movers &= inv_mask;
		}
	};
	add_moves((open_squares & !MASK_LEFT_COL).rotate_right(1) & white_men, 1);
	add_moves((open_squares & !MASK_RIGHT_COL).rotate_right(9) & white_men, 9);
	moves
}

/// Generate all legal sliding king moves.
pub fn gen_sliding_kings(board: &Board, mut moves: Vec<Board>) -> Vec<Board> {
	let white_kings = board.white & board.kings;
	let open_up = !MASK_BOTTOM_ROW & !(board.white | board.black);
	let open_down = !MASK_TOP_ROW & !(board.white | board.black);
	let mut add_moves = |mut movers: u32, offset| {
		while movers != 0 {
			let square = 1 << movers.trailing_zeros();
			let inv_mask = !square;
			moves.push(Board {
				white: board.white & inv_mask | square.rotate_left(offset),
				black: board.black,
				kings: board.kings & inv_mask | square.rotate_left(offset),
				moves_since_action: board.moves_since_action + 1
			});
			movers &= inv_mask;
		}
	};
	add_moves((open_up & !MASK_LEFT_COL).rotate_right(1) & white_kings, 1);
	add_moves((open_up & !MASK_RIGHT_COL).rotate_right(9) & white_kings, 9);
	add_moves((open_down & !MASK_RIGHT_COL).rotate_right(31) & white_kings, 31);
	add_moves((open_down & !MASK_LEFT_COL).rotate_right(23) & white_kings, 23);
	moves
}

/// Generate all legal captures made by MEN, not kings. Also handles promotion.
pub fn gen_men_captures(board: &Board, mut moves: Vec<Board>) -> Vec<Board> {
	let mut to_search = Vec::new();
	let get_jump_squares = |b: &Board| {
		let empty_squares = !(b.white | b.black);
		let empty_ljump = MASK_ULJUMP & empty_squares;
		let empty_rjump = MASK_URJUMP & empty_squares;
		let left_jumps = (empty_ljump.rotate_right(9) & b.black).rotate_right(9);
		let right_jumps = (empty_rjump.rotate_right(1) & b.black).rotate_right(1);
		(left_jumps, right_jumps)
	};
	let get_new_board = |old_board: &Board, from_mask: u32, offset| {
		let new_white = old_board.white
			& !from_mask
			| from_mask.rotate_left(2 * offset);
		let new_black = old_board.black
			& !(from_mask.rotate_left(offset));
		let new_kings = old_board.kings
			& !(from_mask.rotate_left(offset))
			| (MASK_TOP_ROW & new_white);
		(Board {
			white: new_white,
			black: new_black,
			kings: new_kings,
			moves_since_action: 0
		}, from_mask.rotate_left(2 * offset))
	};

	let white_men = board.white & !board.kings;
	let (mut left_jumps, mut right_jumps) = get_jump_squares(&board);
	left_jumps &= white_men;
	right_jumps &= white_men;
	while left_jumps != 0 {
		let square = 1 << left_jumps.trailing_zeros(); // take advantage of BSF/CTZ
		to_search.push(get_new_board(&board, square, 9));
		left_jumps &= !square;
	}
	while right_jumps != 0 {
		let square = 1 << right_jumps.trailing_zeros();
		to_search.push(get_new_board(&board, square, 1));
		right_jumps &= !square;
	}
	while let Some((b, i)) = to_search.pop() {
		let (lj, rj) = get_jump_squares(&b);
		let lj_available = (lj & i) != 0;
		let rj_available = (rj & i) != 0;
		if !lj_available && !rj_available {
			moves.push(b);
			continue;
		}
		if lj_available { to_search.push(get_new_board(&b, i, 9)); }
		if rj_available { to_search.push(get_new_board(&b, i, 1)); }
	}
	moves
}

/// Generate all legal captures made by kings.
pub fn gen_kings_captures(board: &Board, mut moves: Vec<Board>) -> Vec<Board> {
	let mut to_search = Vec::new();
	let get_jump_squares = |b: &Board| {
		let empty_squares = !(b.white | b.black);
		let empty_uljump = MASK_ULJUMP & empty_squares;
		let empty_urjump = MASK_URJUMP & empty_squares;
		let empty_dljump = MASK_DLJUMP & empty_squares;
		let empty_drjump = MASK_DRJUMP & empty_squares;
		let ul_jumps = (empty_uljump.rotate_right(9) & b.black).rotate_right(9);
		let ur_jumps = (empty_urjump.rotate_right(1) & b.black).rotate_right(1);
		let dl_jumps = (empty_dljump.rotate_left(1) & b.black).rotate_left(1);
		let dr_jumps = (empty_drjump.rotate_left(9) & b.black).rotate_left(9);
		(ul_jumps, ur_jumps, dl_jumps, dr_jumps)
	};
	let get_new_board = |old_board: &Board, from_mask: u32, offset| {
		let new_white = old_board.white
			& !from_mask
			| from_mask.rotate_left(2 * offset);
		let new_black = old_board.black
			& !(from_mask.rotate_left(offset));
		let new_kings = old_board.kings
			& !from_mask
			& !(from_mask.rotate_left(offset))
			| from_mask.rotate_left(2 * offset);
		(Board {
			white: new_white,
			black: new_black,
			kings: new_kings,
			moves_since_action: 0
		}, from_mask.rotate_left(2 * offset))
	};

	let white_kings = board.white & board.kings;
	let (mut ul_jumps,
		mut ur_jumps,
		mut dl_jumps,
		mut dr_jumps) = get_jump_squares(&board);
	ul_jumps &= white_kings;
	ur_jumps &= white_kings;
	dl_jumps &= white_kings;
	dr_jumps &= white_kings;
	while ul_jumps != 0 {
		let square = 1 << ul_jumps.trailing_zeros();
		to_search.push(get_new_board(&board, square, 9));
		ul_jumps &= !square;
	}
	while ur_jumps != 0 {
		let square = 1 << ur_jumps.trailing_zeros();
		to_search.push(get_new_board(&board, square, 1));
		ur_jumps &= !square;
	}
	while dl_jumps != 0 {
		let square = 1 << dl_jumps.trailing_zeros();
		to_search.push(get_new_board(&board, square, 31));
		dl_jumps &= !square;
	}
	while dr_jumps != 0 {
		let square = 1 << dr_jumps.trailing_zeros();
		to_search.push(get_new_board(&board, square, 23));
		dr_jumps &= !square;
	}
	while let Some((b, i)) = to_search.pop() {
		let (ulj, urj, dlj, drj) = get_jump_squares(&b);
		let ulj_available = (ulj & i) != 0;
		let urj_available = (urj & i) != 0;
		let dlj_available = (dlj & i) != 0;
		let drj_available = (drj & i) != 0;
		if !(ulj_available || urj_available || dlj_available || drj_available) {
			moves.push(b);
			continue;
		}
		if ulj_available { to_search.push(get_new_board(&b, i, 9)); }
		if urj_available { to_search.push(get_new_board(&b, i, 1)); }
		if dlj_available { to_search.push(get_new_board(&b, i, 31)); }
		if drj_available { to_search.push(get_new_board(&b, i, 23)); }
	}
	moves
}

////////////////////////////////////////////////////////////////////////////

/// Given a board, generate all legal capturing moves possible on the board.
pub fn gen_captures(board: &Board) -> Vec<Board> {
	gen_men_captures(board, gen_kings_captures(board, Vec::new()))
}

/// Given a board, generate all legal moves for the position.
pub fn gen_moves(board: &Board) -> Vec<Board> {
	let caps = gen_captures(board);
	if caps.len() != 0 {
		// force captures. if we want to change rules to captures not forced,
		// just use the else branch only to generate all moves
		caps
	} else {
		// We generate in this order to try to induce early alpha/beta cutoff
		// if captures aren't forced, consider generating them last
		// (after promotions) because they are often good moves
		gen_sliding_promotions(board,
			gen_sliding_men(board,
				gen_sliding_kings(board, caps)))
	}
}

/////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn board_repr_and_equality() {
		let mut v = BOARD_REPR.to_vec();
		v.sort();
		assert_eq!(v, (0..32).collect::<Vec<i32>>());

		assert_eq!(Board {
			white: 1, black: 2, kings: 3, moves_since_action: 0
		}, Board {
			white: 1, black: 2, kings: 3, moves_since_action: 0
		});
		assert_ne!(Board {
			white: 1, black: 2, kings: 3, moves_since_action: 0
		}, Board {
			white: 1, black: 2, kings: 2, moves_since_action: 0
		});
		assert_ne!(Board {
			white: 1000000, black: 2, kings: 3, moves_since_action: 1
		}, Board {
			white: 1000000, black: 2, kings: 3, moves_since_action: 0
		});
	}

	#[test]
	fn string_to_board_tests() {
		assert_eq!(
			string_to_board("wwwwwwwwwwwwwwwwwwwwwwwwwwwwwwww"),
			Board {
				white: 0xffffffff,
				black: 0,
				kings: 0,
				moves_since_action: 0 
			});
		assert_eq!(
			string_to_board("W     asdfghj      $@#&          kl;[]!          "),
			Board {
				white: 0x80000000,
				black: 0,
				kings: 0x80000000,
				moves_since_action: 0
			});
		assert_eq!(
			string_to_board("\
			.w.W.b. \n\
			B. .b.b.\n\
			.W.W.w.w\n \
			 . .w.B.\n\
			. .b.B.w\n\
			w.B.W.b.\n\
			. .b. .W\n\
			B. . .B.\n"),
			Board {
				white: 0b10100100101010000010011000110000,
				black: 0b01011010000001001100000101001101,
				kings: 0b01011100101000000010001100000101,
				moves_since_action: 0
			});
	}

	#[test]
	fn board_to_string_tests() {
		assert_eq!(
		board_to_string(&Board {
				white: 0b10100100101010000010011000110000,
				black: 0b01011010000001001100000101001101,
				kings: 0b01011100101000000010001100000101,
				moves_since_action: 0
			}), "\
			.w.W.b. \n\
			B. .b.b.\n\
			.W.W.w.w\n \
			 . .w.B.\n\
			. .b.B.w\n\
			w.B.W.b.\n\
			. .b. .W\n\
			B. . .B.\n");
		assert_eq!(
			board_to_string(&Board {
				white: 0,
				black: 0x80000001,
				kings: 0x80000000,
				moves_since_action: 0
			}), "\
			.B. . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
 			. . . . \n \
			 . . . .\n\
			. . . . \n\
			b. . . .\n");
	}

	#[test]
	fn flip_tests() {
		assert_eq!(flip_board(&Board {
			white: 0, black: 0, kings: 0, moves_since_action: 12}), Board {
			white: 0, black: 0, kings: 0, moves_since_action: 12});
		assert_eq!(board_to_string(&flip_board(&string_to_board("\
			.W. . .b\n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
 			. . . . \n \
			 . . . .\n\
			. . . . \n\
			w. . .B.\n"
		))), "\
			.W. . .b\n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
 			. . . . \n \
			 . . . .\n\
			. . . . \n\
			w. . .B.\n");
		assert_eq!(board_to_string(&flip_board(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . .W.\n\
 			. . . . \n \
			 . . . .\n\
			. . . . \n\
			b. . . .\n"
		))), "\
			. . . .w\n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
 			.B. . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n");
		assert_eq!(board_to_string(&flip_board(&string_to_board("\
			.w.W.b.B\n\
			b.W.w.B.\n\
			.b.b.w.W\n\
			w.b.B.W.\n\
 			.w.w.w.b\n\
			B.B.W.b.\n\
			.W.b.W.B\n\
			b.w.b.w.\n"
		))), "\
			.b.w.b.w\n\
			W.B.w.B.\n\
			.w.B.W.W\n\
			w.b.b.b.\n\
 			.B.W.w.b\n\
			B.b.w.w.\n\
			.W.b.B.w\n\
			W.w.B.b.\n");
		assert_eq!(board_to_string(&flip_board(&string_to_board("\
			. .W.B. \n\
			w.w. .b.\n\
			. .b.B.B\n \
			 .w. .w.\n\
			.W.b.b.B\n\
			B. . .w.\n\
			. . .W.b\n\
			b. .w. .\n"))), "\
			. .b. .w\n\
			w.B. . .\n\
			.b. . .W\n\
			W.w.w.B.\n\
 			.b. .b. \n\
			W.W.w. .\n\
			.w. .b.b\n \
			 .W.B. .\n");
		assert_eq!(board_to_string(&flip_board(&string_to_board(
			".b.b.b. \n\
			 w. . . .\n\
		 	 . . . .W\n \
				. . . .\n\
		 	 . . . . \n \
				. . . .\n\
		 	 . . . . \n \
			  . . . .\n"))),
			". . . . \n \
			  . . . .\n\
		 	 . . . . \n \
				. . . .\n\
		 	 . . . . \n\
			 B. . . .\n\
		 	 . . . .b\n \
			  .w.w.w.\n");
	}

	#[test]
	fn gen_sliding_promotions_tests() {
		assert_eq!(gen_sliding_promotions(&string_to_board("\
			. . . . \n \
			 .w. . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". .W. . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
				".W. . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n"
			]);
		assert_eq!(gen_sliding_promotions(&string_to_board("\
			.B. . . \n\
			w. . .w.\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				".B. . .W\n\
				 w. . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
				".B. .W. \n\
				 w. . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n"
			]);
		assert_eq!(gen_sliding_promotions(&string_to_board("\
			. . .w.b\n\
			w. . .w.\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				".W. .w.b\n \
				  . . .w.\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_sliding_promotions(&string_to_board("\
			. . . . \n \
			 . . .w.\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . .W\n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
				". . .W. \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_sliding_promotions(&string_to_board("\
			. .b. .W\n\
			W.w.w.w.\n\
			. .b. . \n\
			b.w. .W.\n\
			. .w.w.B\n\
			b.b. . .\n\
			. .B.W. \n \
			 .w. .w.\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". .b.W.W\n\
				 W.w. .w.\n\
				 . .b. . \n\
				 b.w. .W.\n\
				 . .w.w.B\n\
				 b.b. . .\n\
				 . .B.W. \n \
				  .w. .w.\n",
				". .b.W.W\n\
				 W.w.w. .\n\
				 . .b. . \n\
				 b.w. .W.\n\
				 . .w.w.B\n\
				 b.b. . .\n\
				 . .B.W. \n \
				  .w. .w.\n",
				".W.b. .W\n\
				 W. .w.w.\n\
				 . .b. . \n\
				 b.w. .W.\n\
				 . .w.w.B\n\
				 b.b. . .\n\
				 . .B.W. \n \
				  .w. .w.\n",
			]);
		assert_eq!(gen_sliding_promotions(&string_to_board("\
			. .b.b.W\n\
			W. .w.w.\n\
			. .b. . \n\
			b.w. .W.\n\
			. .w.w.B\n\
			b.b. . .\n\
			. .B.W. \n \
			 .w. .w.\n"), Vec::new()).len(), 0);	
		assert_eq!(gen_sliding_promotions(&string_to_board("\
			. . . . \n\
			w.w.w.w.\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new()).len(), 7);
	}

	#[test]
	fn gen_sliding_men_tests() {
		assert_eq!(gen_sliding_men(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . .w. .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . .w. \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
				". . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . .w. . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_sliding_men(&string_to_board("\
			. . . . \n \
			 . . .w.\n\
			. . . . \n\
			w. . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . . .w.\n\
				 .w. . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_sliding_men(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . .W.\n\
			. . . .w\n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new()).len(), 0);
		assert_eq!(gen_sliding_men(&string_to_board("\
			. . . . \n \
			 . . .w.\n\
			. . . .w\n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new()).len(), 0);
		assert_eq!(gen_sliding_men(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . .w\n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n\
			w. . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . . . .\n\
				 . . . .w\n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 .w. . . \n \
				  . . . .\n",
				". . . . \n \
				  . . .w.\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n\
				 w. . . .\n",
			]);
		assert_eq!(gen_sliding_men(&string_to_board("\
			.W.b. . \n\
			b. .w.B.\n\
			.w. .w. \n \
			 . .W. .\n\
			.b. .b. \n\
			w. .w. .\n\
			. . .W.B\n \
			 .w. .b.\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				".W.b. . \n\
				 b.w.w.B.\n\
				 . . .w. \n \
				  . .W. .\n\
				 .b. .b. \n\
				 w. .w. .\n\
				 . . .W.B\n \
				  .w. .b.\n",
				".W.b. . \n\
				 b. .w.B.\n\
				 .w. .w. \n \
				  . .W. .\n\
				 .b. .b. \n\
				 w. .w. .\n\
				 . .w.W.B\n \
				  . . .b.\n",
				".W.b. . \n\
				 b. .w.B.\n\
				 .w. .w. \n \
				  . .W. .\n\
				 .b. .b. \n\
				 w. .w. .\n\
				 .w. .W.B\n \
				  . . .b.\n",
				".W.b. . \n\
				 b. .w.B.\n\
				 .w. .w. \n \
				  . .W. .\n\
				 .b.w.b. \n\
				 w. . . .\n\
				 . . .W.B\n \
				  .w. .b.\n",
			]);
		assert_eq!(gen_sliding_men(&string_to_board("\
			.b.b.b.b\n\
			b.b.b.b.\n\
			.b.b.b.b\n \
			 . . . .\n\
			. . . . \n\
			w.w.w.w.\n\
			.w.w.w.w\n\
			w.w.w.w.\n"), Vec::new()).len(), 7);
		assert_eq!(gen_sliding_men(&string_to_board("\
			. .w. . \n \
			w. .w.w.\n\
			.w. . . \n \
			 . . . .\n\
			.w.w.w.w\n \
			 . . . .\n\
			. . . . \n\
			w.w.w.w.\n"), Vec::new()).len(), 15);
		assert_eq!(gen_sliding_men(&string_to_board("\
			. . . . \n \
			 .w.B. .\n\
			. .w. . \n \
			 .W. .b.\n\
			.B. . .w\n \
			 . .w.B.\n\
			.W.b. . \n\
			w. .w.w.\n"), Vec::new()).len(), 5);
	}

	#[test]
	fn gen_sliding_kings_tests() {
		assert_eq!(gen_sliding_kings(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . .W. .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . .W. \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
				". . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . .W. . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
				". . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . .W. . \n \
				  . . . .\n",
				". . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . .W. \n \
				  . . . .\n",
			]);
		assert_eq!(gen_sliding_kings(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 .W. . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . .W. . \n \
				  . . . .\n",
				". . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 .W. . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_sliding_kings(&string_to_board("\
			. .W. . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  .W. . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
				". . . . \n \
				  . .W. .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_sliding_kings(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . .W\n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . .W.\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
				". . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . .W.\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_sliding_kings(&string_to_board("\
			. . . .W\n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . . .W.\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_sliding_kings(&string_to_board("\
			. . . . \n\
			W. . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				".W. . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
				". . . . \n \
				  . . . .\n\
				 .W. . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_sliding_kings(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n\
			W. . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . . . .\n\
				 .W. . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
				". . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 .W. . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_sliding_kings(&string_to_board("\
			.W. .b. \n \
			 . .w.W.\n\
			.w.b. .W\n \
			 .w.w.b.\n\
			. .b.W. \n\
			B. .W.w.\n\
			. .b. . \n\
			W. .B.w.\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				".W. .b. \n \
				  . .w.W.\n\
				 .w.b. .W\n \
				  .w.w.b.\n\
				 . .b.W. \n\
				 B. .W.w.\n\
				 .W.b. . \n \
				  . .B.w.\n",
				".W. .b.W\n \
				  . .w. .\n\
				 .w.b. .W\n \
				  .w.w.b.\n\
				 . .b.W. \n\
				 B. .W.w.\n\
				 . .b. . \n\
				 W. .B.w.\n",
				".W. .b. \n \
				  . .w. .\n\
				 .w.b.W.W\n \
				  .w.w.b.\n\
				 . .b.W. \n\
				 B. .W.w.\n\
				 . .b. . \n\
				 W. .B.w.\n",
				". . .b. \n\
				 W. .w.W.\n\
				 .w.b. .W\n \
				  .w.w.b.\n\
				 . .b.W. \n\
				 B. .W.w.\n\
				 . .b. . \n\
				 W. .B.w.\n",
				".W. .b. \n \
				  . .w.W.\n\
				 .w.b. .W\n \
				  .w.w.b.\n\
				 . .b.W. \n\
				 B. . .w.\n\
				 . .b.W. \n\
				 W. .B.w.\n",
				". . .b. \n \
				  .W.w.W.\n\
				 .w.b. .W\n \
				  .w.w.b.\n\
				 . .b.W. \n\
				 B. .W.w.\n\
				 . .b. . \n\
				 W. .B.w.\n",
			]);
		assert_eq!(gen_sliding_kings(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 .w.w. .\n\
			. .W. . \n \
			 .w.w. .\n\
			. . . . \n \
			 . . . .\n"), Vec::new()).len(), 0);
		assert_eq!(gen_sliding_kings(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 .W.W. .\n\
			. .W. . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new()).len(), 8);
		assert_eq!(gen_sliding_kings(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n\
			W.b. . .\n\
			.W. . . \n \
			 .W. . .\n"), Vec::new()).len(), 3);
		assert_eq!(gen_sliding_kings(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n\
			 . . . .\n\
			. . .B. \n \
			 . . .W.\n"), Vec::new()).len(), 1);
		assert_eq!(gen_sliding_kings(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . .B.W\n \
			 . . .W.\n"), Vec::new()).len(), 1);
	}

	#[test]
	fn gen_men_captures_tests() {
		assert_eq!(gen_men_captures(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. .b.B. \n \
			 . .w. .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . .w.\n\
				 . .b. . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
				". . . . \n \
				  . . . .\n\
				 . . . . \n \
				  .w. . .\n\
				 . . .B. \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_men_captures(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . .b. \n \
			 . . . .\n\
			. . .B. \n \
			 . .w. .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . .w. .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_men_captures(&string_to_board("\
			. . . . \n \
			 .b. . .\n\
			.w. . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". .W. . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_men_captures(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. .b.b. \n \
			 . . . .\n\
			. .B.B. \n \
			 . . . .\n\
			. .B.b. \n \
			 . .w. .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . . .w.\n\
				 . .b. . \n \
				  . . . .\n\
				 . .B. . \n \
				  . . . .\n\
				 . .B. . \n \
				  . . . .\n",
				". . . . \n \
				  .w. . .\n\
				 . . .b. \n \
				  . . . .\n\
				 . .B. . \n \
				  . . . .\n\
				 . .B. . \n \
				  . . . .\n",
				". . . . \n \
				  . . .w.\n\
				 . .b. . \n \
				  . . . .\n\
				 . . .B. \n \
				  . . . .\n\
				 . . .b. \n \
				  . . . .\n",
				". . . . \n \
				  .w. . .\n\
				 . . .b. \n \
				  . . . .\n\
				 . . .B. \n \
				  . . . .\n\
				 . . .b. \n \
				  . . . .\n",
			]);
		assert_eq!(gen_men_captures(&string_to_board("\
			. . . . \n \
			 .b.b. .\n\
			. . . . \n \
			 .b.b. .\n\
			.w. .w. \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . .W. \n \
				  .b. . .\n\
				 . . . . \n \
				  . .b. .\n\
				 . . .w. \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
				".W. . . \n \
				  . .b. .\n\
				 . . . . \n \
				  . .b. .\n\
				 . . .w. \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
				". . .W. \n \
				  .b. . .\n\
				 . . . . \n \
				  .b. . .\n\
				 .w. . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
				".W. . . \n \
				  . .b. .\n\
				 . . . . \n \
				  .b. . .\n\
				 .w. . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_men_captures(&string_to_board("\
			. . . . \n \
			 . . .B.\n\
			. . . .w\n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . .W. \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_men_captures(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . .B. \n \
			 . . . .\n\
			. .B. . \n \
			 . . . .\n\
			.B. . . \n\
			w. . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . . .w.\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_men_captures(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . .b. \n \
			 . . .w.\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . .w. .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_men_captures(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n\
			b. . . .\n\
			.w. . . \n \
			 . . . .\n"), Vec::new()).len(), 0);
		assert_eq!(gen_men_captures(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . .b\n\
			w. . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new()).len(), 0);
		assert_eq!(gen_men_captures(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . .b\n \
			 . . .w.\n\
			. . . . \n \
			 . . . .\n"), Vec::new()).len(), 0);
		assert_eq!(gen_men_captures(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n\
			b. . . .\n\
			. . . .w\n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new()).len(), 0);
		assert_eq!(gen_men_captures(&string_to_board("\
			. . . . \n \
			 . . .b.\n\
			. . .w. \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . .W\n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_men_captures(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			.B. . . \n \
			 .w. . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n\
				 w. . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_men_captures(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . .b.\n\
			. . .w. \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . . . .\n\
				 . . . .w\n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_men_captures(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . .b\n \
			 . . .w.\n"), Vec::new()).len(), 0);
		assert_eq!(gen_men_captures(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. .b.b. \n \
			 . . . .\n\
			. .b. . \n \
			 . . . .\n\
			.b. . . \n\
			w. . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . . .w.\n\
				 . .b. . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
				". . . . \n \
				  .w. . .\n\
				 . . .b. \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_men_captures(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . .b\n \
			 .b. . .\n\
			. .b.B. \n \
			 . .w. .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . .b\n \
				  .b. .w.\n\
				 . .b. . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_men_captures(&string_to_board("\
			.W. . . \n \
			 .b.b.w.\n\
			. .w. . \n \
			 . .B.b.\n\
			. .w.w. \n \
			 .b. . .\n\
			. .b.W. \n \
			 . .w.W.\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				".W. . . \n \
				  .b.b.w.\n\
				 . .w. .w\n \
				  . .B. .\n\
				 . .w. . \n \
				  .b. . .\n\
				 . .b.W. \n \
				  . .w.W.\n",
				".W. .W. \n \
				  .b. .w.\n\
				 . . . . \n \
				  . .B.b.\n\
				 . .w.w. \n \
				  .b. . .\n\
				 . .b.W. \n \
				  . .w.W.\n",
				".W.W. . \n \
				  .b. .w.\n\
				 . .w. . \n \
				  . . .b.\n\
				 . . .w. \n \
				  .b. . .\n\
				 . .b.W. \n \
				  . .w.W.\n",
			]);
	}

	#[test]
	fn gen_kings_captures_tests() {
		assert_eq!(gen_kings_captures(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. .b.b. \n \
			 . .W. .\n\
			. .b.b. \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . . . .\n\
				 . .b.b. \n \
				  . . . .\n\
				 . .b. . \n \
				  . . .W.\n\
				 . . . . \n \
				  . . . .\n",
				". . . . \n \
				  . . . .\n\
				 . .b.b. \n \
				  . . . .\n\
				 . . .b. \n \
				  .W. . .\n\
				 . . . . \n \
				  . . . .\n",
				". . . . \n \
				  . . .W.\n\
				 . .b. . \n \
				  . . . .\n\
				 . .b.b. \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
				". . . . \n \
				  .W. . .\n\
				 . . .b. \n \
				  . . . .\n\
				 . .b.b. \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_kings_captures(&string_to_board("\
			. . . . \n \
			 . .W. .\n\
			. .b.b. \n \
			 . . . .\n\
			. .b.b. \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![ // CCW and CW both paths transpose to same position
				". . . . \n \
				  . .W. .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
				". . . . \n \
				  . .W. .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_kings_captures(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. .b.B. \n \
			 . . . .\n\
			. .b.B. \n \
			 . . .W.\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . . . .\n\
				 . .b.B. \n \
				  . . . .\n\
				 . . . . \n \
				  .W. . .\n\
				 . . . . \n \
				  . . . .\n",
				". . . . \n \
				  . . .W.\n\
				 . .b. . \n \
				  . . . .\n\
				 . .b. . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
				". . . . \n \
				  .W. . .\n\
				 . . .B. \n \
				  . . . .\n\
				 . .b. . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_kings_captures(&string_to_board("\
			.W. . .W\n\
			b.b. .B.\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . .W\n\
				 b. . .B.\n\
				 . .W. . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
				".W. . . \n\
				 b.b. . .\n\
				 . . .W. \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_kings_captures(&string_to_board("\
			. . . . \n \
			 .W. . .\n\
			. .B. . \n \
			 . . . .\n\
			. .B.b. \n \
			 .B. . .\n\
			. . .b. \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 . .B. . \n \
				  .B. . .\n\
				 . . . . \n \
				  . .W. .\n",
			]);
		assert_eq!(gen_kings_captures(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . .b. \n \
			 . . . .\n\
			.b.b.b. \n\
			W. . . .\n\
			.B. . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				". . . . \n \
				  . . . .\n\
				 . . .b. \n \
				  . . . .\n\
				 .b.b.b. \n \
				  . . . .\n\
				 . . . . \n \
				  .W. . .\n",
				". . . . \n \
				  . .W. .\n\
				 . . . . \n \
				  . . . .\n\
				 . . . . \n \
				  . . . .\n\
				 .B. . . \n \
				  . . . .\n",
			]);
		assert_eq!(gen_kings_captures(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . .b\n \
			 . . .W.\n\
			. . . . \n \
			 . . . .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			 Vec::<String>::new());
		assert_eq!(gen_kings_captures(&string_to_board("\
			.W. . . \n\
			B. . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new()).len(), 0);
		assert_eq!(gen_kings_captures(&string_to_board("\
			. . . . \n \
			 . . .W.\n\
			. . . .b\n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new()).len(), 0);
		assert_eq!(gen_kings_captures(&string_to_board("\
			. . . . \n \
			 . . . .\n\
			. . . . \n\
			b. . . .\n\
			.W. . . \n \
			 . . . .\n\
			. . . . \n \
			 . . . .\n"), Vec::new()).len(), 0);
		assert_eq!(gen_kings_captures(&string_to_board("\
			.W. . . \n \
			 .b.b.w.\n\
			. .w. . \n \
			 . .B.b.\n\
			. .w.w. \n \
			 .b. . .\n\
			. .b.W. \n \
			 . .w.W.\n"), Vec::new()).len(), 0);
		assert_eq!(gen_kings_captures(&string_to_board("\
			.W. . .W\n\
			w.w. .b.\n\
			. . . .w\n \
			 .B.w.B.\n\
			.B. . .b\n \
			 .W.b. .\n\
			. . .W.w\n\
			b.W.b. .\n"), Vec::new())
			 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
			vec![
				".W. . . \n\
				 w.w. . .\n\
				 . . .W.w\n \
				  .B.w.B.\n\
				 .B. . .b\n \
				  .W.b. .\n\
				 . . .W.w\n\
				 b.W.b. .\n",
				".W. . .W\n\
				 w.w. .b.\n\
				 .W. . .w\n \
				  . .w.B.\n\
				 .B. . .b\n \
				  .W. . .\n\
				 . . . .w\n\
				 b.W.b. .\n",
				".W. . .W\n\
				 w.w. .b.\n\
				 . . . .w\n\
				 W.B.w.B.\n\
				 . . . .b\n \
				  . .b. .\n\
				 . . .W.w\n\
				 b.W.b. .\n",
			]);
	}

	#[test]
	fn gen_captures_tests() {
		assert_eq!(gen_captures(&string_to_board("\
		. . . . \n \
		 . . . .\n\
		. .b.b. \n \
		 . .W. .\n\
		. .b.b. \n \
		 . . . .\n\
		. . . . \n \
		 . . . .\n"))
		 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
		vec![
			". . . . \n \
				. . . .\n\
			 . .b.b. \n \
				. . . .\n\
			 . .b. . \n \
				. . .W.\n\
			 . . . . \n \
				. . . .\n",
			". . . . \n \
				. . . .\n\
			 . .b.b. \n \
				. . . .\n\
			 . . .b. \n \
				.W. . .\n\
			 . . . . \n \
				. . . .\n",
			". . . . \n \
				. . .W.\n\
			 . .b. . \n \
				. . . .\n\
			 . .b.b. \n \
				. . . .\n\
			 . . . . \n \
				. . . .\n",
			". . . . \n \
				.W. . .\n\
			 . . .b. \n \
				. . . .\n\
			 . .b.b. \n \
				. . . .\n\
			 . . . . \n \
				. . . .\n",
		]);
		assert_eq!(gen_captures(&string_to_board("\
		. . . . \n \
		 . . . .\n\
		. . . . \n \
		 . .b. .\n\
		. .w.W. \n \
		 . . . .\n\
		. . . . \n \
		 . . . .\n"))
		 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
		vec![
			". . . . \n \
				. . . .\n\
			 . .W. . \n \
				. . . .\n\
			 . .w. . \n \
				. . . .\n\
			 . . . . \n \
				. . . .\n",
				". . . . \n \
				. . . .\n\
			 . . .w. \n \
				. . . .\n\
			 . . .W. \n \
				. . . .\n\
			 . . . . \n \
				. . . .\n",
		]);
		assert_eq!(gen_captures(&string_to_board("\
		.W. .W. \n \
		 .B.b. .\n\
		. .w. . \n \
		 . . . .\n\
		. . . . \n \
		 . . . .\n\
		. . . . \n \
		 . . . .\n")).len(), 0);
	}

	#[test]
	fn gen_moves_tests() {
		assert_eq!(gen_moves(&string_to_board("\
		.W. .b. \n \
		 .w. .W.\n\
		. .B.b. \n \
		 .w. . .\n\
		. .b.b. \n \
		 . . . .\n\
		. . . . \n \
		 . . . .\n"))
		 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
		vec![
			".W. .b. \n \
			  .w. . .\n\
			 . .B. . \n \
			  .w. . .\n\
			 . .b. . \n \
			  . . .W.\n\
			 . . . . \n \
			  . . . .\n",
			".W. .b. \n \
			  .w. . .\n\
		 	 . .B. . \n \
				.w. . .\n\
		 	 . . .b. \n \
				.W. . .\n\
		 	 . . . . \n \
				. . . .\n",
			".W. .b. \n \
			  .w.w.W.\n\
		 	 . . .b. \n \
				. . . .\n\
		 	 . .b.b. \n \
				. . . .\n\
		 	 . . . . \n \
				. . . .\n",
		]);
		assert_eq!(gen_moves(&string_to_board("\
		.W. .b. \n \
		 .w.b.W.\n\
		. .B.b. \n \
		 .w.w. .\n\
		. .w. . \n \
		 . .W. .\n\
		. . . . \n \
		 . . . .\n"))
		 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
		vec![
			".W. .b.W\n \
			  .w.b. .\n\
			 . .B.b. \n \
			  .w.w. .\n\
			 . .w. . \n \
			  . .W. .\n\
			 . . . . \n \
			  . . . .\n",
			".W. .b. \n \
			  .w.b.W.\n\
			 . .B.b. \n \
			  .w.w. .\n\
			 . .w.W. \n \
			  . . . .\n\
			 . . . . \n \
			  . . . .\n",
			".W. .b. \n \
			  .w.b.W.\n\
			 . .B.b. \n \
			  .w.w. .\n\
			 . .w. . \n \
			  . . . .\n\
			 . .W. . \n \
			  . . . .\n",
			". . .b. \n\
			 W.w.b.W.\n\
			 . .B.b. \n \
			  .w.w. .\n\
			 . .w. . \n \
			  . .W. .\n\
			 . . . . \n \
			  . . . .\n",
			".W. .b. \n \
			  .w.b. .\n\
			 . .B.b.W\n \
			  .w.w. .\n\
			 . .w. . \n \
			  . .W. .\n\
			 . . . . \n \
			  . . . .\n",
			".W. .b. \n \
			  .w.b.W.\n\
			 . .B.b. \n \
			  .w.w. .\n\
			 . .w. . \n \
			  . . . .\n\
			 . . .W. \n \
			  . . . .\n",
			".W. .b. \n \
			  .w.b.W.\n\
			 .w.B.b. \n \
			  . .w. .\n\
			 . .w. . \n \
			  . .W. .\n\
			 . . . . \n \
			  . . . .\n",
			".W.W.b. \n \
			  . .b.W.\n\
			 . .B.b. \n \
			  .w.w. .\n\
			 . .w. . \n \
			  . .W. .\n\
			 . . . . \n \
			  . . . .\n",
		]);
		assert_eq!(gen_moves(&string_to_board("\
		. . . . \n\
		w. . . .\n\
		. . . . \n \
		 . . . .\n\
		. . . . \n \
		 . . . .\n\
		. . . . \n\
		w. . . .\n"))
		 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
		vec![
			". . . . \n\
			 w. . . .\n\
		 	 . . . . \n \
				. . . .\n\
		 	 . . . . \n \
				. . . .\n\
		 	 .w. . . \n \
			  . . . .\n",
			".W. . . \n \
			  . . . .\n\
		 	 . . . . \n \
				. . . .\n\
		 	 . . . . \n \
				. . . .\n\
		 	 . . . . \n\
			 w. . . .\n",
		]);
		assert_eq!(gen_moves(&string_to_board("\
		. . . . \n\
		w. . . .\n\
		. . . . \n \
		 . . . .\n\
		. . . . \n \
		 . . . .\n\
		.b. . . \n\
		w. . . .\n"))
		 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
		vec![
			". . . . \n\
			 w. . . .\n\
		 	 . . . . \n \
				. . . .\n\
		 	 . . . . \n \
				.w. . .\n\
		 	 . . . . \n \
			  . . . .\n",
		]);
		assert_eq!(gen_moves(&string_to_board("\
		.b.b.b. \n \
		 . . . .\n\
		.w. . .W\n \
		 . . . .\n\
		. . . . \n \
		 . . . .\n\
		. . . . \n \
		 . . . .\n"))
		 .into_iter().map(|x| board_to_string(&x)).collect::<Vec<String>>(),
		vec![
			".b.b.b. \n \
			  . . .W.\n\
		 	 .w. . . \n \
				. . . .\n\
		 	 . . . . \n \
				. . . .\n\
		 	 . . . . \n \
			  . . . .\n",
			".b.b.b. \n \
			  . . . .\n\
		 	 .w. . . \n \
				. . .W.\n\
		 	 . . . . \n \
				. . . .\n\
		 	 . . . . \n \
			  . . . .\n",
			".b.b.b. \n \
			  .w. . .\n\
		 	 . . . .W\n \
				. . . .\n\
		 	 . . . . \n \
				. . . .\n\
		 	 . . . . \n \
			  . . . .\n",
			".b.b.b. \n\
			 w. . . .\n\
		 	 . . . .W\n \
				. . . .\n\
		 	 . . . . \n \
				. . . .\n\
		 	 . . . . \n \
			  . . . .\n",
		]);
		assert_eq!(gen_moves(&string_to_board(
			". . . . \n \
			  . . . .\n\
		 	 . . . . \n \
				. . . .\n\
		 	 . . . . \n\
			 B. . . .\n\
		 	 . . . .b\n \
			  .w.w.w.\n")).len(), 5);
	}
}
