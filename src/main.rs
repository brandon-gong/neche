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

mod bitboard;
mod genome;
mod evaluation;
mod search;
mod evolution;

use std::fs;
use std::cmp::Ordering;

/// Main entry point for evolution. We check if there are existing files from
/// previous generations. If so, we attempt to continue from that point; if not,
/// we make the debug folder and start from zero.
fn main() {
	if fs::metadata("./nechedbg/").is_ok() {
		println!("Found debug folder, attempting to resume evolution...");
		evolution::evolve_resume(200, 200, 6);
	} else {
		println!("No debug folder found. Evolving from zero knowledge.");
		fs::create_dir("./nechedbg").unwrap();
		evolution::evolve_zero(200, 200, 6);
	}
}
