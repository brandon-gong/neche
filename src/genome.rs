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

use rand::Rng;
use std::fs::OpenOptions;
use std::io::Write;

/**
 * genome.rs
 *
 * Helper functions for creating, mutating, and hybridizing genome sequences,
 * as well as outputting files containing genomes or their visualizations.
 */

/// Convert raw genome weights to actual weights used in the network.
pub fn raw_to_weight(raw: u32) -> f64 { 
	 ((raw as f64) - 524288.0) / 10000.0
}

/// Converts a segment of a genome from its base-4 ATCG reprecentation to
/// a number.
fn to_num(segment: &[u8]) -> u32 {

	// We are essentially doing a base 4 to base 2 conversion.
	let mut res = 0;

	for c in segment.iter() {
		// shift left by 2 bits to make space for the next base-4 value
		res <<= 2;
		res |= match c {
			65 => 0b00, // A corresponds to 0
			67 => 0b01, // C = 1
			71 => 0b10, // G = 2
			84 => 0b11, // T = 3
			_ => panic!("Invalid genome in to_num") // not A, T, C, or G
		};
	}
	res
}

/// Parse a genome into a vector of tuples (a, b, w), where a is the
/// ID of the neuron the connection is going outward from, b is the neuron
/// that is receiving the connection, and w is the weight of the connection.
pub fn parse_genome(genome: &String) -> Vec<(u32, u32, u32)> {
	
	let gene_bytes = genome.as_bytes();

	// Each single gene is 16 characters long, so if our genome is not a multiple
	// of 16 something bad happened
	if gene_bytes.len() % 16 != 0 {
		panic!("Invalid gene: bp not right length");
	}

	// Points to the beginning of each gene
	let mut s = 0;

	// info will hold all of our parsed edges
	let mut info = Vec::new();

	while s < gene_bytes.len() {
		// The first three bits encodes the source neuron, the second three
		// bits encodes the sink neuron, and the last three bits encodes the weight.
		info.push( (to_num(&gene_bytes[s..s+3]), to_num(&gene_bytes[s+3..s+6]),
			to_num(&gene_bytes[s+6..s+16])) );
		
		// move pointer to beginning of next gene
		s += 16;
	}

	// Filter out "invalid edges", i.e. backwards and self connections, by
	// restricting what neurons the edge can end at based on where it starts at
	info.into_iter().filter(|&(a, b, _)| {
		if a < 39 {
			// Input neurons cannot connect to other input neurons
			b >= 39 && b <= 63
		} else if a < 55 {
			// Layer 1 neurons cannot connect to input neurons or other L1 neurons
			b >= 55 && b <= 63
		} else if a < 63 {
			// Layer 2 neurons can only connect to the output neuron
			b == 63
		} else {
			// Catch all other edge cases
			false
		}
	}).collect()
}

/// Determines if a given genome is "viable", i.e. if the genome has any neurons
/// connecting to OUT.
fn is_viable(genome: &String) -> bool {
	// Simply parse the genome and count up the number of connections to OUT.
	// We need at least one connection to OUT, otherwise it's pointless.
	parse_genome(genome)
		.into_iter()
		.filter(|&(_, b, _)| b == 63)
		.collect::<Vec<(u32, u32, u32)>>()
		.len() > 0 
}

/// Generate a completely random and naive genome for an individual in the
/// parental (F0) generation.
pub fn gen_f0_genome(num_genes: u32) -> String {
  let mut rng = rand::thread_rng();

	// Repeatedly generate new random strings until we have a valid genome.
	// This seems terribly inefficient, and it is, but it isn't really a "hot"
	// piece of code, so it's ok.
	loop {
		// Allocate a string of the correct size
  	let mut gene = String::with_capacity((num_genes * 16).try_into().unwrap());
		for _ in 0..(16 * num_genes) {
			// Fairly self explanatory, simply push a random A, C, G, or T based on
			// what rng gives us.
			gene.push(match rng.gen_range(0..4) {
				0 => 'A',
				1 => 'C',
				2 => 'G',
				3 => 'T',
				_ => panic!("rng broke")
    	});
  	}
		// If it's viable, go ahead and return, else start over.
		if is_viable(&gene) { return gene; }
	}
}

/// Induce point mutations in the genome (single bp changes).
fn point_mutate(genome: &String, mutation_rate: f64) -> String {
  let mut rng = rand::thread_rng();

	// Will hold the new genome after mutations.
	let mut new = String::with_capacity(genome.len());
	for c in genome.chars() {
		if rng.gen::<f64>() < mutation_rate {
			// Induce a mutation by adding a random base pair at this location.
	    new.push(match rng.gen_range(0..4) {
	      0 => 'A',
	      1 => 'C',
	      2 => 'G',
	      3 => 'T',
	    	 _ => panic!("rng broke")
    	});
		} else {
			// Most likely case; here just correctly replicate the genome's base pair
			new.push(c);
		}
	}

	new
}

/// Reshuffle a chunk of genes in the genome.
fn translocation_mutate(genome: &String, mutation_rate: f64) -> String {
	let mut rng = rand::thread_rng();
	if rng.gen::<f64>() < mutation_rate {

		// A quick and dirty hack to force mutation to occur. I've found this is
		// necessary, otherwise even with 100% mutation rate oftentimes nothing
		// happens.
		let mut newgene = genome.to_string();

		// Keep looping while nothing happens
		while &newgene == genome {

			let genome_size = genome.len() / 16;

			// Cut along gene lines by randomly picking a starting and ending point.
			let excision_start = rng.gen_range(0..genome_size);
			let excision_end = rng.gen_range(excision_start..(genome_size + 1));
			let excision =  genome
				.chars()
				.skip(excision_start * 16)
				.take(16 * (excision_end - excision_start));

			// Remainder is the whole genome, except without the excision.
			let remainder: String = genome
				.chars()
				.take(excision_start * 16)
				.chain(genome.chars().skip(excision_end * 16))
				.collect();
			
			// Now we reinsert the excision at a random position in the remaining
			// genome, but it must be at the border between two genes. We do this
			// to avoid frameshift mutations, which I think would be too extreme
			let remainder_size = remainder.len() / 16;
			let insertion_point = 16 * rng.gen_range(0..(remainder_size + 1));
			newgene = remainder.chars()
				.take(insertion_point)
				.chain(excision)
				.chain(remainder.chars().skip(insertion_point))
				.collect();
		}
		newgene

	} else {
		// Nothing happens.
		genome.clone()
	}
}

/// Given two parental genomes, mix and match their genes and then induce
/// mutations in the child genes.
pub fn gen_offspring(
	p1: &String,
	p2: &String,
	point_mutation_rate: f64,
	translocation_mutation_rate: f64) -> String {

	// randomly take genes from parents, and then perform point & translocation
	// mutations
	let mut rng = rand::thread_rng();

	// Chunk each parent genes into segments of 16 characters (one gene)
	let p1_genes: Vec<String> = p1.chars()
  	.collect::<Vec<char>>()
    .chunks(16)
    .map(|x| x.iter().collect::<String>()).collect();
	let p2_genes: Vec<String> = p2.chars()
  	.collect::<Vec<char>>()
    .chunks(16)
    .map(|x| x.iter().collect::<String>()).collect();
	
	// Similar to gen_f0_genome, we will loop until we have a viable offspring.
	loop {
		let mut child_genes = Vec::new();

		// Randomly take genes at each position from each parent
		for (g1, g2) in p1_genes.iter().zip(p2_genes.iter()) {
			if rng.gen::<f64>() < 0.5 {
				child_genes.push(g1.clone());
			} else {
				child_genes.push(g2.clone());
			}
		}

		// Apply mutations to this child genome
		let res = translocation_mutate(
			&point_mutate(&child_genes.join(""), point_mutation_rate),
			translocation_mutation_rate);
		
		// Only return child genome if it is viable, otherwise try again
		if is_viable(&res) { return res; }
	}
}

/// Writes a genome out to a .dot file that can be passed through
/// GraphViz to produce a visualization of the network structure.
#[allow(dead_code)]
pub fn write_to_dot(genome: &String, filename: &String) -> () {
	// Parse genome into edges that are easier to work with
	let processed: Vec<(u32, u32, u32)> = parse_genome(genome);

	// As far as I know, GraphViz does not have an easy way to strip out
	// declared but unused nodes, so we only want to declare nodes that are
	// actually connect in our network.
	let mut used_idx = Vec::new();
	for r in processed.iter() {
		if !used_idx.contains(&r.0) {
			used_idx.push(r.0);
		}
		if !used_idx.contains(&r.1) {
			used_idx.push(r.1);
		}
	}

	// Output is a string containing everything we want to write to the file
	// in the end
	let mut output = String::from("digraph network {\nrankdir=LR;\n\
		overlap=\"prism\";\nsep=\"+15\";\nnode [shape=\"circle\"]\n");
	
	// Pretty-printed square names, corresponding to the BOARD_REPR indices order
	let ppsquare = vec!["A1", "B2", "C3", "D4", "E5", "F6", "G7", "H8", "G1",
		"H2", "A3", "B4", "C5", "D6", "E7", "F8", "E1", "F2", "G3", "H4", "A5",
		"B6", "C7", "D8", "C1", "D2", "E3", "F4", "G5", "H6", "A7", "B8"];
	
	// Declare all used input neurons
	for i in 0..32 {
		if used_idx.contains(&i) {
			output.push_str(&format!("{} [label=\"{}\"]\n", i, ppsquare[i as usize]));
		}
	}

	// Change to double circle style
	output.push_str("node [shape=\"doublecircle\"]\n");

	// Declare all used bias neurons
	for i in 32..39 {
		if used_idx.contains(&i) {
			output.push_str(&format!(
				"{} [label=<<FONT POINT-SIZE=\"16\">b</FONT>\
				<SUB><FONT POINT-SIZE=\"11\">{}</FONT></SUB>>]\n", i, i - 32));
		}
	}

	// Change to square style
	output.push_str("node [shape=\"square\"]\n");

	// Declare all used layer 1 neurons
	for i in 39..55 {
		if used_idx.contains(&i) {
			output.push_str(&format!(
				"{} [label=<<FONT POINT-SIZE=\"16\">L1</FONT>\
				<SUB><FONT POINT-SIZE=\"11\">{}</FONT></SUB>>]\n", i, i - 39));
		}
	}

	// Change to diamond style
	output.push_str("node [shape=\"diamond\"]\n");

	// Declare all used layer 2 neurons
	for i in 55..63 {
		if used_idx.contains(&i) {
			output.push_str(&format!(
				"{} [label=<<FONT POINT-SIZE=\"16\">L2</FONT>\
				<SUB><FONT POINT-SIZE=\"11\">{}</FONT></SUB>>]\n", i, i - 55));
		}
	}

	// Change to tripleoctagon style and declare the output neuron
	if used_idx.contains(&63) {
		output.push_str("node [shape=\"tripleoctagon\"]\n63 [label=\"OUT\"]\n");
	}

	// Now we simply add all edges to the file.
	// If the weight is negative, it will be red colored; positive will be green
	// The width of the arrow is corrrelated to the magnitude of the weight
	for (a, b, w) in processed.into_iter() {
		let weight = raw_to_weight(w);
		let clamped_w = weight.abs() / 52.4288 * 9.0 + 1.0;
		if weight < 0.0 {
			output.push_str(&format!("{} -> {} [penwidth=\"{}\" color=\"red\"]\n",
				a, b, clamped_w));
		} else {
			output.push_str(&format!("{} -> {} [penwidth=\"{}\" color=\"green\"]\n",
				a, b, clamped_w));
		}
	}
	output.push_str("}\n");

	// Write everything to dot file
	let mut f = OpenOptions::new()
		.write(true)
		.create(true)
		.truncate(true)
		.open(format!("./nechedbg/{}.dot", filename))
		.unwrap();
	f.write_all(output.as_bytes()).expect("Failed to write");
}


#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn to_num_tests() {
		assert_eq!(to_num("A".as_bytes()), 0);
		assert_eq!(to_num("T".as_bytes()), 3);
		assert_eq!(to_num("C".as_bytes()), 1);
		assert_eq!(to_num("G".as_bytes()), 2);
		assert_eq!(to_num("AAAAGCC".as_bytes()), 37);
		assert_eq!(to_num("CGTC".as_bytes()), 109);
	}
}
