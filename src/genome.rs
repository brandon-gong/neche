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

/// Parse a genome into a vector of tuples (a, b, w), where a is the
/// ID of the neuron the connection is going outward from, b is the neuron
/// that is receiving the connection, and w is the weight of the connection.
pub fn parse_genome(genome: &String) -> Vec<(u32, u32, u32)> {
	let gene_bytes = genome.as_bytes();
	if gene_bytes.len() % 16 != 0 {
		panic!("Invalid gene: bp not right length");
	}
	let mut s = 0;
	let mut info = Vec::new();
	while s < gene_bytes.len() {
		info.push( (to_num(&gene_bytes[s..s+3]), to_num(&gene_bytes[s+3..s+6]),
			to_num(&gene_bytes[s+6..s+16])) );
		s += 16;
	}
	info.into_iter().filter(|&(a, b, _)| {
		if a < 39 {
			b >= 39 && b <= 63
		} else if a < 55 {
			b >= 55 && b <= 63
		} else if a < 63 {
			b == 63
		} else {
			false
		}
	}).collect()
}

/// Determines if a given genome is "viable", i.e. if the genome has any neurons
/// connecting to OUT.
fn is_viable(genome: &String) -> bool {
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
	loop {
  	let mut gene = String::with_capacity((num_genes * 16).try_into().unwrap());
		for _ in 0..(16 * num_genes) {
			gene.push(match rng.gen_range(0..4) {
				0 => 'A',
				1 => 'C',
				2 => 'G',
				3 => 'T',
				_ => panic!("rng broke")
    	});
  	}
		if is_viable(&gene) { return gene; }
	}
}

/// Converts a segment of a genome from its base-4 ATCG reprecentation to
/// a number.
fn to_num(segment: &[u8]) -> u32 {
	let mut res = 0;
	for c in segment.iter() {
		res <<= 2;
		res |= match c {
			65 => 0b00,
			67 => 0b01,
			71 => 0b10,
			84 => 0b11,
			_ => panic!("Invalid genome in to_num")
		};
	}
	res
}

/// Induce point mutations in the genome (single bp changes).
fn point_mutate(genome: &String, mutation_rate: f64) -> String {
  let mut rng = rand::thread_rng();
	let mut new = String::with_capacity(genome.len());
	for c in genome.chars() {
		if rng.gen::<f64>() < mutation_rate {
	    new.push(match rng.gen_range(0..4) {
	      0 => 'A',
	      1 => 'C',
	      2 => 'G',
	      3 => 'T',
	    	 _ => panic!("rng broke")
    	});
		} else {
			new.push(c);
		}
	}
	new
}

/// Reshuffle a chunk of genes in the genome.
fn translocation_mutate(genome: &String, mutation_rate: f64) -> String {
	let mut rng = rand::thread_rng();
	if rng.gen::<f64>() < mutation_rate {
		// a quick and dirty hack to force mutation to occur
		let mut newgene = genome.to_string();
		while &newgene == genome {

		let genome_size = genome.len() / 16;
		// Cut along gene lines
		let excision_start = rng.gen_range(0..genome_size);
		let excision_end = rng.gen_range(excision_start..(genome_size + 1));
		let excision =  genome
			.chars()
			.skip(excision_start * 16)
			.take(16 * (excision_end - excision_start));
		let remainder: String = genome
			.chars()
			.take(excision_start * 16)
			.chain(genome.chars().skip(excision_end * 16))
			.collect();
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
	let p1_genes: Vec<String> = p1.chars()
  	.collect::<Vec<char>>()
    .chunks(16)
    .map(|x| x.iter().collect::<String>()).collect();
	let p2_genes: Vec<String> = p2.chars()
  	.collect::<Vec<char>>()
    .chunks(16)
    .map(|x| x.iter().collect::<String>()).collect();
	
	loop {
		let mut child_genes = Vec::new();
		for (g1, g2) in p1_genes.iter().zip(p2_genes.iter()) {
			if rng.gen::<f64>() < 0.5 {
				child_genes.push(g1.clone());
			} else {
				child_genes.push(g2.clone());
			}
		}
		let res = translocation_mutate(
			&point_mutate(&child_genes.join(""), point_mutation_rate),
			translocation_mutation_rate);
		if is_viable(&res) { return res; }
	}
}

/// Writes a genome out to a .dot file that can be passed through
/// GraphViz to produce a visualization of the network structure.
pub fn write_to_dot(genome: &String, filename: &String) -> () {
	let processed: Vec<(u32, u32, u32)> = parse_genome(genome);
	let mut used_idx = Vec::new();
	for r in processed.iter() {
		if !used_idx.contains(&r.0) {
			used_idx.push(r.0);
		}
		if !used_idx.contains(&r.1) {
			used_idx.push(r.1);
		}
	}
	let mut output = String::from("digraph network {\nrankdir=LR;\n\
		overlap=\"prism\";\nsep=\"+15\";\nnode [shape=\"circle\"]\n");
	let ppsquare = vec!["A1", "B2", "C3", "D4", "E5", "F6", "G7", "H8", "G1",
		"H2", "A3", "B4", "C5", "D6", "E7", "F8", "E1", "F2", "G3", "H4", "A5",
		"B6", "C7", "D8", "C1", "D2", "E3", "F4", "G5", "H6", "A7", "B8"];
	for i in 0..32 {
		if used_idx.contains(&i) {
			output.push_str(&format!("{} [label=\"{}\"]\n", i, ppsquare[i as usize]));
		}
	}
	output.push_str("node [shape=\"doublecircle\"]\n");
	for i in 32..39 {
		if used_idx.contains(&i) {
			output.push_str(&format!(
				"{} [label=<<FONT POINT-SIZE=\"16\">b</FONT>\
				<SUB><FONT POINT-SIZE=\"11\">{}</FONT></SUB>>]\n", i, i - 32));
		}
	}
	output.push_str("node [shape=\"square\"]\n");
	for i in 39..55 {
		if used_idx.contains(&i) {
			output.push_str(&format!(
				"{} [label=<<FONT POINT-SIZE=\"16\">L1</FONT>\
				<SUB><FONT POINT-SIZE=\"11\">{}</FONT></SUB>>]\n", i, i - 39));
		}
	}
	output.push_str("node [shape=\"diamond\"]\n");
	for i in 55..63 {
		if used_idx.contains(&i) {
			output.push_str(&format!(
				"{} [label=<<FONT POINT-SIZE=\"16\">L2</FONT>\
				<SUB><FONT POINT-SIZE=\"11\">{}</FONT></SUB>>]\n", i, i - 55));
		}
	}
	if used_idx.contains(&63) {
		output.push_str("node [shape=\"tripleoctagon\"]\n63 [label=\"OUT\"]\n");
	}

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
