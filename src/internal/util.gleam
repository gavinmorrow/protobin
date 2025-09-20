import gleam/bit_array

pub fn bit_array_to_uint(bits: BitArray) -> Int {
  let size = bit_array.bit_size(bits)
  // Interpert the entire bit array as an unsigned int
  let assert <<n:unsigned-big-size(size)>> = bits
  n
}
