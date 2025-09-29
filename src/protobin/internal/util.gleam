import gleam/bit_array

pub fn bit_array_to_uint(bits: BitArray) -> Int {
  let size = bit_array.bit_size(bits)
  // Interpert the entire bit array as an unsigned int
  // It is impossible for this to crash, it interperts the entire bit array
  let assert <<n:unsigned-big-size(size)>> = bits
  n
}
