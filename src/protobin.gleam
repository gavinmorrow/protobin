import gleam/bit_array
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/list
import gleam/option
import gleam/result

import protobin/internal/util
import protobin/internal/wire_type.{type WireType}

pub type BytePos =
  Int

pub type ParseError {
  UnknownWireType(Int, pos: BytePos)
  InvalidVarInt(leftover_bits: BitArray, acc: BitArray, pos: BytePos)
  InvalidFixed(size: Int, bits: BitArray, pos: BytePos)
  InvalidLen(len: Int, value_bits: BitArray, pos: BytePos)
  UnableToDecode(List(decode.DecodeError))
}

pub type DecodeResult(t) =
  Result(t, ParseError)

pub type Parsed(t) {
  Parsed(value: t, rest: BitArray, pos: BytePos)
}

fn parsed_map(of parsed: Parsed(t), with fun: fn(t) -> u) -> Parsed(u) {
  let Parsed(value:, rest:, pos:) = parsed
  Parsed(value: fun(value), rest:, pos:)
}

type Field {
  Field(key: Dynamic, value: Dynamic)
}

pub type ValueResult =
  DecodeResult(Parsed(BitArray))

pub type ValueParser =
  fn(BitArray, BytePos) -> ValueResult

pub fn parse(
  from bits: BitArray,
  using decoder: Decoder(t),
) -> DecodeResult(Parsed(t)) {
  use Parsed(value: data, rest:, pos:) <- result.try(read_fields(bits, [], 0))
  decode.run(data, decoder)
  |> result.map(fn(value) { Parsed(value:, rest:, pos:) })
  |> result.map_error(UnableToDecode)
}

fn read_fields(
  bits: BitArray,
  acc: List(Field),
  pos: BytePos,
) -> DecodeResult(Parsed(Dynamic)) {
  case bits {
    <<>> ->
      acc
      |> repeated_to_list
      |> dict.to_list
      |> dynamic.properties
      |> Parsed(value: _, rest: bits, pos:)
      |> Ok
    bits -> {
      use Parsed(value: prop, rest: bits, pos:) <- result.try(read_field(
        bits,
        pos,
      ))
      read_fields(bits, [prop, ..acc], pos)
    }
  }
}

fn repeated_to_list(reversed_fields: List(Field)) -> dict.Dict(Dynamic, Dynamic) {
  let fields = {
    // Every field is a list of values for two reasons:
    // a) expanded repeated values are encoded as repeated fields
    // b) if a non-repeating field is defined twice, then the last value should
    //    be considered the correct one. since the parser doesn't know which
    //    fields are repeating, it parses all fields as a list and then the
    //    decoders will handle choosing which value(s) to keep.
    let acc: dict.Dict(Dynamic, List(Dynamic)) = dict.new()
    use fields, Field(key:, value:) <- list.fold(
      over: reversed_fields,
      from: acc,
    )

    use existing_values <- dict.upsert(in: fields, update: key)
    let existing_values = option.unwrap(existing_values, or: [])
    // The repeated values were previously reversed, now being reversed again
    [value, ..existing_values]
  }

  dict.map_values(in: fields, with: fn(_key, field) { field |> dynamic.list })
}

fn read_field(bits: BitArray, tag_pos: BytePos) -> DecodeResult(Parsed(Field)) {
  use Parsed(value: tag, rest: bits, pos:) <- result.try(parse_varint(
    bits,
    tag_pos,
  ))
  let tag = util.bit_array_to_uint(tag)

  let field_id = tag |> int.bitwise_shift_right(3)
  let wire_type = tag |> int.bitwise_and(0b111)

  use wire_type <- result.try(option.to_result(
    wire_type.parse(wire_type),
    UnknownWireType(wire_type, pos: tag_pos),
  ))

  let read = wire_type_read_fn(wire_type)
  use value: Parsed(Dynamic) <- result.try(
    read(bits, pos) |> result.map(parsed_map(_, dynamic.bit_array)),
  )

  let field =
    parsed_map(value, fn(value) { Field(key: dynamic.int(field_id), value:) })

  Ok(field)
}

fn wire_type_read_fn(ty: WireType) -> ValueParser {
  case ty {
    wire_type.VarInt -> read_varint
    wire_type.I64 -> read_fixed(64)
    wire_type.Len -> read_len
    wire_type.I32 -> read_fixed(32)
    wire_type.SGroup | wire_type.EGroup -> read_group
  }
}

/// Reads the bits from a varint and returns *all* of them. They cannot be
/// parsed as a uint.
///
/// Specifically, the continuation bits are included, so the value's bit size
/// will be a multiple of 8.
pub fn read_varint(bits: BitArray, pos: BytePos) -> ValueResult {
  read_varint_acc(bits, <<>>, pos)
}

fn read_varint_acc(bits: BitArray, acc: BitArray, pos: BytePos) -> ValueResult {
  case bits {
    <<0:size(1), n:bits-size(7), rest:bytes>> -> {
      let bit = <<0:size(1), n:bits>>
      let acc = bit_array.concat([acc, bit])
      Ok(Parsed(value: acc, rest:, pos: pos + 1))
    }
    <<1:size(1), n:bits-size(7), rest:bytes>> -> {
      let bit = <<1:size(1), n:bits>>
      read_varint_acc(rest, bit_array.concat([acc, bit]), pos + 1)
    }
    bits -> Error(InvalidVarInt(leftover_bits: bits, acc:, pos:))
  }
}

/// Reads the bits from a varint and parses them a `BitArray`. The returned bits
/// can be parsed as a uint.
///
/// For a decoder that does this, use `decoders.uint()`.
///
/// The continuation bits are not included, so the value's bit size will be a
/// multiple of 7.
pub fn parse_varint(bits: BitArray, pos: BytePos) -> ValueResult {
  parse_varint_acc(bits, <<>>, pos)
}

fn parse_varint_acc(bits: BitArray, acc: BitArray, pos: BytePos) -> ValueResult {
  case bits {
    <<0:size(1), n:bits-size(7), rest:bytes>> -> {
      let acc = bit_array.concat([n, acc])
      Ok(Parsed(value: acc, rest:, pos: pos + 1))
    }
    <<1:size(1), n:bits-size(7), rest:bytes>> ->
      parse_varint_acc(rest, bit_array.concat([n, acc]), pos + 1)
    bits -> {
      Error(InvalidVarInt(leftover_bits: bits, acc:, pos:))
    }
  }
}

/// Size is in bits, usually either 32 or 64.
///
/// Size must be a multiple of 8, or the returned position will be incorrect.
pub fn read_fixed(size: Int) -> ValueParser {
  fn(bits: BitArray, pos: BytePos) -> ValueResult {
    case bits {
      <<num:bits-size(size), rest:bytes>> ->
        Ok(Parsed(value: num, rest:, pos: pos + size / 8))
      bits -> Error(InvalidFixed(size:, bits:, pos:))
    }
  }
}

fn read_len(bits: BitArray, len_pos: BytePos) -> ValueResult {
  // First, read the length of the value
  // It is encoded as a varint immediately after the tag
  use Parsed(value: len, rest: bits, pos:) <- result.try(parse_varint(
    bits,
    len_pos,
  ))
  let len = util.bit_array_to_uint(len)

  use value <- result.try(
    bit_array.slice(from: bits, at: 0, take: len)
    |> result.map_error(fn(_) {
      InvalidLen(len:, value_bits: bits, pos: len_pos)
    }),
  )
  // Assert b/c if the len was too long, it would've errored in the prev slice
  let assert Ok(rest) =
    bit_array.slice(from: bits, at: len, take: bit_array.byte_size(bits) - len)

  Ok(Parsed(value:, rest:, pos: pos + len))
}

/// Currently just does nothing.
fn read_group(bits: BitArray, pos: BytePos) -> ValueResult {
  Ok(Parsed(value: <<>>, rest: bits, pos: pos))
}

/// Decode a repeated field that may be either packed or expanded.
/// 
/// If it is impossible for a field to be packed (ie because it is encoded as
/// a `LEN`) and therefore there is no `ValueParser` for it, then use the
/// stdlib's `decode.list()` instead.
pub fn decode_multiple(
  of decoder: Decoder(t),
  using parser: ValueParser,
) -> Decoder(List(t)) {
  use values <- decode.then(packed_values(of: decoder, using: parser))
  values |> decode.success
}

fn packed_values(
  of decoder: Decoder(t),
  using parser: ValueParser,
) -> Decoder(List(t)) {
  use bits <- decode.then(decode.list(of: decode.bit_array))
  let bits =
    list.fold(bits, <<>>, fn(acc, elem) { bit_array.concat([acc, elem]) })

  let values = {
    use values <- result.try(unpack_bits(bits, [], at: 0, using: parser))
    values
    |> list.map(dynamic.bit_array)
    |> list.try_map(fn(value) { decode.run(value, decoder) })
    |> result.map_error(UnableToDecode)
  }

  case values {
    Ok(values) -> decode.success(values)
    Error(_) -> decode.failure([], "Packed values")
  }
}

fn unpack_bits(
  bits: BitArray,
  acc: List(BitArray),
  at pos: BytePos,
  using parser: ValueParser,
) -> DecodeResult(List(BitArray)) {
  case bits {
    <<>> -> acc |> list.reverse |> Ok
    bits -> {
      use Parsed(value:, rest:, pos:) <- result.try(parser(bits, pos))
      unpack_bits(rest, [value, ..acc], at: pos, using: parser)
    }
  }
}

/// Takes the last value from the list.
fn single(
  of decoder: Decoder(t),
  named name: String,
  default default: t,
) -> Decoder(t) {
  use values <- decode.then(decode.list(of: decoder))

  case list.last(values) {
    Ok(value) -> decode.success(value)
    Error(Nil) -> decode.failure(default, name)
  }
}

// Allows the decoders to be used for either single or repeated fields
fn single_or_raw(
  of decoder: Decoder(t),
  named name: String,
  default default: t,
) -> Decoder(t) {
  decode.one_of(decoder, or: [
    single(of: decoder, named: name, default: default),
  ])
}

fn single_or_raw_bit_array() -> Decoder(BitArray) {
  single_or_raw(of: decode.bit_array, named: "BitArray", default: <<>>)
}

pub fn decode_protobuf(
  // Passed as a function so recursive decoders are easier
  using decoder: fn() -> Decoder(t),
  named name: String,
  default default: t,
) -> Decoder(t) {
  use bits <- decode.then(single_or_raw_bit_array())

  let value = parse(from: bits, using: decoder())
  case value {
    Ok(Parsed(value:, rest: <<>>, pos: _)) -> decode.success(value)
    _ -> decode.failure(default, name)
  }
}

pub fn decode_uint() -> Decoder(Int) {
  use bits <- decode.then(single_or_raw_bit_array())
  use bits <- decode.then(case parse_varint(bits, 0) {
    Ok(Parsed(value:, rest: <<>>, pos: _)) -> decode.success(value)
    _ -> decode.failure(<<>>, "uint")
  })
  bits |> util.bit_array_to_uint |> decode.success
}

pub fn decode_fixed(size: Int) -> Decoder(Int) {
  use bits <- decode.then(single_or_raw_bit_array())
  case bits {
    <<num:unsigned-little-size(size)>> -> decode.success(num)
    _ -> decode.failure(0, "Fixed(" <> int.to_string(size) <> ")")
  }
}

pub fn decode_bool() -> Decoder(Bool) {
  use bool <- decode.then(decode_uint())
  case bool {
    0 -> False |> decode.success
    1 -> True |> decode.success
    _ -> decode.failure(False, "Bool")
  }
}

pub fn decode_string() -> Decoder(String) {
  use bits <- decode.then(single_or_raw_bit_array())

  let str = bit_array.to_string(bits)
  case str {
    Ok(str) -> decode.success(str)
    Error(_) -> decode.failure("", "String")
  }
}
