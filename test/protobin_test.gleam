import gleam/dynamic/decode.{type Decoder}
import gleam/option
import gleeunit
import simplifile as file

import protobin.{Parsed, parse, read_fixed, read_varint}

pub fn main() -> Nil {
  gleeunit.main()
}

type Person {
  Person(id: Int, age: Int, score: Int, self: option.Option(Person), day: Int)
}

const default_person = Person(
  id: 0,
  age: 0,
  score: 0,
  self: option.None,
  day: 0,
)

fn person_decoder() -> Decoder(Person) {
  let person_inner_decoder =
    protobin.decode_protobuf(
      using: person_decoder,
      named: "Person",
      default: default_person,
    )

  use id <- decode.field(3, protobin.decode_fixed(64))
  use age <- decode.field(1, protobin.decode_uint())
  use score <- decode.field(2, protobin.decode_uint())
  use person <- decode.optional_field(
    4,
    option.None,
    decode.optional(person_inner_decoder),
  )
  use day <- decode.field(5, protobin.decode_fixed(32))

  Person(id:, age:, score:, self: person, day:) |> decode.success
}

pub fn person_pb_test() {
  let path = "./test/person.pb"

  let assert Ok(bits) = file.read_bits(from: path)
  let assert Ok(Parsed(value: person, rest: <<>>, pos: _)) =
    parse(from: bits, using: person_decoder())

  assert person
    == Person(
      id: 42,
      age: 150,
      score: 81_050,
      self: option.Some(Person(
        id: 42,
        age: 150,
        score: 81_050,
        self: option.None,
        day: 22,
      )),
      day: 22,
    )
}

type TwoInts {
  Test(id: Int, age: Int)
}

fn two_ints_decoder() -> Decoder(TwoInts) {
  use id <- decode.field(1, protobin.decode_uint())
  use age <- decode.field(2, protobin.decode_uint())

  Test(id:, age:) |> decode.success
}

pub fn two_ints_test() {
  let bits = <<
    // field 1
    0x08,
    0x96,
    0x01,
    // field 2
    0x10,
    0x96,
    0xf2,
    0x04,
  >>

  let assert Ok(Parsed(value: data, rest: <<>>, pos: 7)) =
    parse(from: bits, using: two_ints_decoder())
  assert data == Test(id: 150, age: 80_150)
}

pub type FeedHeader {
  FeedHeader(gtfs_realtime_version: String, timestamp: Int, nyct: NyctHeader)
}

pub type NyctHeader {
  NyctHeader(
    version: String,
    trip_replacement_periods: List(TripReplacementPeriod),
  )
}

pub type TripReplacementPeriod {
  TripReplacementPeriod(route_id: String, replacement_period: Int)
}

const nyct_header_default = NyctHeader(
  version: "1.0",
  trip_replacement_periods: [],
)

const trip_replacement_period_default = TripReplacementPeriod(
  route_id: "",
  replacement_period: 0,
)

fn feed_header_decoder() -> Decoder(FeedHeader) {
  use gtfs_realtime_version <- decode.field(1, protobin.decode_string())
  use timestamp <- decode.field(3, protobin.decode_fixed(64))
  use nyct <- decode.field(
    1001,
    protobin.decode_protobuf(
      using: nyct_header_decoder,
      named: "NyctHeader",
      default: nyct_header_default,
    ),
  )
  FeedHeader(gtfs_realtime_version:, timestamp:, nyct:) |> decode.success
}

fn nyct_header_decoder() -> Decoder(NyctHeader) {
  use version <- decode.field(1, protobin.decode_string())

  use trip_replacement_periods <- decode.field(
    2,
    decode.list(of: protobin.decode_protobuf(
      using: trip_replacement_period_decoder,
      named: "TripReplacementPeriod",
      default: trip_replacement_period_default,
    )),
  )

  NyctHeader(version:, trip_replacement_periods:) |> decode.success
}

fn trip_replacement_period_decoder() -> Decoder(TripReplacementPeriod) {
  use route_id <- decode.field(1, protobin.decode_string())
  use replacement_period <- decode.field(
    2,
    protobin.decode_protobuf(
      using: trip_replacement_period_time_range_decoder,
      named: "TripReplacePeriod.TimeRange",
      default: 0,
    ),
  )
  TripReplacementPeriod(route_id:, replacement_period:) |> decode.success
}

fn trip_replacement_period_time_range_decoder() -> Decoder(Int) {
  use time <- decode.field(2, protobin.decode_fixed(64))
  time |> decode.success
}

pub fn gtfs_test() {
  let path = "./test/gtfs-short.pb"
  let assert Ok(bits) = file.read_bits(from: path)
  let assert Ok(header) = parse(from: bits, using: feed_header_decoder())

  assert header
    == Parsed(
      FeedHeader(
        "1.0",
        1_758_224_061,
        NyctHeader("1.0", [
          TripReplacementPeriod("1", 1_758_224_061),
          TripReplacementPeriod("2", 1_758_224_061),
          TripReplacementPeriod("5", 1_758_224_061),
          TripReplacementPeriod("6", 1_758_224_061),
          TripReplacementPeriod("7", 1_758_224_061),
        ]),
      ),
      <<>>,
      102,
    )
}

type PackedFields {
  PackedFields(
    packed: List(Int),
    expanded: List(Int),
    mixed_packed_and_expanded: List(Int),
  )
}

fn packed_fields_decoder() -> Decoder(PackedFields) {
  use packed <- decode.field(
    3,
    protobin.decode_multiple(of: protobin.decode_uint(), using: read_varint),
  )
  use expanded <- decode.field(
    4,
    protobin.decode_multiple(of: protobin.decode_uint(), using: read_varint),
  )
  use mixed_packed_and_expanded <- decode.field(
    11,
    protobin.decode_multiple(
      of: protobin.decode_fixed(32),
      using: read_fixed(32),
    ),
  )
  PackedFields(packed:, expanded:, mixed_packed_and_expanded:) |> decode.success
}

pub fn packed_fields_test() {
  let path = "./test/packed-fields.pb"
  let assert Ok(bits) = file.read_bits(from: path)
  let assert Ok(packed_fields) =
    parse(from: bits, using: packed_fields_decoder())

  assert packed_fields
    == Parsed(
      value: PackedFields(
        packed: [0, 1, 2, 3],
        expanded: [4, 5, 6],
        mixed_packed_and_expanded: [256, 22, 42, 101, 4_294_967_295, 0, 1],
      ),
      rest: <<>>,
      pos: 48,
    )
}
