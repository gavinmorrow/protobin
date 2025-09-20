import gleam/dynamic/decode.{type Decoder}
import gleam/option
import gleeunit
import simplifile as file

import decoders
import protobuf_decode_gleam.{parse}

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
    decoders.protobuf(
      using: person_decoder,
      named: "Person",
      default: default_person,
    )

  use id <- decode.field(3, decoders.fixed(64))
  use age <- decode.field(1, decoders.uint())
  use score <- decode.field(2, decoders.uint())
  use person <- decode.optional_field(
    4,
    option.None,
    decode.optional(person_inner_decoder),
  )
  use day <- decode.field(5, decoders.fixed(32))

  decode.success(Person(id:, age:, score:, self: person, day:))
}

pub fn person_pb_test() {
  let path = "./test/person.pb"
  let assert Ok(bits) = file.read_bits(from: path)
  let assert Ok(person) = parse(from: bits, using: person_decoder())
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
  use id <- decode.field(1, decoders.uint())
  use age <- decode.field(2, decoders.uint())

  decode.success(Test(id:, age:))
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

  let assert Ok(data) = parse(from: bits, using: two_ints_decoder())
  assert data == Test(id: 150, age: 80_150)
}

type Gtfs {
  Gtfs(header: FeedHeader)
}

fn gtfs_decoder() -> Decoder(Gtfs) {
  use header <- decode.field(
    1,
    decoders.protobuf(
      using: feed_header_decoder,
      named: "FeedHeader",
      default: feed_header_default,
    ),
  )
  decode.success(Gtfs(header:))
}

type FeedHeader {
  FeedHeader(version: String)
}

const feed_header_default = FeedHeader(version: "0.0")

fn feed_header_decoder() -> Decoder(FeedHeader) {
  use version <- decode.field(1, decode_string())
  decode.success(FeedHeader(version:))
}

pub fn gtfs_test() {
  let path = "./test/gtfs-short.pb"
  let assert Ok(bits) = file.read_bits(from: path)
  let assert Ok(gtfs) = parse(from: bits, using: gtfs_decoder())

  echo gtfs

  Nil
}
