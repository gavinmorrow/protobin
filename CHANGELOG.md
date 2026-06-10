# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

- Decoder for sints (`decode_sint()`). Thanks to [@eaon] in [#2]!

[@eaon]: https://github.com/eaon
[#2]: https://github.com/gavinmorrow/protobin/pull/2

## v2.1.0 - 2026-02-27

### Added

- Decoder for bytes (`decode_bytes()`). Thanks to [@eaon] in [#1]!

[@eaon]: https://github.com/eaon
[#1]: https://github.com/gavinmorrow/protobin/pull/1

## v2.0.0 - 2025-10-01

### Added

- Make parsing configurable, with `parse_with_config()` and `Config`.
  - Allow ignoring groups instead of always failing.

### Changed

- Groups now result in an `UnexpectedGroup` error instead of `UnknownWireType`.

## v1.1.0 - 2025-10-01

### Added

- Decoder for bools (`decode_bool()`)

## v1.0.0 - 2025-09-29

- Initial version.
