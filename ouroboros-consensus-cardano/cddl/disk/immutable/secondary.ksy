meta:
  id: secondary_file
  endian: be

types:
  entry:
    seq:
      - id: block_offset
        type: u8

      - id: header_offset
        type: u2

      - id: header_size
        type: u2

      - id: crc
        type: u4

      - id: header_hash
        size: 32

      - id: block_or_ebb
        type: u8

seq:
  - id: secondary_offsets
    type: entry
    repeat: eos
